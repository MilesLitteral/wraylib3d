{-# LANGUAGE RecordWildCards, FlexibleInstances #-}
module HRayLib3d.GameEngine.Graphics.OBJ
  ( addOBJ
  , addGPUOBJ
  , setOBJFrame
  , uploadOBJ
  , GPUOBJ(..)
  , OBJInstance(..)
  ) where


import Data.Map             (Map)
import Data.Vector          (Vector)
import Data.HashSet         (HashSet)
import Data.Vector.Generic  (convert)
import Control.Monad        ( forM )
import qualified Data.Map        as Map
import qualified Data.Map.Strict as M
import qualified Data.HashSet    as HashSet
import qualified Data.Vector     as V
import qualified Data.Vector.Storable  as SV
import qualified Data.ByteString.Char8 as SB8

import Foreign      ( castPtr )
import Codec.Wavefront.TexCoord ( TexCoord(..) )

import LambdaCube.GL
    ( V4(V4),
      V2(V2),
      GLStorage, --TODO Create a typeclass for converting between GLStorage and other storage types
      Primitive(TriangleList),
      IndexStream(IndexStream),
      Buffer,
      Stream(ConstV2F, Stream, ConstV4F),
      Object,
      compileBuffer,
      updateBuffer,
      addObject,
      Array(..),
      ArrayType(ArrFloat, ArrWord32),
      StreamType(Attribute_V3F, Attribute_V2F), V3 (V3) )
import LambdaCube.GL.Mesh ( addMeshToObjectArray, uploadMeshToGPU )
import LambdaCube.GL.Type
    ( Array(..),
      ArrayType(ArrFloat, ArrWord32),
      Buffer,
      GLStorage,
      IndexStream(IndexStream),
      Object(..),
      Primitive(TriangleList),
      Stream(ConstV2F, Stream, ConstV4F) )
      
import Data.Vect.Float.Base    ( Vec3(..)     )
import qualified Data.Text as T
import qualified Data.Foldable

import Codec.Wavefront         ( Location(..) )
import Codec.Wavefront.Face    ( Face(..)     )
import Codec.Wavefront.Element ( Element(..)  )
import HRayLib3d.GameEngine.Data.OBJ         ( WavefrontOBJ(..) )
import HRayLib3d.GameEngine.Graphics.Storage ( addObjectWithMaterial )
import HRayLib3d.GameEngine.Utils            ( bbox, setNub, sphere  )

data OBJInstance
  = OBJInstance
  { objinstanceObject :: [Object]
  , objinstanceBuffer :: Buffer
  , objinstanceFrames :: Vector [(Int,Array)]
  , objinstanceModel  :: WavefrontOBJ
  }

data GPUOBJ
  = GPUOBJ
  { gpuobjBuffer    :: Buffer
  , gpuobjSurfaces  :: [(IndexStream Buffer,Map String (Stream Buffer))] -- index stream, attribute streams
  , gpuobjFrames    :: Vector [(Int,Array)]
  , gpuobjModel     :: WavefrontOBJ
  , gpuobjShaders   :: HashSet String
  }

instance SV.Storable TexCoord
instance SV.Storable (Element Face)

setOBJFrame :: OBJInstance -> Int -> IO ()
setOBJFrame (OBJInstance{..}) idx = Data.Foldable.forM_ (objinstanceFrames V.!? idx) (updateBuffer objinstanceBuffer)

texCoordToArray :: [TexCoord] -> [Array]
texCoordToArray   = undefined

texCoordVToTupleV :: [Vector TexCoord] -> [Vector (b, b)]
texCoordVToTupleV = undefined

uploadOBJ :: WavefrontOBJ -> IO GPUOBJ
uploadOBJ model = do
  let cvtSurface WavefrontOBJ{..} =
        ( Array ArrWord32 (V.length objFaces)         (withV $ convert objFaces)
        , Array ArrFloat  (2 * V.length objTexCoords) (withV $ convert objTexCoords)
        , objTexCoords --convert $ 
        )
        where
          withV a f = SV.unsafeWith a (\p -> f $ castPtr p)
          createV3 tex = V3 (texcoordR tex) (texcoordS tex) (texcoordT tex)
          cvtPosNorm (p, n) = (f p, f n) where f sv = Array ArrFloat (3 * SV.length sv) $ withV sv

      addSurface sf (il, tl, pl, nl, pnl) = (i:il,t:tl,p:pl,p:nl,pn:pnl) where
        (i,t,pn) = cvtSurface sf
        p        = V.head pn
      (il, tl, pl, nl, pnl) = addSurface model ([],[],[],[],[])

  buffer <- compileBuffer (concat [il, tl, texCoordToArray pl, texCoordToArray nl])

  let numSurfaces = V.length $ objFaces model
      surfaceData idx WavefrontOBJ{..} = (index, attributes) where
        index  = IndexStream buffer idx 0 (V.length objFaces)
        countV = V.length objTexCoords
        attributes = Map.fromList $
          [ ("diffuseUV",   Stream Attribute_V2F buffer (1 * numSurfaces + idx) 0 countV)
          , ("position",    Stream Attribute_V3F buffer (2 * numSurfaces + idx) 0 countV)
          , ("normal",      Stream Attribute_V3F buffer (3 * numSurfaces + idx) 0 countV)
          , ("color",       ConstV4F (V4 1 1 1 1))
          , ("lightmapUV",  ConstV2F (V2 0 0))
          ]
      frames = foldr addSurfaceFrames emptyFrame $ zip [0..] (texCoordVToTupleV pnl) where
        emptyFrame = V.replicate 1 []
        addSurfaceFrames (idx,pn) f = V.zipWith (\l (p,n) -> (2 * numSurfaces + idx,p):(3 * numSurfaces + idx,n):l) f pn

  return $ GPUOBJ
    { gpuobjBuffer    = buffer
    , gpuobjSurfaces  = zipWith surfaceData [0..] [model] --it takes one model, perhaps this could be expanded to automate loading of OBJs
    , gpuobjFrames    = frames
    , gpuobjModel     = model
    , gpuobjShaders   = HashSet.fromList $ concat [map T.unpack $ V.toList (objMtlLibs model) | faces <- V.toList $ objFaces model]
    }

addGPUOBJ :: GLStorage -> GPUOBJ -> T.Text -> [T.Text] -> IO OBJInstance
addGPUOBJ r GPUOBJ{..} skin unis = do
  let WavefrontOBJ{..} = gpuobjModel
      materialMap      = V.toList objMtlLibs
  objs <- forM (zip gpuobjSurfaces $ V.toList objFaces) $ \((index,attrs),sf) -> do
    let materialName s = if skin `elem` materialMap  then skin else s
    objList <- concat <$> forM (V.toList objMtlLibs) (\s -> do
      a <- addObjectWithMaterial r (T.unpack $ materialName s) TriangleList (Just index) attrs $ setNub $ "worldMat": map T.unpack unis
      b <- addObject r "LightMapOnly" TriangleList (Just index) attrs $ setNub $ "worldMat": map T.unpack unis
      return [a,b])

    let objX  = Vec3   (locX (V.head objLocations)) 0 0
        objY  = Vec3 0 (locY (V.head objLocations)) 0

    -- add collision geometry
    collisionObjs <- case objList of
      (Object{..}:_) -> do
        sphereObj <- uploadMeshToGPU (sphere (V4 1 0 0 1) 4  (locW (V.head objLocations))) >>= addMeshToObjectArray r "CollisionShape" (setNub $ ["worldMat","origin"] ++ map T.unpack unis)
        boxObj    <- uploadMeshToGPU (bbox (V4 0 0 1 1) objX objY) >>= addMeshToObjectArray r "CollisionShape" (setNub $ ["worldMat","origin"] ++ map T.unpack unis)
        return [sphereObj,boxObj]
      _ -> return []

    return $ objList ++ collisionObjs
  -- question: how will be the referred shaders loaded?
  --           general problem: should the gfx network contain all passes (every possible materials)?
  return $ OBJInstance
    { objinstanceObject = concat objs
    , objinstanceBuffer = gpuobjBuffer
    , objinstanceFrames = gpuobjFrames
    , objinstanceModel  = gpuobjModel
    }

addOBJ :: GLStorage -> WavefrontOBJ -> T.Text -> [T.Text] -> IO OBJInstance
addOBJ r model skin unis = do
  gpuOBJ <- uploadOBJ model
  addGPUOBJ r gpuOBJ skin unis
