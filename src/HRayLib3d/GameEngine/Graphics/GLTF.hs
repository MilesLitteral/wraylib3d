{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module HRayLib3d.GameEngine.Graphics.GLTF
  ( addGlTF
  , addGPUGlTF
  , setGlTFFrame
  , uploadGlTF
  , GPUGlTF(..)
  , GlTFInstance(..)
  ) where

import Foreign             ( Ptr, Storable, castPtr )
import Data.Map            ( Map )
import Data.Aeson          ( (.:), withObject, eitherDecodeFileStrict, FromJSON, decode, Value )
import Data.Maybe          ( fromJust, fromMaybe )
import Data.Vector         ( Vector   )
import Data.HashSet        ( HashSet  )
import Data.Text.Unsafe    ( inlinePerformIO )
import qualified Data.Text              as T
import qualified Data.Map               as Map
import qualified Data.HashSet           as HashSet
import qualified Data.Vector            as V
import qualified Data.Vector.Storable   as SV
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.UTF8   as BSU      -- from utf8-string
import qualified Data.ByteString.Char8  as SB8
import qualified Data.ByteString.Base64 as B64
import qualified Data.Foldable

import Codec.GlTF            ( GlTF(..) )
import Codec.GlTF.Accessor   ( Accessor(..), ComponentType(..), AttributeType(..) )
import Codec.GlTF.Animation  ()
import Codec.GlTF.Buffer     ( Buffer(..),   unBufferIx )
import Codec.GlTF.BufferView ( BufferView(..) )
import Codec.GlTF.Texture    ()
import Codec.GlTF.URI        ( loadURI, URI   )

import LambdaCube.GL                         ( ArrayType (..), Buffer, Stream(..), Object(..), IndexStream(..),  updateBuffer, Array(..), GLStorage(..), ArrayType (ArrFloat), compileBuffer, addObject, Primitive (TriangleList), V4 (..), V2 (..) )
import LambdaCube.GL.Mesh                    ( addMeshToObjectArray, uploadMeshToGPU )
import HRayLib3d.GameEngine.Data.GLTF        ( GLTFModel(..) )
import HRayLib3d.GameEngine.Graphics.Storage ( addObjectWithMaterial )
import HRayLib3d.GameEngine.Utils            ( bbox, setNub, sphere )
import LambdaCube.PipelineSchema             ( StreamType(..) )
import qualified Codec.GlTF.Material as Material
import Data.Vect.Float.Base (Vec3(..))
import Data.Aeson.Types (FromJSON(parseJSON))

data GlTFInstance
  = GlTFInstance
  { gltfInstanceObject :: [Object]
  , gltfInstanceBuffer :: LambdaCube.GL.Buffer
  , gltfInstanceFrames :: Vector [(Int, Array)]
  , gltfInstanceModel  :: GLTFModel
  }

data GPUGlTF
  = GPUGlTF
  { gpuGlTFBuffer    :: LambdaCube.GL.Buffer
  , gpuGlTFSurfaces  :: [(IndexStream LambdaCube.GL.Buffer, Map String (Stream LambdaCube.GL.Buffer))] -- index stream, attribute streams
  , gpuGlTFFrames    :: Vector [(Int,Array)]
  , gpuGlTFModel     :: GLTFModel
  , gpuGlTFShaders   :: HashSet String
  }

setGlTFFrame :: GlTFInstance -> Int -> IO ()
setGlTFFrame (GlTFInstance{..}) idx = Data.Foldable.forM_ (gltfInstanceFrames V.!? idx) (updateBuffer gltfInstanceBuffer)

decodeBase64 ::   BSU.ByteString -> Maybe BS.ByteString
decodeBase64 bs = case B64.decode bs of
                    Left _     -> error "Bad B64 String"
                    Right dt   -> Just dt
                    --B64.decode bs >>= return . SB8.dropWhile (== '\NUL')

procRawBytes :: Codec.GlTF.URI.URI -> IO T.Text
procRawBytes uriBytes = do return $ T.pack . show $ uriBytes

instance (Num ComponentType)

-- Type GlTF.BufferView was BL.ByteString before 
extractData :: Codec.GlTF.Buffer.Buffer ->  Codec.GlTF.Accessor.Accessor -> Codec.GlTF.BufferView.BufferView -> Maybe [Float]
extractData dataBuffer accessor dataBufferView = do
  let uriBytes       = (fromJust . uri) dataBuffer
      bcount         = Codec.GlTF.BufferView.byteLength dataBufferView
      bOffset        = Codec.GlTF.BufferView.byteOffset dataBufferView
      byteStride     = Codec.GlTF.BufferView.byteStride dataBufferView
      compType       = componentType accessor
      dataType = case unAttributeType $ type' accessor of
        "SCALAR" -> 1
        "VEC2"   -> 2
        "VEC3"   -> 3
        "VEC4"   -> 4
        "MAT2"   -> 5
        "MAT3"   -> 6
        "MAT4"   -> 7
        _ -> error "Unsupported accessor type"
      bytes      = T.drop bOffset (inlinePerformIO $ procRawBytes uriBytes)
      dataChunks = case byteStride of
        Nothing     -> map T.unpack $ T.chunksOf (dataType * bcount) bytes
        Just stride -> map T.unpack $ T.chunksOf stride bytes
  case compType of
    5120 -> Just (map fromIntegral (concatMap readInt8     dataChunks))
    5121 -> Just (map fromIntegral (concatMap readUInt8    dataChunks))
    5122 -> Just (map fromIntegral (concatMap readInt16LE  dataChunks))
    5123 -> Just (map fromIntegral (concatMap readUInt16LE dataChunks))
    5124 -> Just (concatMap readInt32LE  dataChunks)
    5125 -> Just (concatMap readUInt32LE dataChunks)
    5126 -> Just (concatMap readFloatLE  dataChunks)
    _ -> Nothing
  where
    readInt8  str    = map (fromIntegral . read  . T.unpack) $ T.chunksOf 1 (T.pack str)
    readUInt8 str    = map (fromIntegral . read  . T.unpack) $ T.chunksOf 1 (T.pack str)
    readInt16LE str  = map (fromIntegral . read  . T.unpack) $ T.chunksOf 2 (T.pack str)
    readUInt16LE str = map (fromIntegral . read  . T.unpack) $ T.chunksOf 2 (T.pack str)
    readInt32LE str  = map (fromIntegral . read  . T.unpack) $ T.chunksOf 4 (T.pack str)
    readUInt32LE str = map (fromIntegral . read  . T.unpack) $ T.chunksOf 4 (T.pack str)
    readFloatLE str  = map (fromIntegral . read  . T.unpack) $ T.chunksOf 4 (T.pack str)

cvtGlTF :: Codec.GlTF.GlTF ->  (Array, Array, Vector (Array,Array))
cvtGlTF    Codec.GlTF.GlTF{..} = do
  -- Extract ByteString from Base64 URI
  let gBuffers        = fromJust buffers
      gAccessors      = fromJust accessors
      bufferViewIxs   = fromJust bufferViews
      dataMaybe       = show $ fromJust . uri $ gBuffers V.! 0

      --bufferViewIndex = bufferViewIx -- Choose the desired buffer view index
      -- encodedData = C8.dropWhile (/= ',') (C8.pack $ URI $ fromJust uri) (C8.unpack encodedData) 
  if "data:application/octet-stream;base64," `SB8.isPrefixOf` SB8.pack dataMaybe
    then processGLTF gBuffers gAccessors bufferViewIxs
    else error "Bad Data URI"

processGLTF :: Vector Codec.GlTF.Buffer.Buffer -> Vector Accessor -> Vector BufferView -> (Array, Array, Vector (Array, Array))
processGLTF gBuffers gAccessors bufferViewIxs = do
  let indices          = fromJust $ extractData (gBuffers   V.! 4) (gAccessors V.! 4) (bufferViewIxs V.! 4)
      normals          = fromJust $ extractData (gBuffers   V.! 1) (gAccessors V.! 1) (bufferViewIxs V.! 1)
      texCoords        = fromJust $ extractData (gBuffers   V.! 3) (gAccessors V.! 3) (bufferViewIxs V.! 3)
      ar1              = Array ArrFloat   (SV.length $ SV.fromList indices)       (withV $ SV.fromList indices)
      ar2              = Array ArrFloat   (2 * SV.length (SV.fromList texCoords)) (withV $ SV.fromList texCoords)
      vb               = V.map cvtPosNorm (V.fromList [(SV.fromList   texCoords,     SV.fromList normals)])
  (ar1, ar2, vb)

withV :: Storable a => SV.Vector a -> (Ptr b1 -> IO b2) -> IO b2
withV a f        = SV.unsafeWith a (\p -> f $ castPtr p)

cvtPosNorm :: Storable a =>  (SV.Vector a, SV.Vector a) -> (Array, Array) -- (Int -> BufferSetter -> Array, Int -> BufferSetter -> Array)
cvtPosNorm (p,n) = (f p, f n) where f sv = Array ArrFloat (3 * SV.length sv) $ withV sv

addSurface :: GlTF -> ([Array], [Array], [Array], [Array], [Vector (Array, Array)]) -> ([Array], [Array], [Array], [Array], [Vector (Array, Array)])
addSurface sf (il,tl,pl,nl,pnl) = do
  (i:il,t:tl,p:pl,n:nl,pn:pnl)
  where
    (i, t, pn) = cvtGlTF sf
    (p, n)     = V.head pn

uploadGlTF :: GLTFModel -> IO GPUGlTF
uploadGlTF model = do
  let (il, tl, pl, nl, pnl) = addSurface (gltfRaw model) ([],[],[],[],[])
  buffer <- compileBuffer (concat [il, tl, pl, nl])
  let numSurfaces = length il -- / 4) -- TODO: a Let to properly support GlTF animations
      surfaceData idx GLTFModel{..} = (index, attributes) where
        index  = IndexStream buffer idx 0 (length tl)
        countV = length pl
        attributes = Map.fromList
          [ ("diffuseUV",   Stream Attribute_V2F buffer (1 * numSurfaces + idx) 0 countV)
          , ("position",    Stream Attribute_V3F buffer (2 * numSurfaces + idx) 0 countV)
          , ("normal",      Stream Attribute_V3F buffer (3 * numSurfaces + idx) 0 countV)
          , ("color",       ConstV4F (V4 1 1 1 1))
          , ("lightmapUV",  ConstV2F (V2 0 0))
          ]
      frames = Prelude.foldr addSurfaceFrames emptyFrame $ zip [0..] pnl where
        emptyFrame = V.replicate 0 [] --gltfFrames
        addSurfaceFrames (idx,pn) f = V.zipWith (\l (p,n) -> (2 * numSurfaces + idx,p):(3 * numSurfaces + idx,n):l) f pn
  return $ GPUGlTF
    { gpuGlTFBuffer    = buffer
    , gpuGlTFSurfaces  = zipWith surfaceData [0..] [model] --(V.fromList gltfSurfaces)
    , gpuGlTFFrames    = frames
    , gpuGlTFModel     = model
    , gpuGlTFShaders   = HashSet.fromList $ concat [[] | GLTFModel{..} <- [model]] -- [map (T.unpack . fromJust . Material.name) $ listOfMat | GLTFModel{..} <- V.toList gltfRaw]]
    } --V.toList gpuGlTFShaders

addGPUGlTF :: GLStorage -> GPUGlTF -> [String] -> IO GlTFInstance
addGPUGlTF r GPUGlTF{..} unis = do
  let GlTF{..} = gltfRaw gpuGlTFModel
  objs <- V.forM (V.fromList gpuGlTFSurfaces) $ \(index, _) -> do
    objList       <- concat <$> V.forM (V.fromList [fromMaybe Codec.GlTF.extensions]) (\_ -> do
      a <- addObjectWithMaterial r (T.unpack . fromJust $ Material.name (fromJust materials V.! 1)) TriangleList (Just index) Map.empty $ setNub $ "worldMat":unis
      b <- addObject r "LightMapOnly" TriangleList (Just index) Map.empty $ setNub $ "worldMat":unis
      return [a,b]) --attrs

    -- add collision geometry
    collisionObjs <-  do --TODO: re-add radius
                        sphereObj <- uploadMeshToGPU (sphere (V4 1 0 0 1) 4  1.0) >>= addMeshToObjectArray r "CollisionShape" (setNub $ ["worldMat","origin"] ++ unis)
                        boxObj    <- uploadMeshToGPU (bbox   (V4 0 0 1 1) (Vec3 0 0 0) {- Codec.GlTF.Accessor.min -} (Vec3 0 0 0) {- Codec.GlTF.Accessor.max -})     >>= addMeshToObjectArray r "CollisionShape" (setNub $ ["worldMat","origin"] ++ unis)
                        --when (frOrigin /= zero) $ putStrLn $ "frOrigin: " ++ show frOrigin
                        return [sphereObj,boxObj]
    return $ objList ++ collisionObjs
  -- question: how will be the referred shaders loaded?
  --           general problem: should the gfx network contain all passes (every possible materials)?
  return $ GlTFInstance
    { gltfInstanceObject = concat objs
    , gltfInstanceBuffer = gpuGlTFBuffer
    , gltfInstanceFrames = gpuGlTFFrames
    , gltfInstanceModel  = gpuGlTFModel
    }

addGlTF :: GLStorage -> GLTFModel -> [String] -> IO GlTFInstance
addGlTF r model unis = do
  gpuGlTF <- uploadGlTF model
  addGPUGlTF r gpuGlTF unis