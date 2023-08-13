{-# LANGUAGE OverloadedStrings #-}

module Lib (
    GLTFModel(..)
    ,   fromByteString
    ,   fromChunk
    ,   validateGLTF
    ,   readGLTF
    ) where

import Foreign.Ptr
import Data.Maybe ( fromJust )
import Codec.GlTF
    ( GlTF(..),
      fromByteString,
      fromChunk,
      fromFile )

import Control.Monad         (forM_)
import Codec.GlTF.Mesh       ( Mesh(..), MeshPrimitive(..) )
import Codec.GlTF.Buffer     ( Buffer(..) )
import Codec.GlTF.Accessor   (Accessor(..), unAccessorIx )
import Codec.GlTF.BufferView ( BufferViewIx(..) ) 
import qualified Data.Vector as V 
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector.Storable   as SV

-- user will provide stream data using this setup function
type BufferSetter = (Ptr () -> IO ()) -> IO ()
data Array  -- array type, element count (NOT byte size!), setter
    = Array ArrayType Int BufferSetter
    
data ArrayType
    = ArrWord8
    | ArrWord16
    | ArrWord32
    | ArrInt8
    | ArrInt16
    | ArrInt32
    | ArrFloat
    | ArrHalf     -- Hint: half float is not supported in haskell
    deriving (Show,Eq,Ord)

newtype GLTFModel = GLTFModel { gltfRaw :: Codec.GlTF.GlTF } deriving Show

withV :: SV.Storable a => SV.Vector a -> (Ptr b1 -> IO b2) -> IO b2
withV a f        = SV.unsafeWith a (\p -> f $ castPtr p)

-- cvtPosNorm :: (SV.Vector a1, SV.Vector a2) -> (Int -> BufferSetter -> Array, Int -> BufferSetter -> Array)
-- cvtPosNorm (p,n) = (f p, f n) where f sv = Array $ ArrFloat (3 * SV.length sv) $ withV sv

-- processGLTF :: Vector Codec.GlTF.Buffer.Buffer -> Vector Accessor -> Vector BufferView -> (Array, Array, Vector (Array, Array))
-- processGLTF gBuffers gAccessors bufferViewIxs = do
--   let indices          = fromJust $ extractData (gBuffers   V.! 4) (gAccessors V.! 4) (bufferViewIxs V.! 4)
--       normals          = fromJust $ extractData (gBuffers   V.! 1) (gAccessors V.! 1) (bufferViewIxs V.! 1)
--       texCoords        = fromJust $ extractData (gBuffers   V.! 3) (gAccessors V.! 3) (bufferViewIxs V.! 3)
--   return (Array (ArrWord32 (SV.length $ SV.fromList indices) (withV $ SV.fromList indices)), Array (ArrFloat  (2 * SV.length (SV.fromList texCoords)) (withV (V.fromList texCoords))), V.map cvtPosNorm (V.fromList normals))

validateGLTF :: Either String GlTF -> IO GlTF
validateGLTF gl = case gl of 
                    Left s     -> error $ "Parse Error" ++ s
                    Right gltf -> return gltf 

readGLTF :: String -> IO ()
readGLTF gltfFilename = do

    -- open the gltf file
    gltfFile <- fromFile gltfFilename
    rawGLTF  <- validateGLTF gltfFile

    -- 1. Extract the name of the bin file, for the sake of simplicity I'm assuming there's only one.
    -- 2. Open it with the cursor at the end of the file so we can determine it's size,
    -- We could techincally read the filesize from the gltf file, but I trust the file itself more.
    -- 3. Now that we have the files read out, let's actually do something with them
    -- This code prints out all the vertex positions for the first primitive
    -- Get the primitve we want to print out. 
    -- 4. Get the accessor for position.
    -- 5. Get the bufferView 
    -- 6. Now get the start of the float3 array by adding the bufferView byte offset to the bin pointer
    -- It's a little sketchy to cast to a raw float array, but hey, it works.
    let primitive        = primitives (fromJust (meshes rawGLTF) V.! 0) V.! 0
        positionActual   = unAccessorIx $ fromJust (HashMap.lookup "POSITION"   $ attributes primitive)
        texcoord_0       = unAccessorIx $ fromJust (HashMap.lookup "TEXCOORD_0" $ attributes primitive)
        normal           = unAccessorIx $ fromJust (HashMap.lookup "NORMAL"     $ attributes primitive)
        positionAccessor = (fromJust $ accessors   rawGLTF) V.! positionActual
        normalAccessor   = (fromJust $ accessors   rawGLTF) V.! normal
        texcoordAccessor = (fromJust $ accessors   rawGLTF) V.! texcoord_0
        texcoordActual   = (fromJust $ bufferViews rawGLTF) V.! unBufferViewIx (fromJust (bufferView texcoordAccessor))
        normalActual     = (fromJust $ bufferViews rawGLTF) V.! unBufferViewIx (fromJust (bufferView normalAccessor))
        bufferViewActual = (fromJust $ bufferViews rawGLTF) V.! unBufferViewIx (fromJust (bufferView positionAccessor))
        buffer           = (fromJust $ buffers     rawGLTF)

    -- Print out all the vertex positions 
    V.forM_ buffer print --(lookup "count" positionAccessor) buffer

    -- And as a cherry on top, let's print out the total number of verticies
    print $ "vertices:   "  ++ show (count positionAccessor)
    print $ "position:   "  ++ show positionActual
    print $ "normals:    "  ++ show normalActual
    print $ "texcoord:   "  ++ show texcoordActual 
    print $ "all bufferViews: " ++ show (fromJust $ bufferViews rawGLTF) 
    -- print $ "bufferView: "  ++ show bufferViewActual
    -- print $ (Array (ArrWord32 (SV.length $ SV.fromList primitive) (withV $ SV.fromList primitive)), Array (ArrFloat  (2 * SV.length (SV.fromList texCoords)) (withV (V.fromList [texcoord_0]))), cvtPosNorm normal)
    return (Array (ArrWord32 (SV.length $ SV.fromList primitive)   (withV $ SV.fromList primitive)), Array (ArrFloat  (2 * SV.length (SV.fromList texCoords)) (withV (V.fromList [texcoord_0]))), cvtPosNorm normal)
