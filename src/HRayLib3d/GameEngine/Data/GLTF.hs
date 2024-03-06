{-# LANGUAGE OverloadedStrings #-}
module HRayLib3d.GameEngine.Data.GLTF (
    GLTFModel(..)
    , fromByteString
    , fromChunk
    , validateGLTF
    , readGLTF
    ) where

import Codec.GlTF
    ( GlTF(..)
      , fromByteString
      , fromChunk
      , fromFile )
import Codec.GlTF.Mesh ( Mesh(..), MeshPrimitive(..) )
import Codec.GlTF.Buffer ( Buffer(..) )
import Codec.GlTF.Accessor (Accessor(..))
import Codec.GlTF.BufferView ( BufferViewIx(..) ) 

import Data.Maybe ( fromJust )
import qualified Data.Vector as V 
import qualified Data.HashMap.Strict as HashMap

newtype GLTFModel = GLTFModel { gltfRaw :: Codec.GlTF.GlTF } deriving Show

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

    let binFilename      = fromJust $ uri (fromJust (buffers rawGLTF)    V.! 0)
        primitive        = primitives  (fromJust (meshes rawGLTF) V.! 0) V.! 0
        positionAccessor = fromJust (accessors rawGLTF) V.! 0
        positionActual   = fromJust (HashMap.lookup "POSITION" $ attributes primitive)
        bufferViewActual = fromJust (bufferViews rawGLTF) V.! unBufferViewIx (fromJust (bufferView positionAccessor))
        buffer           = fromJust $ buffers rawGLTF -- (float*)(bin.data() + bufferView["byteOffset"].asInt());

    -- Print out all the vertex positions 
    V.forM_ buffer print --(lookup "count" positionAccessor) buffer

    -- And as a cherry on top, let's print out the total number of verticies
    print $ "binFilename"    ++ show binFilename
    print $ "vertices: "     ++ show (count positionAccessor)
    print $ "position: "     ++ show positionActual
    print $ "raw bufferView" ++ show bufferViewActual