module HRayLib3d.GameEngine.Data.GLTF (
    GLTFModel(..)
    ,   fromByteString 
    ,   fromChunk
    ) where

import Codec.GlTF ( fromByteString, fromChunk, GlTF )

newtype GLTFModel = GLTFModel { gltfRaw :: Codec.GlTF.GlTF } deriving Show
