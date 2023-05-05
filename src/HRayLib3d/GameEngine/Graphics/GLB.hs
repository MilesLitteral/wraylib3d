module HRayLib3d.GameEngine.Graphics.GLB
  ( addGLB
  , GLBInstance(..)
  ) where

import HRayLib3d.GameEngine.Data.GLB
import HRayLib3d.GameEngine.Data.GLTF
import HRayLib3d.GameEngine.Utils
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Codec.GLB  as GLB
import qualified Codec.GlTF as GlTF

-- GLTF ByteStrings
data GLBInstance = GLBInstance { glbInstanceModel  :: GLBModel, glbContents :: V.Vector (String, BS.ByteString) } deriving (Show)

unpackChunk :: Either String GlTF.GlTF -> GlTF.GlTF
unpackChunk chuk = case chuk of
                Left  _ -> error "Bad Chunk"
                Right a -> a

addGLB :: GLBModel -> [String] -> IO GLBInstance
addGLB model unis = do
  let objs = V.map (\x -> (show $ GLB.chunkType x, GLB.chunkData x)) $ (glbChunks model)
  return $ GLBInstance { glbInstanceModel = model, glbContents = objs }

-- loadGlTFFromGLB :: GLBInstance -> Int -> IO GLTFModel
-- loadGlTFFromGLB glb index = do
--   return $ 
--   GlTF.fromByteString (snd $ glbContents glb V.! index)
