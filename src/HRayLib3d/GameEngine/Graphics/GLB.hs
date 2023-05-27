module HRayLib3d.GameEngine.Graphics.GLB
  ( unpackChunk
  , unpackGLBChunk
  , addGLB
  , GLBInstance(..)
  ) where

import Data.Binary.Get
import qualified Data.Vector as V
import qualified Data.ByteString as BS

import HRayLib3d.GameEngine.Data.GLB
import HRayLib3d.GameEngine.Data.GLTF
import HRayLib3d.GameEngine.Utils
import qualified Codec.GLB  as GLB
import qualified Codec.GlTF as GlTF

-- GLTF ByteStrings
data GLBInstance = GLBInstance { glbInstanceModel  :: GLBModel, glbContents :: V.Vector (String, BS.ByteString) } deriving (Show)

unpackGLBChunk :: Either (ByteOffset, String) GLB.GLB -> Get GLB.GLB
unpackGLBChunk chuk = case chuk of
                Left  (bos, st) -> error ("Bad Read, ByteOffset: " ++ show bos ++ ", Msg: " ++ st)
                Right a -> return a

-- GLB.Chunk -> GLTF
unpackChunk :: Either String GlTF.GlTF -> GlTF.GlTF
unpackChunk chuk = case chuk of
                Left  _ -> error "Bad Chunk"
                Right a -> a

addGLB :: GLBModel -> [String] -> IO GLBInstance
addGLB model unis = do
  let objs = V.map (\x -> (show $ GLB.chunkType x, GLB.chunkData x)) $ (glbChunks model)
  return $ GLBInstance { glbInstanceModel = model, glbContents = objs }

loadGlTFFromGLB :: GLBInstance -> Int -> GlTF.GlTF --IO GLTFModel
loadGlTFFromGLB glb index = 
  case GlTF.fromByteString (snd $ glbContents glb V.! index) of
    Left  _      -> error "mismatch of loader type (currently GLB Chunk -> GLTF)" 
    Right gltf   -> gltf
