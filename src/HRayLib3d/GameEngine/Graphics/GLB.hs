module HRayLib3d.GameEngine.Graphics.GLB
  ( unpackChunk
  , unpackGLBChunk
  , addGLB
  , loadGlTFFromGLB
  , GLBInstance(..)
  ) where

import Data.Binary.Get ( Get(..), ByteOffset(..) )
import qualified Data.Vector as V
import qualified Data.ByteString as BS

import HRayLib3d.GameEngine.Data.GLB  ( GLBModel(..)  )
import HRayLib3d.GameEngine.Data.GLTF ( GLTFModel(..) )
import qualified Codec.GLB  as GLB
import qualified Codec.GlTF as GlTF

-- GLTF ByteStrings
data GLBInstance = GLBInstance { 
    glbInstanceModel  :: GLBModel, 
    glbContents       :: V.Vector (String, BS.ByteString) 
  } deriving (Show)

unpackGLBChunk :: Either (ByteOffset, String) GLB.GLB -> Get GLB.GLB
unpackGLBChunk chuk = case chuk of
                Left  (bos, st) -> error ("Bad Read, ByteOffset: " ++ show bos ++ ", Msg: " ++ st)
                Right a -> return a

-- GLB.Chunk -> GLTF
unpackChunk :: Either String GlTF.GlTF -> GlTF.GlTF
unpackChunk chunk = case chunk of
                Left  _ -> error "Bad Chunk"
                Right a -> a

addGLB :: GLBModel -> IO GLBInstance
addGLB model = do
  let objs = V.map (\x -> (show $ GLB.chunkType x, GLB.chunkData x)) $ GLB.chunks (rawGlb model)
  return $ GLBInstance { glbInstanceModel = model, glbContents = objs }

loadGlTFFromGLB :: GLBInstance -> Int -> GLTFModel
loadGlTFFromGLB glb index = 
  case GlTF.fromByteString (snd $ glbContents glb V.! index) of
    Left  _     -> error "mismatch of loader type (currently GLB Chunk -> GLTF)" 
    Right gltf  -> GLTFModel gltf
