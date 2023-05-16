module Manifest.OpenGL.Format.Glb (
    GLB,
    loadGLBFromBytes,
    loadGLBFromFile,
)  
where 

import Codec.GLB (GLB, fromByteString, fromFile)
import Manifest.Utils.Log
import Data.ByteString.Lazy.Internal
import qualified Data.Binary as BW
import qualified Data.Binary.Get as B 
import qualified Data.ByteString as BS

--https://hackage.haskell.org/package/gltf-codec-0.1.0.4/docs/Codec-GLB.html

loadGLBFromBytes :: BS.ByteString -> Either (B.ByteOffset, String) GLB
loadGLBFromBytes = fromByteString

loadGLBFromFile :: FilePath    -> IO (Either (B.ByteOffset, String) GLB) 
loadGLBFromFile  = fromFile

exportGLBToBytes :: BW.Binary a => FilePath -> Data.ByteString.Lazy.Internal.ByteString
exportGLBToBytes =  BW.encode 

exportGLBFile :: BW.Binary a => FilePath -> a -> IO () 
exportGLBFile    = BW.encodeFile