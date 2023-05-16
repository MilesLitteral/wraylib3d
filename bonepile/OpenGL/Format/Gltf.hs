module Manifest.OpenGL.Format.Gltf 
(
    GlTF,
    loadGLTFFromBytes,
    loadGLTFFromFile,
    loadGLTFFromMesh
)  
where 

import Codec.GlTF
import Codec.GlTF.URI
import qualified Data.ByteString as BS 
import qualified Data.ByteString.Lazy as BL 
import qualified Data.Aeson as JSON
import Manifest.Utils.Log

-- https://hackage.haskell.org/package/gltf-codec-0.1.0.4/docs/Codec-GlTF.html

loadGLTFFromBytes :: BS.ByteString -> Either String GlTF
loadGLTFFromBytes = fromByteString 

loadGLTFFromFile  :: FilePath -> IO (Either String GlTF)
loadGLTFFromFile  = fromFile 

loadGLTFFromMesh :: (FilePath -> IO (Either String BS.ByteString)) -> URI -> IO (Either String BS.ByteString)
loadGLTFFromMesh urii = loadURI urii

exportGLTFToBytes :: JSON.ToJSON a => a -> BL.ByteString
exportGLTFToBytes = JSON.encode 

exportGLTFFile :: JSON.ToJSON a => FilePath -> a -> IO () 
exportGLTFFile = JSON.encodeFile