module HRayLib3d.Core.ResourceBundler (loadBundle) where

import MegaStore
import Codec.GLB
import Codec.GlTF
import Data.Binary.Get
import System.Directory
import qualified Data.Text as T 
import qualified Data.ByteString as BS 

type AssetBundle = MegaStore

-- use on .GLB and .GLTF exported files
asByteString :: FilePath -> IO BS.ByteString
asByteString = BS.fromFilePath 

loadGLB  :: BS.ByteString   -> Either (ByteOffset, String) GLB
loadGLB  = Codec.GLB.fromByteString 

loadGLTF :: BS.ByteString  -> Either String GlTF
loadGLTF = Codec.GlTF.fromByteString 

withGLBAsGLTF :: Chunk -> Either String GlTF
withGLBAsGLTF = fromChunk 

saveBundle :: [(T.Text, BS.ByteString)] -> Bool -> String -> IO ()
saveBundle paths makeDir outpath = do
    createDirectoryIfMissing makeDir outpath
    saveStore (outpath) $ MegaStore paths 

loadBundle :: FilePath -> IO MegaStore
loadBundle path = do loadStore path

