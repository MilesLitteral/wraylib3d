module HRayLib3d.Core.ResourceBundler (loadBundle, saveBundle, saveCache) where

import Data.Aeson
import Data.Binary
import MegaStore
import Codec.GLB
import Codec.GlTF
import Codec.GlTF.Prelude
import Data.Binary.Get
import System.Directory
import qualified Data.Text as T 
import qualified Data.ByteString as BS 
import qualified Data.ByteString.Lazy as BL

type AssetBundle = MegaStore

-- use on .GLB and .GLTF exported files
asByteString :: FilePath -> IO BS.ByteString
asByteString = BS.fromFilePath 

loadGLB  :: BS.ByteString   -> Either (ByteOffset, String) GLB
loadGLB  = Codec.GLB.fromByteString 

loadGLTF :: BS.ByteString  -> Either String GlTF
loadGLTF = Codec.GlTF.fromByteString 

withGLBAsGLTF :: Chunk -> Either String GlTF
withGLBAsGLTF = Codec.GlTF.fromChunk 

loadBundle :: FilePath -> IO AssetBundle
loadBundle path = do loadStore path

saveBundle :: [(T.Text, BS.ByteString)] -> Bool -> String -> IO ()
saveBundle paths makeDir outpath = do
    createDirectoryIfMissing makeDir outpath
    saveStoreBundle outpath $ MegaStore paths 

saveCache :: [(T.Text, BS.ByteString)] -> Bool -> String -> IO ()
saveCache paths makeDir outpath = do
    createDirectoryIfMissing makeDir outpath
    saveStoreCache outpath $ MegaStore paths 

-- writeGLB :: FilePath -> GLB -> IO ()
-- writeGLB  path glb  = BL.writeFile  path $ Data.Binary.encode glb

-- writeGLTF :: FilePath -> GlTF -> IO ()
-- writeGLTF path gltf = BL.writeFile path $ Data.Aeson.encode $ Codec.GlTF.Prelude.toJSON gltf   
