module HRayLib3d.Core.ResourceBundler (loadBundle, saveBundle, saveCache) where

import Data.Aeson
import Data.Binary
import Data.Binary.Get

import MegaStore
import Codec.GLB
import Codec.GlTF
import Codec.GlTF.Prelude

import System.Directory
import qualified Data.Text as T 
import qualified Data.ByteString as BS 
import qualified Data.ByteString.Lazy as BL

type AssetBundle = MegaStore

-- Load AssetBundle
loadBundle :: FilePath -> IO AssetBundle
loadBundle path = do loadStore path

-- Creates an AssetBundle
saveBundle :: [(T.Text, BS.ByteString)] -> Bool -> String -> IO ()
saveBundle paths makeDir outpath = do
    createDirectoryIfMissing makeDir outpath
    saveStoreBundle outpath $ MegaStore paths 

-- Creates a Shader Cache
saveCache :: [(T.Text, BS.ByteString)] -> Bool -> String -> IO ()
saveCache paths makeDir outpath = do
    createDirectoryIfMissing makeDir outpath
    saveStoreCache outpath $ MegaStore paths 

-- use on .GLB and .GLTF files in filesystem
asByteString :: FilePath -> IO BS.ByteString
asByteString = BS.fromFilePath 

-- GLTF/GLB IO Functions
loadGLB  :: BS.ByteString   -> Either (ByteOffset, String) GLB
loadGLB  = Codec.GLB.fromByteString 

loadGLTF :: BS.ByteString  -> Either String GlTF
loadGLTF = Codec.GlTF.fromByteString 

writeGLB :: FilePath -> GLB -> IO ()
writeGLB  path glb  = BL.writeFile  path $ Data.Binary.encode glb

writeGLTF :: FilePath -> GlTF -> IO ()
writeGLTF path gltf = BL.writeFile path $ Data.Aeson.encode $ Codec.GlTF.Prelude.toJSON gltf   

withGLBAsGLTF :: Chunk -> Either String GlTF
withGLBAsGLTF = Codec.GlTF.fromChunk 

