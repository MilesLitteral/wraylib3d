{-# LANGUAGE ViewPatterns #-}
module HRayLib3d.GameEngine.Content where

import Data.Char
import Data.Map (Map)
import Data.List (isPrefixOf,elemIndex,stripPrefix)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BL

import MegaStore
import Text.Printf
import Control.Monad
import Control.Applicative
import System.Directory
import System.FilePath

import HRayLib3d.Core.ResourceBundler
import HRayLib3d.GameEngine.Utils
import HRayLib3d.GameEngine.Loader.Zip
import HRayLib3d.GameEngine.Loader.ShaderParser
import HRayLib3d.GameEngine.Data.Material hiding (Vec3)

type AssetContent  = BS.ByteString
type ShaderContent = BS.ByteString

loadPK3 :: IO (Map String Entry)
loadPK3 = do
  let takeExtensionCI = map toLower . takeExtension
  Map.unions <$> (mapM readArchive =<< filter (\n -> ".pk3" == takeExtensionCI n) <$> getDirectoryContents ".")

loadAssetBundle :: IO (Map String AssetContent)
loadAssetBundle = do
  let takeExtensionCI = map toLower . takeExtension
  mst <- loadStore "./wrl3d-assets.assetBundle"
  return (Map.fromList (map (\x -> (T.unpack $ fst x , snd x)) $ _contents mst))
  -- Map.unions <$> (mapM loadStore =<< filter (\n -> ".assetBundle" == takeExtensionCI n) <$> getDirectoryContents ".")

pk3ToAssetBundle :: IO (Map String AssetContent)
pk3ToAssetBundle = do
  let takeExtensionCI = map toLower . takeExtension
  --mst <- loadStore "./wrl3d-assets.assetBundle"
  pk3 <- loadPK3
  let fileData = Map.keys pk3
  files      <- mapM BL.readFile fileData 
  pathExists <- doesPathExist "./pak0.assetBundle"
  if pathExists
    then return (Map.fromList $ zip fileData $ map BL.toStrict files)
    else do
      saveBundle (zip (map T.pack fileData) $ map BL.toStrict files) False "./pak0" -- IMPORTANT to replace pk3 this is necessary, use file paths as keys to access bundles 
      return (Map.fromList $ zip fileData $ map BL.toStrict files)
      -- Map.unions <$> (mapM loadStore =<< filter (\n -> ".assetBundle" == takeExtensionCI n) <$> getDirectoryContents ".")

-- saveBundle :: [(T.Text, BS.ByteString)] -> Bool -> String -> IO ()
-- saveBundle paths makeDir outpath

loadShaderCache :: IO (Map String ShaderContent)
loadShaderCache = do
  let takeExtensionCI = map toLower . takeExtension
  mst <- loadStore "./wrl3d-shaders.shadercache"
  return (Map.fromList (map (\x -> (T.unpack $ fst x , snd x)) $ _contents mst)) --loadStore =<< filter (\n -> ".shaderCache" == takeExtensionCI n) <$> getDirectoryContents ".")

loadShaderMap :: Map String Entry -> IO (Map String CommonAttrs)
loadShaderMap ar = do
  l <- sequence <$> forM [(n,e) | (n,e) <- Map.toList ar, ".shader" == takeExtension n, isPrefixOf "scripts" n] (\(n,e) -> parseShaders (eArchiveName e ++ ":" ++ n) . BS8.unpack <$> readEntry e)
  case l of
    Left err -> fail err
    Right (unzip -> (x,w)) -> do
      let shaders = Map.fromList . concat $ x
      writeFile (lc_q3_cache </> "shader.log") $ unlines $ printf "%d shaders" (Map.size shaders) : concat w
      return shaders

-- loadMegaStore :: IO (Map String BS.ByteString)
-- loadMegaStore = do Map.unions <$> (mapM loadStore =<< filter (\n -> fst n) <$> _contents (\b -> snd b))

{- 
  loadMegaStore   :: IO (Map String Entry)
  loadMegaStore = do
    let takeExtensionCI = map toLower . takeExtension
    mst <- loadStore "./wrl3d-shaders.megastore"
    return (Map.fromList (map (\x -> (T.unpack $ fst x , snd x)) $ _contents mst))
    --Map.unions <$> (mapM loadStore =<< filter (\n -> ".megastore"   == takeExtensionCI n) <$> getDirectoryContents ".")
-}