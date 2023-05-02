{-# LANGUAGE ViewPatterns #-}
module HRayLib3d.GameEngine.Content where

import MegaStore
import Control.Monad
import Data.Char
import Data.List (isPrefixOf,elemIndex,stripPrefix)
import System.Directory
import System.FilePath
import Text.Printf
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString       as BS

import HRayLib3d.GameEngine.Data.Material hiding (Vec3)
import HRayLib3d.GameEngine.Loader.ShaderParser
import HRayLib3d.GameEngine.Loader.Zip
import HRayLib3d.GameEngine.Utils

loadPK3 :: IO (Map String Entry)
loadPK3 = do
  let takeExtensionCI = map toLower . takeExtension
  Map.unions <$> (mapM readArchive =<< filter (\n -> ".pk3" == takeExtensionCI n) <$> getDirectoryContents ".")

-- loadMegaStore :: IO (Map String BS.ByteString)
-- loadMegaStore = do Map.unions <$> (mapM loadStore =<< filter (\n -> fst n) <$> _contents (\b -> snd b))

loadShaderMap :: Map String Entry -> IO (Map String CommonAttrs)
loadShaderMap ar = do
  l <- sequence <$> forM [(n,e) | (n,e) <- Map.toList ar, ".shader" == takeExtension n, isPrefixOf "scripts" n] (\(n,e) -> parseShaders (eArchiveName e ++ ":" ++ n) . BS8.unpack <$> readEntry e)
  case l of
    Left err -> fail err
    Right (unzip -> (x,w)) -> do
      let shaders = Map.fromList . concat $ x
      writeFile (lc_q3_cache </> "shader.log") $ unlines $ printf "%d shaders" (Map.size shaders) : concat w
      return shaders
