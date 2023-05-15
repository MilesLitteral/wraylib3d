{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module HRayLib3d.GameEngine.Loader.GLB
  ( readGLB
  , loadGLB
  ) where

import Control.Monad
import Data.Int
import Data.Char
import Data.List
import Data.Maybe

import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import Data.Binary as B
import Data.Binary.Get as B
import Data.Binary.IEEE754
import Data.Vect hiding (Vector)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV

import HRayLib3d.GameEngine.Data.GLB
import HRayLib3d.GameEngine.Graphics.GLB
import qualified Codec.GLB as GLB

-- https://wirewhiz.com/read-gltf-files/
-- GLBs are Binaries of GlTFs in this context, 
-- they store GlTFs as ByteStrings and GlTFs in
-- turn store all binary data as 'Data URIs' 
-- (Raw Base64 Encoded Binary) all in file.
-- Therefore, we foregoe support of GLBs 
-- pointing to a .bin of some kind
getGLBModel :: Get GLBModel
getGLBModel = do
    dat   <- lookAhead getRemainingLazyByteString
    glb   <- unpackGLBChunk $ GLB.fromByteString (LB.toStrict dat) 
    when (GLB.version (GLB.header glb) < 1) $ fail "unsupported GLB version"
    return $ GLBModel
      { glbHeader  = GLB.header glb
      , glbChunks  = GLB.chunks glb
      }

readGLB :: LB.ByteString -> GLBModel
readGLB dat = runGet getGLBModel dat

loadGLB :: String -> IO GLBModel
loadGLB n   = readGLB <$> LB.readFile n

-- readGLBSkin :: ByteString -> GLBSkin
-- readGLBSkin txt = Map.fromList
--   [ (head k, head v)
--   | l <- lines . map toLower $ SB8.unpack txt
--   , i <- maybeToList $ elemIndex ',' l
--   , let (words -> k,words . tail -> v) = splitAt i l
--   , not . null $ k
--   , not . null $ v
--   ]
