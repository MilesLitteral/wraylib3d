{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module HRayLib3d.GameEngine.Loader.GLB
  ( readGLB
  , loadGLB
  ) where

import Data.Int      ()
import Data.Char     ()
import Data.List     ()
import Data.Maybe    ()
import Data.Binary     as B ( Get )
import Data.Binary.Get as B ( runGet, getRemainingLazyByteString, lookAhead )
import Data.Binary.IEEE754  ()
import Data.Vect ()
import Data.ByteString ( ByteString )
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as V
import qualified Data.Vector.Storable  as SV
import qualified Data.ByteString.Lazy  as LB
import qualified Data.ByteString.Char8 as SB8

import Control.Monad ( when )
import HRayLib3d.GameEngine.Data.GLB     ( GLBModel(..) )
import HRayLib3d.GameEngine.Graphics.GLB ( unpackGLBChunk )
import qualified Codec.GLB as GLB

-- https://wirewhiz.com/read-gltf-files/
-- GLBs are Binaries of GlTFs in this context, 
-- they store GlTFs as ByteStrings and GlTFs in
-- turn store all binary data as 'Data URIs' 
-- (Raw Base64 Encoded Binary) all in file.
-- Therefore, we foregoe support of GLBs 
-- pointing to a .bin of some kind and instead trust
-- the engine to load the GlTF contents of GLBs as necessary
-- the GLB then is what is held in memory whilst the GLTFs
-- are instances of data from these GLBs

readGLB :: LB.ByteString -> GLBModel
readGLB     = runGet getGLBModel

loadGLB :: String -> IO GLBModel
loadGLB n   = readGLB <$> LB.readFile n

getGLBModel :: Get GLBModel
getGLBModel = do
    dat   <- lookAhead getRemainingLazyByteString
    glb   <- unpackGLBChunk $ GLB.fromByteString (LB.toStrict dat) 
    when (GLB.version (GLB.header glb) < 1) $ fail "unsupported GLB version"
    return $ GLBModel { rawGlb  = GLB.GLB (GLB.header glb) (GLB.chunks glb) }