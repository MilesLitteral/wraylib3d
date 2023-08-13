{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module HRayLib3d.GameEngine.Loader.GLTF
  ( readGLTF
  , loadGLTF
  , getGLTFModel
  , getGLTFFromGLB
  ) where

import Data.Char  ()
import Codec.GLB  ( Chunk )
import Codec.GlTF ( fromByteString, fromChunk )
import Data.Binary     as B ( Get )
import Data.Binary.Get as B ( runGet, getRemainingLazyByteString, lookAhead )
import Data.ByteString      ( ByteString(..) )
import HRayLib3d.GameEngine.Data.GLTF ( GLTFModel(..) )
import qualified Data.ByteString.Lazy  as LB
import qualified Data.ByteString.Char8 as SB8

-- https://wirewhiz.com/read-gltf-files/

-- GlTF	 
-- asset              :: Asset	 
-- extensionsUsed     :: Maybe (Vector Text)	 
-- extensionsRequired :: Maybe (Vector Text)	 
-- accessors          :: Maybe (Vector Accessor)	 
-- animations         :: Maybe (Vector Animation)	 
-- buffers            :: Maybe (Vector Buffer)	 
-- bufferViews        :: Maybe (Vector BufferView)	 
-- cameras            :: Maybe (Vector Camera)	 
-- images             :: Maybe (Vector Image)	 
-- materials          :: Maybe (Vector Material)	 
-- meshes             :: Maybe (Vector Mesh)	 
-- nodes              :: Maybe (Vector Node)	 
-- samplers           :: Maybe (Vector Sampler)	 
-- scenes             :: Maybe (Vector Scene)	 
-- skins              :: Maybe (Vector Skin)	 
-- textures           :: Maybe (Vector Texture)	 
-- extensions         :: Maybe Object	 
-- extras             :: Maybe Value

loadGLTF :: String -> IO GLTFModel
loadGLTF n = readGLTF <$> LB.readFile n

readGLTF :: LB.ByteString -> GLTFModel
readGLTF = runGet getGLTFModel

-- you would throw this at Codec.GLB.chunks (Vector Chunks) (ie: V.map getGLTFFromGLB $ chunks glb) -> Vector GLTFModel)
-- to load all GlTF Instances
getGLTFFromGLB :: Chunk -> Get GLTFModel
getGLTFFromGLB glbChunk = do
    case Codec.GlTF.fromChunk glbChunk of
      Left _     -> error "bad gltf chunk"
      Right gltf -> return $ GLTFModel gltf

getGLTFModel :: Get GLTFModel
getGLTFModel = do
    dat <- lookAhead getRemainingLazyByteString
    let raw = Codec.GlTF.fromByteString $ LB.toStrict dat
    case raw of
      Left _     -> error "bad gltf bytestring"
      Right gltf -> return $ GLTFModel gltf