{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module HRayLib3d.GameEngine.Loader.GLTF
  ( --readGLTF
  -- , loadGLTF
  -- , readGLTFSkin
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

import HRayLib3d.GameEngine.Data.GLTF
-- https://wirewhiz.com/read-gltf-files/

getLowerCaseString :: Int -> Get ByteString
getLowerCaseString len = SB8.map toLower . SB8.takeWhile (/= '\0') <$> getByteString len

getUByte    = B.get                                       :: Get Word8
getFloat    = getFloat32le                                :: Get Float
getVec2     = Vec2 <$> getFloat <*> getFloat              :: Get Vec2
getVec3     = Vec3 <$> getFloat <*> getFloat <*> getFloat :: Get Vec3
getMat3     = Mat3 <$> getVec3 <*> getVec3 <*> getVec3
getVec3i16  = (\x y z -> Vec3 (fromIntegral x) (fromIntegral y) (fromIntegral z)) <$> getInt16 <*> getInt16 <*> getInt16 :: Get Vec3
getInt16    = fromIntegral <$> getInt'                           :: Get Int
  where getInt' = fromIntegral <$> getWord16le                   :: Get Int16
getInt      = fromIntegral <$> getInt'                           :: Get Int
  where getInt' = fromIntegral <$> getWord32le                   :: Get Int32
getInt3     = (,,) <$> getInt <*> getInt <*> getInt              :: Get (Int, Int, Int)
getAngle    = (\i -> fromIntegral i * 2 * pi / 255) <$> getUByte :: Get Float

getV :: Int -> Int -> Get a -> LB.ByteString -> V.Vector a
getV o n f dat = runGet (V.replicateM n f) (LB.drop (fromIntegral o) dat)

getSV :: SV.Storable a => Int -> Int -> Get a -> LB.ByteString -> SV.Vector a
getSV o n f dat = runGet (SV.replicateM n f) (LB.drop (fromIntegral o) dat)

getFrame    = Frame <$> getVec3 <*> getVec3 <*> getVec3 <*> getFloat <*> getLowerCaseString 64 :: Get Frame
getTag      = Tag <$> getLowerCaseString 64 <*> getVec3 <*> getMat3 :: Get Tag
getShader   = Shader <$> getLowerCaseString 64 <*> getInt  :: Get Shader

getXyzNormal :: Get (Vec3, Vec3)
getXyzNormal = do
    v <- getVec3i16
    lat <- getAngle
    lng <- getAngle
    return (v &* (1/64), Vec3 (cos lat * sin lng) (sin lat * sin lng) (cos lng))


-- -- import Codec.GLTF
-- -- fromByteString :: ByteString -> Either String GlTF
-- -- fromFile       :: FilePath   -> IO (Either String GlTF)
-- -- fromChunk      :: Chunk      -> Either String GlTF

-- getSurface :: Get Surface
-- getSurface = (\(o,v) -> skip o >> return v) =<< lookAhead getSurface'
--   where
--     getSurface' = do
--         dat <- lookAhead getRemainingLazyByteString
--         "IDP3" <- getByteString 4
--         name <- getLowerCaseString 64
--         flags <- getInt
--         [nFrames,nShaders,nVerts,nTris] <- replicateM 4 getInt
--         [oTris,oShaders,oTexCoords,oXyzNormals,oEnd] <- replicateM 5 getInt
--         return $ (oEnd,Surface name
--           (getV oShaders nShaders getShader dat)
--           (getSV oTris (3*nTris) (fromIntegral <$> getWord32le) dat)
--           (getSV oTexCoords nVerts getVec2 dat)
--           (getV oXyzNormals nFrames ((\(p,n) -> (SV.fromList p,SV.fromList n)) . unzip <$> replicateM nVerts getXyzNormal) dat))

-- getGLTFModel :: Get GLTFModel
-- getGLTFModel = do
--     dat <- lookAhead getRemainingLazyByteString
--     "IDP3"  <- getByteString 4
--     version <- getInt
--     when (version /= 15) $ fail "unsupported GlTF version"
--     name <- getLowerCaseString 64
--     flags <- getInt
--     [nFrames,nTags,nSurfaces,nSkins] <- replicateM 4 getInt
--     [oFrames,oTags,oSurfaces,oEnd] <- replicateM 4 getInt
--     return $ GLTFModel
--       { gltfFrames    = getV oFrames nFrames getFrame dat
--       , gltfTags      = (\v -> HashMap.fromList [(tgGlTFName t,t) | t <- V.toList v]) <$> getV oTags nFrames (V.replicateM nTags getTag) dat
--       , gltfSurfaces  = getV oSurfaces nSurfaces getSurface dat
--       }

-- loadGLTF :: String -> IO GLTFModel
-- loadGLTF n = readGLTF <$> LB.readFile n

-- readGLTF :: LB.ByteString -> GLTFModel
-- readGLTF dat = runGet getGLTFModel dat

-- readGLTFSkin :: ByteString -> GLTFSkin
-- readGLTFSkin txt = Map.fromList
--   [ (head k,head v)
--   | l <- lines . map toLower $ SB8.unpack txt
--   , i <- maybeToList $ elemIndex ',' l
--   , let (words -> k,words . tail -> v) = splitAt i l
--   , not . null $ k
--   , not . null $ v
--   ]
