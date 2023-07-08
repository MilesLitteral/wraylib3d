{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module HRayLib3d.GameEngine.Loader.BSP
  ( readBSP
  , loadBSP
  ) where

import Data.Char ( toLower )
import Data.Int  ( Int32 )
import Data.Word ( Word8, Word32 )
import Data.Binary as B
import Data.Binary.Get as B
import Data.Binary.IEEE754
import Data.Vect hiding (Vector)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad ( replicateM )

import HRayLib3d.GameEngine.Data.BSP
    ( BSPLevel(BSPLevel),
      Brush(Brush),
      BrushSide(BrushSide),
      DrawVertex(DrawVertex),
      Fog(Fog),
      Leaf(Leaf),
      LightGrid(..),
      Lightmap(Lightmap),
      Model(Model),
      Node(Node),
      Plane(Plane),
      Shader(Shader),
      Surface(Surface),
      SurfaceType(..),
      Visibility(Visibility)
      )
import HRayLib3d.Network.Realms ( Query, sendRealmQueryBS )
{-
Information:
  http://graphics.stanford.edu/~kekoa/q3/
  http://www.mralligator.com/q3/
-}
getLowerCaseString :: Int -> Get ByteString
getLowerCaseString leng = SB8.map toLower . SB8.takeWhile (/= '\0') <$> getByteString leng

getFloat  :: Get Float
getFloat  = getFloat32le

getWord   :: Get Word32
getWord   = getWord32le

getUByte  :: Get Word8
getUByte  = B.get

getUByte2 :: Get (Word8, Word8)
getUByte2 = B.get

getUByte3 :: Get (Word8, Word8, Word8)
getUByte3 = B.get

getVec2 :: Get Vec2
getVec2   = Vec2 <$> getFloat <*> getFloat

getVec3 :: Get Vec3
getVec3   = Vec3 <$> getFloat <*> getFloat <*> getFloat

getVec2i :: Get Vec2
getVec2i  = (\x y -> Vec2 (fromIntegral x) (fromIntegral y)) <$> getInt <*> getInt

getVec3i :: Get Vec3
getVec3i  = (\x y z -> Vec3 (fromIntegral x) (fromIntegral y) (fromIntegral z)) <$> getInt <*> getInt <*> getInt

getVec4RGBA :: Get Vec4
getVec4RGBA = (\r g b a -> Vec4 (f r) (f g) (f b) (f a)) <$> getUByte <*> getUByte <*> getUByte <*> getUByte
  where f v = fromIntegral v / 255

getInt :: Get Int
getInt = fromIntegral <$> getInt' :: Get Int
  where getInt' = fromIntegral <$> getWord32le :: Get Int32

getInt2 :: Get (Int, Int)
getInt2 = (,) <$> getInt <*> getInt

getItems :: Monad m => Int -> m a -> Int -> m (Vector a)
getItems elemSize a byteCount = V.replicateM (byteCount `div` elemSize) a

getHeader :: Get [(Int, Int)]
getHeader = do
    magic <- getByteString 4
    (if magic == "IBSP" then return () else fail "Invalid format.")
    version <- getWord -- add (if version == "1" then return (replicateM 17 getInt2) else fail "Unsupported version.")
    replicateM 17 getInt2

getSurfaceType :: Get SurfaceType
getSurfaceType  = getInt >>= \case
    1 -> return Planar
    2 -> return Patch
    3 -> return TriangleSoup
    4 -> return Flare
    _ -> fail "Invalid surface type"

getEntities :: Int -> Get ByteString
getEntities = getLowerCaseString

getShaders  :: Int -> Get (Vector Shader)
getShaders  = getItems  72 $ Shader     <$> getLowerCaseString 64 <*> getInt <*> getInt

getPlanes   :: Int -> Get (Vector Plane)
getPlanes   = getItems  16 $ Plane      <$> getVec3 <*> getFloat

getNodes    :: Int -> Get (Vector Node)
getNodes    = getItems  36 $ Node       <$> getInt <*> getInt2 <*> getVec3i <*> getVec3i

getLeaves   :: Int -> Get (Vector Leaf)
getLeaves   = getItems  48 $ Leaf       <$> getInt <*> getInt <*> getVec3i <*> getVec3i <*> getInt
                                        <*> getInt <*> getInt <*> getInt

getLeafSurfaces :: Int -> Get (Vector Int)
getLeafSurfaces = getItems   4   getInt

getLeafBrushes :: Int -> Get (Vector Int)
getLeafBrushes  = getItems   4   getInt

getModels :: Int -> Get (Vector Model)
getModels       = getItems  40 $ Model      <$> getVec3 <*> getVec3 <*> getInt <*> getInt <*> getInt <*> getInt

getBrushes :: Int -> Get (Vector Brush)
getBrushes      = getItems  12 $ Brush      <$> getInt <*> getInt <*> getInt

getBrushSides :: Int -> Get (Vector BrushSide)
getBrushSides   = getItems   8 $ BrushSide  <$> getInt <*> getInt

getDrawVertices :: Int -> Get (Vector DrawVertex)
getDrawVertices = getItems  44 $ DrawVertex <$> getVec3 <*> getVec2 <*> getVec2 <*> getVec3 <*> getVec4RGBA

getDrawIndices :: Int -> Get (Vector Int)
getDrawIndices  = getItems   4   getInt

getFogs :: Int -> Get (Vector Fog)
getFogs         = getItems  72 $ Fog        <$> getLowerCaseString 64 <*> getInt <*> getInt

getSurfaces :: Int -> Get (Vector Surface)
getSurfaces     = getItems 104 $ Surface    <$> getInt  <*> getInt  <*> getSurfaceType <*> getInt <*> getInt
                                            <*> getInt  <*> getInt  <*> getInt  <*> getVec2i <*> getVec2i
                                            <*> getVec3 <*> getVec3 <*> getVec3 <*> getVec3  <*> getInt2

getLightmaps :: Int -> Get (Vector Lightmap)
getLightmaps    = getItems (128*128*3) (Lightmap <$> getByteString (128*128*3))

getLightGrid :: Int -> Get (Vector LightGrid)
getLightGrid = getItems 8 $ do
    ambient     <- getUByte3
    directional <- getUByte3
    dir         <- getUByte2
    return LightGrid

getVisibility :: Int -> Get Visibility
getVisibility l = do
    nvecs   <- getInt
    szvecs  <- getInt
    vecs    <- getByteString $ nvecs * szvecs
    return $ Visibility nvecs szvecs $ V.fromList $ SB.unpack vecs

readBSP :: LB.ByteString -> BSPLevel
readBSP dat = BSPLevel
    (lump getEntities      0)
    (lump getShaders       1)
    (lump getPlanes        2)
    (lump getNodes         3)
    (lump getLeaves        4)
    (lump getLeafSurfaces  5)
    (lump getLeafBrushes   6)
    (lump getModels        7)
    (lump getBrushes       8)
    (lump getBrushSides    9)
    (lump getDrawVertices  10)
    (lump getDrawIndices   11)
    (lump getFogs          12)
    (lump getSurfaces      13)
    (lump getLightmaps     14)
    (lump getLightGrid     15)
    (lump getVisibility    16)
  where
    el = runGet getHeader dat :: [(Int, Int)]
    lump g i = runGet (let (o,l) = el !! i in skip o >> g l) dat

loadBSP :: String -> IO BSPLevel
loadBSP n = readBSP <$> LB.readFile n

-- Realm Functions
-- it is presumed the Query will be in a style to actually
-- get a Level, like "SELECT <bsp_name> FROM realm/<bsp_name>/bsp/"
-- where this will result in a Lazy ByteString that is then
-- read by readBSP in IO
sendRealmBSPQuery :: Query -> IO BSPLevel
sendRealmBSPQuery q = readBSP <$> sendRealmQueryBS q