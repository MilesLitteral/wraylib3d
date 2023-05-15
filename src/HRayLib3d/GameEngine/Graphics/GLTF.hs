{-# LANGUAGE RecordWildCards #-}
module HRayLib3d.GameEngine.Graphics.GLTF 
  (-- ( addGLTF
  -- , addGPUGLTF
  -- , setGLTFFrame
  -- , uploadGLTF
  -- , GPUGLTF(..)
  -- , GLTFInstance(..)
  ) where

import Foreign
import Data.Map (Map)
import Data.Maybe
import Data.Word
import Data.Vector (Vector)
import Data.HashSet (HashSet)
import qualified Data.Map as Map
import qualified Data.HashSet as HashSet
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Control.Monad

import Codec.GlTF
import Codec.GlTF.Texture
import Codec.GlTF.Material
import Codec.GlTF.Animation
import Codec.GlTF.Buffer (Buffer(..))
import Codec.GlTF.Accessor
import Codec.GlTF.URI 
import Data.ByteString.UTF8 as BSU      -- from utf8-string

import LambdaCube.GL
import LambdaCube.GL.Mesh
import HRayLib3d.GameEngine.Data.GLTF
import HRayLib3d.GameEngine.Graphics.Storage
import HRayLib3d.GameEngine.Utils


-- data GlTFInstance
--   = GlTFInstance
--   { gltfInstanceObject :: [Object]
--   , gltfInstanceBuffer :: LambdaCube.GL.Buffer
--   , gltfInstanceFrames :: Vector [(Int, Array)]
--   , gltfInstanceModel  :: GLTFModel
--   }

-- data GPUGlTF
--   = GPUGlTF
--   { gpuGlTFBuffer    :: LambdaCube.GL.Buffer
--   , gpuGlTFSurfaces  :: [(IndexStream LambdaCube.GL.Buffer, Map String (Stream LambdaCube.GL.Buffer))] -- index stream, attribute streams
--   , gpuGlTFFrames    :: Vector [(Int,Array)]
--   , gpuGlTFModel     :: GLTFModel
--   , gpuGlTFShaders   :: HashSet String
--   }

-- setGlTFFrame :: GlTFInstance -> Int -> IO ()
-- setGlTFFrame (GlTFInstance{..}) idx = case gltfInstanceFrames V.!? idx of
--   Just frame  -> updateBuffer gltfInstanceBuffer frame
--   Nothing     -> pure ()

-- {-
--     buffer layout
--       index arrays for surfaces         [index array of surface 0,          index array of surface 1,         ...]
--       texture coord arrays for surfaces [texture coord array of surface 0,  texture coord array of surface 1, ...]
--       position arrays for surfaces      [position array of surface 0,       position array of surface 1,      ...]
--       normal arrays for surfaces        [normal array of surface 0,         normal array of surface 1,        ...]
--     in short: [ surf1_idx..surfN_idx
--               , surf1_tex..surfN_tex
--               , surf1_pos..surfN_pos
--               , surf1_norm..surfN_norm
--               ]
-- -}

-- -- GlTF	 
-- -- asset :: Asset	 
-- -- extensionsUsed :: Maybe (Vector Text)	 
-- -- extensionsRequired :: Maybe (Vector Text)	 
-- -- accessors :: Maybe (Vector Accessor)	 
-- -- animations :: Maybe (Vector Animation)	 
-- -- buffers :: Maybe (Vector Buffer)	 
-- -- bufferViews :: Maybe (Vector BufferView)	 
-- -- cameras :: Maybe (Vector Camera)	 
-- -- images :: Maybe (Vector Image)	 
-- -- materials :: Maybe (Vector Material)	 
-- -- meshes :: Maybe (Vector Mesh)	 
-- -- nodes :: Maybe (Vector Node)	 
-- -- samplers :: Maybe (Vector Sampler)	 
-- -- scenes :: Maybe (Vector Scene)	 
-- -- skins :: Maybe (Vector Skin)	 
-- -- textures :: Maybe (Vector Texture)	 
-- -- extensions :: Maybe Object	 
-- -- extras :: Maybe Value

-- -- byteLength :: Size	 
-- -- uri :: Maybe URI	 
-- -- name :: Maybe Text	 
-- -- extensions :: Maybe Object	 
-- -- extras :: Maybe Value
--             -- bufferData = BL.fromStrict (SB8.drop byteOffset decodedData)
--             -- binaryData = BL.take byteLength bufferData
--         --return binaryData

-- decodeBase64 ::   BL.ByteString -> Maybe BS.ByteString
-- decodeBase64 bs = B64.decode bs >>= return . SB8.dropWhile (== '\NUL')

-- -- extractData :: Codec.GlTF.Accessor.Accessor -> BL.ByteString -> Maybe [Float]
-- -- extractData accessor buffer = do
-- --   let count         = fromIntegral (accessorCount accessor)
-- --       byteOffset    = fromIntegral (accessorByteOffset accessor)
-- --       byteStride    = fromIntegral <$> accessorByteStride accessor
-- --       componentType = accessorComponentType accessor
-- --       dataType = case accessorType accessor of
-- --         "SCALAR" -> 1
-- --         "VEC2"   -> 2
-- --         "VEC3"   -> 3
-- --         "VEC4"   -> 4
-- --         _ -> error "Unsupported accessor type"
-- --   let bytes = SB8.unpack (BL.toStrict (BL.drop byteOffset buffer))
-- --       dataChunks = case byteStride of
-- --         Nothing -> chunksOf (dataType * count) bytes
-- --         Just stride -> chunksOf stride bytes
-- --   case componentType of
-- --     5120 -> Just (map fromIntegral (concatMap readInt8 dataChunks))
-- --     5121 -> Just (map fromIntegral (concatMap readUInt8 dataChunks))
-- --     5122 -> Just (map fromIntegral (concatMap readInt16LE dataChunks))
-- --     5123 -> Just (map fromIntegral (concatMap readUInt16LE dataChunks))
-- --     5124 -> Just (concatMap readInt32LE  dataChunks)
-- --     5125 -> Just (concatMap readUInt32LE dataChunks)
-- --     5126 -> Just (concatMap readFloatLE  dataChunks)
-- --     _ -> Nothing
-- --   where
-- --     readInt8 :: String -> [Int8]
-- --     readInt8 = map read . chunksOf 1
-- --     readUInt8 :: String -> [Word8]
-- --     readUInt8 = map read . chunksOf 1
-- --     readInt16LE :: String -> [Int16]
-- --     readInt16LE = map (fromIntegral . toInt16 . fromIntegral . read) . chunksOf 2
-- --     readUInt16LE :: String -> [Word16]
-- --     readUInt16LE = map (toWord16 . fromIntegral . read) . T.chunksOf 2
-- --     readInt32LE :: String -> [Int32]
-- --     readInt32LE = map (fromIntegral . Int32 . fromIntegral . read) . chunksOf 4
-- --     readUInt32LE :: String -> [Word32]
-- --     readUInt32LE = map (Word32 . fromIntegral . read) . chunksOf 4
-- --     readFloatLE :: String -> [Float]
-- --     readFloatLE = map toFloat . chunksOf 4 . map (toWord8 . fromIntegral . read)

-- cvtGlTF :: Codec.GlTF.GlTF -> (Array,Array,Vector (Array,Array))
-- cvtGlTF    Codec.GlTF.GlTF{..} = do
--   -- Extract ByteString from Base64 URI
--   let gBuffers   = fromMaybe buffers
--       gAccessors = fromMaybe accessors
--       bufferViewIndex = 0 -- Choose the desired buffer view index
--       bufferView = (fromMaybe bufferViews) V.! bufferViewIndex
--       decodedData     = uri $ (gBuffers V.! 0) -- encodedData = C8.dropWhile (/= ',') (C8.pack $ URI $ fromJust uri) (C8.unpack encodedData) 
--       dataMaybe       = show $ fromJust decodedData
--   if BSU.fromString $ "data:application/octet-stream;base64," `SB8.isPrefixOf` SB8.pack dataMaybe
--     then do
--       let encodedData = SB8.dropWhile (/= ',') (SB8.pack dataMaybe)
--           decodedData = fromJust (B64.decode (SB8.unpack encodedData))
--           byteOffset = fromMaybe 0 (byteOffset bufferView)
--           byteLength = fromMaybe 0 (byteLength bufferView)
--           bufferData = BL.fromStrict (SB8.drop byteOffset decodedData)
--           binaryData = BL.take byteLength bufferData
--       return binaryData
--     else error "Bad Data URI"
--       -- bufferData      = BL.fromStrict      decodedData
--       -- binaryData      = BL.take (byteLength) bufferData
--   --fromJust accessors    
--       -- vertexPositions = extractData (rawAccessors V.! 0) binaryData
--       -- normals         = extractData (rawAccessors V.! 1) binaryData
--       -- texCoords       = extractData (rawAccessors V.! 2) binaryData
--       -- indices         = extractData (rawAccessors V.! 3) binaryData

--       -- The accessors describe the specific data stored at the buffer view 
--       -- such as vertex positions, normals, texture coordinates, or indices
--       -- within the Buffer, they correspond with the "attributes" section of a GlTF
--       -- there may need to be a step which creates tris from either the indicies, or the vertex positions
--   --((Array ArrWord32   (SV.length indices)       (withV indices)), (Array ArrFloat  (2 * SV.length texCoords) (withV texCoords)), V.map cvtPosNorm normals) --srXyzNormal)

-- withV a f        = SV.unsafeWith a (\p -> f $ castPtr p)
-- cvtPosNorm (p,n) = (f p, f n) where f sv = Array ArrFloat (3 * SV.length sv) $ withV sv
-- addSurface sf (il,tl,pl,nl,pnl) = do
--   (i:il,t:tl,p:pl,n:nl,pn:pnl) 
--   where
--     (i, t, pn) = cvtGlTF sf
--     (p, n)     = V.head pn

-- uploadGlTF :: GLTFModel -> IO GPUGlTF
-- uploadGlTF model@GLTFModel{..} = do
--   let (il,tl,pl,nl,pnl) = V.foldr addSurface gltfSurfaces ([],[],[],[],[]) 
--   buffer <- compileBuffer (concat [il, tl, pl, nl]) 
--   let numSurfaces = V.length gltfSurfaces
--       surfaceData idx Surface{..} = (index,attributes) where
--         index = IndexStream buffer idx 0 (SV.length srGlTFTriangles)
--         countV = SV.length srGlTFTexCoords
--         attributes = Map.fromList $
--           [ ("diffuseUV",   Stream Attribute_V2F buffer (1 * numSurfaces + idx) 0 countV)
--           , ("position",    Stream Attribute_V3F buffer (2 * numSurfaces + idx) 0 countV)
--           , ("normal",      Stream Attribute_V3F buffer (3 * numSurfaces + idx) 0 countV)
--           , ("color",       ConstV4F (V4 1 1 1 1))
--           , ("lightmapUV",  ConstV2F (V2 0 0))
--           ]

--       frames = Prelude.foldr addSurfaceFrames emptyFrame $ zip [0..] pnl where
--         emptyFrame = V.replicate (V.length gltfFrames) []
--         addSurfaceFrames (idx,pn) f = V.zipWith (\l (p,n) -> (2 * numSurfaces + idx,p):(3 * numSurfaces + idx,n):l) f pn

--   return $ GPUGlTF
--     { gpuGlTFBuffer    = buffer
--     , gpuGlTFSurfaces  = zipWith surfaceData [0..] (V.toList gltfSurfaces)
--     , gpuGlTFFrames    = frames
--     , gpuGlTFModel     = model
--     , gpuGlTFShaders   = HashSet.fromList $ concat [map (SB8.unpack . fromJust . Codec.GlTF.Material.name) $ V.toList srGlTFShaders | Surface{..} <- V.toList gltfSurfaces]
--     }

-- -- addGPUGlTF :: GLStorage -> GPUGlTF -> GLTFSkin -> [String] -> IO GlTFInstance
-- -- addGPUGlTF r GPUGlTF{..} skin unis = do
-- --   let GLTFModel{..} = gpuGlTFModel
-- --   objs <- forM (zip gpuGlTFSurfaces $ V.toList gltfSurfaces) $ \((index,attrs),sf) -> do
-- --     let materialName s = case Map.lookup (SB8.unpack $ srGlTFName sf) skin of
-- --           Nothing -> SB8.unpack $ shGlTFName s
-- --           Just a  -> a
-- --     objList <- concat <$> forM (V.toList $ srGlTFShaders sf) (\s -> do
-- --       a <- addObjectWithMaterial r (materialName s) TriangleList (Just index) attrs $ setNub $ "worldMat":unis
-- --       b <- addObject r "LightMapOnly" TriangleList (Just index) attrs $ setNub $ "worldMat":unis
-- --       return [a,b])

-- --     -- add collision geometry
-- --     collisionObjs <- case V.toList mdFrames of
-- --       (Frame{..}:_) -> do
-- --         sphereObj <- uploadMeshToGPU (sphere (V4 1 0 0 1) 4 frRadius)   >>= addMeshToObjectArray r "CollisionShape" (setNub $ ["worldMat","origin"] ++ unis)
-- --         boxObj    <- uploadMeshToGPU (bbox  (V4 0 0 1 1) frMins frMaxs) >>= addMeshToObjectArray r "CollisionShape" (setNub $ ["worldMat","origin"] ++ unis)
-- --         --when (frOrigin /= zero) $ putStrLn $ "frOrigin: " ++ show frOrigin
-- --         return [sphereObj,boxObj]
-- --       _ -> return []

-- --     return $ objList ++ collisionObjs
-- --   -- question: how will be the referred shaders loaded?
-- --   --           general problem: should the gfx network contain all passes (every possible materials)?
-- --   return $ GlTFInstance
-- --     { gltfInstanceObject = concat objs
-- --     , gltfInstanceBuffer = gpuGlTFBuffer
-- --     , gltfInstanceFrames = gpuGlTFFrames
-- --     , gltfInstanceModel  = gpuGlTFModel
-- --     }

-- -- addGlTF :: GLStorage -> GLTFModel -> GLTFSkin -> [String] -> IO GlTFInstance
-- -- addGlTF r model skin unis = do
-- --   gpuGlTF <- uploadGlTF model
-- --   addGPUGlTF r gpuGlTF skin unis
