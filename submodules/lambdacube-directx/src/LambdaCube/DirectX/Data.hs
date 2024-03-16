module LambdaCube.DirectX.Data where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.List as L
import Data.Maybe
import Foreign 
--import qualified Data.IntMap as IM
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV

--import Control.DeepSeq

import Data.Word
import Codec.Picture
import Codec.Picture.Types

import Graphics.D3D11.Interface --import Graphics.GL.Core33 
import LambdaCube.DirectX.Type
import LambdaCube.DirectX.Util

-- Buffer
disposeBuffer :: ID3D11Buffer -> IO ()
disposeBuffer (Buffer _ bo) = withArray [bo] $ glDeleteBuffers 1

compileBuffer :: [SimpleVertex] -> IO ID3D11Buffer
compileBuffer arrs = do
    let bd = D3D11BufferDesc
        { byteWidth = fromIntegral $ 8 * sizeOf (undefined :: SimpleVertex)
        , usage = D3D11UsageDefault
        , bindFlags = d3d11BindFlags [D3D11BindVertexBuffer]
        , cpuAccessFlags = 0
        , miscFlags = 0
        , structureByteStride = 0 }
    buffer <- createBuffer device bd arrs                  
    iaSetVertexBuffers deviceContext 0 [(buffer, fromIntegral $ sizeOf (undefined :: SimpleVertex), 0)]
    return $! ID3D11Buffer buffer

-- updateBuffer :: ID3D11Buffer -> [(Int,Array)] -> IO ()
-- updateBuffer (Buffer arrDescs bo) arrs = do
--     glBindBuffer GL_ARRAY_BUFFER bo
--     forM arrs $ \(i,Array arrType cnt setter) -> do
--         let ArrayDesc ty len offset size = arrDescs V.! i
--         when (ty == arrType && cnt == len) $
--             setter $! glBufferSubData GL_ARRAY_BUFFER (fromIntegral offset) (fromIntegral size)
--     glBindBuffer GL_ARRAY_BUFFER 0

-- bufferSize :: ID3D11Buffer -> Int
-- bufferSize = V.length . bufArrays

-- arraySize :: ID3D11Buffer -> Int -> Int
-- arraySize buf arrIdx = arrLength $! bufArrays buf V.! arrIdx

-- arrayType :: ID3D11Buffer -> Int -> ArrayType
-- arrayType buf arrIdx = arrType $! bufArrays buf V.! arrIdx

-- Texture
disposeTexture :: TextureData -> IO ()
disposeTexture (TextureData to) = withArray [to] $ glDeleteTextures 1

-- FIXME: Temporary implemenation
uploadTexture2DToGPU :: DynamicImage -> IO TextureData
uploadTexture2DToGPU = uploadTexture2DToGPU' True False True False

uploadTexture2DToGPU' :: ID3D11Device -> DynamicImage -> IO TextureData
uploadTexture2DToGPU' device bitmap' = do
    let bitmap = case bitmap' of
          ImageRGB8   i@(Image w h _) -> bitmap'
          ImageRGBA8  i@(Image w h _) -> bitmap'
          ImageYCbCr8 i@(Image w h _) -> ImageRGB8 $ convertImage i
          di -> ImageRGBA8 $ convertRGBA8 di
        (width,height) = bitmapSize bitmap
            bitmapSize (ImageRGB8  (Image w h _)) = (w,h)
            bitmapSize (ImageRGBA8 (Image w h _)) = (w,h)
            bitmapSize _ = error "unsupported image type :("
            withBitmap (ImageRGB8  (Image w h v)) f = SV.unsafeWith v $ f (w,h) 3 0
            withBitmap (ImageRGBA8 (Image w h v)) f = SV.unsafeWith v $ f (w,h) 4 0
            withBitmap _ _ = error "unsupported image type :("
        td = D3D11Texture2DDesc
                width
                height
                1
                1
                DxgiFormatD24UnormS8Uint
                (DxgiSampleDesc 1 0)
                D3D11UsageDefault
                (d3d11BindFlags []) --D3D11BindDepthStencil
                0
                0
    to <- createTexture2D device td (SV.toList v :: [Word16])
    return $ TextureData to
