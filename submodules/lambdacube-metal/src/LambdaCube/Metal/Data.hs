module LambdaCube.Metal.Data where

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

import LambdaCube.Metal.Type
import LambdaCube.Metal.Util
import Graphics.GL.Core33
--import LambdaCube.Metal.Bindings  --Graphics.GL.Core33
--Utility Functions
calcDesc (offset,setters,descs) (Array arrType cnt setter) =
          let size = cnt * sizeOfArrayType arrType
            in (size + offset, (offset, size,setter):setters, ArrayDesc arrType cnt offset size:descs)

-- Buffer
disposeBuffer :: Buffer -> IO ()
disposeBuffer (Buffer _ bo) = withArray [bo] $ resetCommandsInBuffer 1 

compileBuffer :: MTLDevice -> [Array] -> IO Buffer
compileBuffer dev arrs = do
    let (bufSize,arrSetters,arrDescs) = foldl' calcDesc (0,[],[]) arrs
    bo <- alloca $! \pbo -> makeBuffer dev pbo 1 MTLResourceOptionCPUCacheModeDefault >> peek pbo --glGenBuffers
    forM_ arrSetters $! \(offset,size,setter) -> setter $! makeBuffer dev (fromIntegral offset) (fromIntegral size) MTLResourceOptionCPUCacheModeDefault
    return $! Buffer (V.fromList $! reverse arrDescs) bo

updateBuffer :: MTLDevice -> Buffer -> [(Int,Array)] -> IO ()
updateBuffer (Buffer arrDescs bo) arrs = forM arrs $ 
    \(i,Array arrType cnt setter) -> do
        let ArrayDesc ty len offset size = arrDescs V.! i
        when (ty == arrType && cnt == len) $ 
            setter $! makeBuffer dev (fromIntegral offset) (fromIntegral size) MTLResourceOptionCPUCacheModeDefault

bufferSize :: Buffer -> Int
bufferSize = V.length . bufArrays

arraySize :: Buffer -> Int -> Int
arraySize buf arrIdx = arrLength $! bufArrays buf V.! arrIdx

arrayType :: Buffer -> Int -> ArrayType
arrayType buf arrIdx = arrType $! bufArrays buf V.! arrIdx

-- Texture
disposeTexture :: TextureData -> IO ()
disposeTexture (TextureData to) = withArray [to] $ metalDeleteTextures --glDeleteTextures 1

-- FIXME: Temporary implemenation
uploadTexture2DToGPU :: DynamicImage -> IO TextureData
uploadTexture2DToGPU = uploadTexture2DToGPU' True False True False

uploadTexture2DToGPU' :: Bool -> DynamicImage -> IO TextureData
uploadTexture2DToGPU' isMip bitmap' = do
    let bitmap = case bitmap' of
                    ImageRGB8   i@(Image w h _) -> bitmap'
                    ImageRGBA8  i@(Image w h _) -> bitmap'
                    ImageYCbCr8 i@(Image w h _) -> ImageRGB8 $ convertImage i
                    di -> ImageRGBA8 $ convertRGBA8 di
        (width,height) = bitmapSize bitmap
                         bitmapSize (ImageRGB8  (Image w h _)) = (w,h)
                         bitmapSize (ImageRGBA8 (Image w h _)) = (w,h)
                         bitmapSize _ = error "unsupported image type :("
                         withBitmap (ImageRGB8 (Image w h v))  f = SV.unsafeWith v $ f (w,h) 3 0
                         withBitmap (ImageRGBA8 (Image w h v)) f = SV.unsafeWith v $ f (w,h) 4 0
                         withBitmap _ _ = error "unsupported image type :("
        to <- alloca $! \pto -> newTextureWithDescriptor pto >> MTLTextureDescriptor PixelFormatBGRA8Unorm width height --peek pto
        replaceRegion 0 v $ castPtr ptr
    when isMip $ replaceRegion 1 v $ castPtr ptr
    return $ TextureData to
