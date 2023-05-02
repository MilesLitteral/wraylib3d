{-# LANGUAGE PackageImports, LambdaCase, OverloadedStrings, RecordWildCards #-}

--VULKAN TEST lets see how much we have to actually change to get this backend set up!
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Catch
import Data.Text (Text)
import Data.Vector (Vector,(!))
import Data.ByteString.Char8 (unpack,pack)
import qualified Data.ByteString as SB
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.ByteString.Base64 as B64

import System.Exit
import Data.Time.Clock
import Data.Aeson
import Foreign

import qualified Network.WebSockets  as WS
import Network.Socket

import "GLFW-b" Graphics.UI.GLFW as GLFW
import "OpenGLRaw" Graphics.GL.Core33
import Codec.Picture as Juicy

import LambdaCube.IR
import LambdaCube.PipelineSchema
import LambdaCube.Mesh
import LambdaCube.GL
import LambdaCube.GL.Mesh
import TestData

import Control.Exception
import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Graphics.UI.GLFW         (ClientAPI (..), WindowHint (..))
import qualified Graphics.UI.GLFW         as GLFW

import LambdaCube.IR
import LambdaCube.Mesh
import LambdaCube.Vulkan
import LambdaCube.Vulkan.Mesh
import LambdaCube.Vulkan.Util
import LambdaCube.PipelineSchema
import TestData

                
vkWithGLFWWindow :: String -> Int -> Int -> (GLFW.Window -> IO ()) -> IO ()
vkWithGLFWWindow  title height width action = do
  GLFW.init >>= flip unless
    (throwVKMsg "Failed to initialize GLFW.")

  -- even if something bad happens, we need to terminate GLFW
  flip finally (GLFW.terminate >> putStrLn "Terminated GLFW.") $ do
    GLFW.getVersionString >>= mapM_ (putStrLn . ("GLFW version: " ++))

    GLFW.vulkanSupported >>= flip unless
      (throwVKMsg "GLFW reports that vulkan is not supported!")

    GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
    GLFW.windowHint $ WindowHint'Resizable False

    mw <- GLFW.createWindow height width title Nothing Nothing
    case mw of
      Nothing -> throwVKMsg "Failed to initialize GLFW window."
      Just w  -> do
        putStrLn "Initialized GLFW window."
        finally (action w)
                (GLFW.destroyWindow w >> putStrLn "Closed GLFW window.")

vkGlfwMainLoop :: GLFW.Window -> IO () -> IO ()
vkGlfwMainLoop w action = go
    where
    go = do
        should <- GLFW.windowShouldClose w
        unless should $ GLFW.pollEvents >> action >> go

main = do
  exts <- GLFW.getRequiredInstanceExtensions
  vkWithGLFWWindow "LambdaCube 3D Vulkan Backend" 256 256 $ \window ->
       withVulkanInstanceExt "LambdaCube 3D Vulkan Backend" exts $ \vulkanInstance -> do
          dev <- vkPickPhysicalDevice vulkanInstance
          putStrLn $ "Selected device: " ++ show dev
          vkGlfwMainLoop window (return ())

          GLFW.setWindowCloseCallback window $ Just $ \_ -> do
            GLFW.destroyWindow window
            GLFW.terminate
            exitSuccess
        
          -- connect to the test server
          forever $ catchAll (setupConnection window) $ \_ -> do
            GLFW.pollEvents
            threadDelay 100000
        
setupConnection win = withSocketsDo $ WS.runClient "192.168.0.12" 9160 "/" $ \conn -> catchAll (execConnection win conn) $ \e -> do
  WS.sendTextData conn . encode $ RenderJobError $ displayException e

execConnection win conn = do
  putStrLn "Connected!"
  -- register backend
  WS.sendTextData conn . encode $ ClientInfo
    { clientName    = "Haskell Vulkan 1.3"
    , clientBackend = OpenGL33
    }
  chan <- newEmptyMVar :: IO (MVar RenderJob)
  -- wait for incoming render jobs
  _ <- forkIO $ forever $ do
        -- get the pipeline from the server
        decodeStrict <$> WS.receiveData conn >>= \case
          Nothing -> putStrLn "unknown message"
          Just renderJob -> putMVar chan renderJob
  -- process render jobs
  forever $ do
    tryTakeMVar chan >>= \case
      Nothing -> return ()
      Just rj -> processRenderJob win conn rj
    WS.sendPing conn ("hello" :: Text)
    GLFW.pollEvents
    threadDelay 100000
  putStrLn "disconnected"
  WS.sendClose conn ("Bye!" :: Text)

doAfter = flip (>>)

processRenderJob win conn renderJob@RenderJob{..} = do
  putStrLn "got render job"
  gpuData@GPUData{..} <- allocateGPUData renderJob
  -- foreach pipeline
  doAfter (disposeGPUData gpuData) $ forM_ pipelines $ \PipelineInfo{..} -> do
    putStrLn $ "use pipeline: " ++ pipelineName
    renderer <- allocRenderer pipeline
    -- foreach scene
    doAfter (disposeRenderer renderer) $ forM_ scenes $ \Scene{..} -> do
      storage <- allocStorage schema
      -- add objects
      forM_ (Map.toList objectArrays) $ \(name,objs) -> forM_ objs $ addMeshToObjectArray storage name [] . (gpuMeshes !)
      -- set render target size
      GLFW.setWindowSize win renderTargetWidth renderTargetHeight
      setScreenSize storage (fromIntegral renderTargetWidth) (fromIntegral renderTargetHeight)
      -- connect renderer with storage
      doAfter (disposeStorage storage) $ setStorage renderer storage >>= \case
        Just err -> putStrLn err
        Nothing  -> do
          -- foreach frame
          forM_ frames $ \Frame{..} -> do
            -- setup uniforms
            updateUniforms storage $ do
              forM_ (Map.toList frameTextures) $ \(name,tex) -> pack name @= return (gpuTextures ! tex)
              forM_ (Map.toList frameUniforms) $ uncurry setUniformValue
            -- rendering
            renderTimes <- V.replicateM renderCount . timeDiff $ do
              renderFrame renderer
              GLFW.swapBuffers win
              GLFW.pollEvents
            -- send render job result to server
            WS.sendTextData conn . encode . RenderJobResult $ FrameResult
              { frRenderTimes   = renderTimes
              , frImageWidth    = renderTargetWidth
              , frImageHeight   = renderTargetHeight
              }
            -- send the last render result using Base64 encoding
            WS.sendBinaryData conn . B64.encode =<< getFrameBuffer renderTargetWidth renderTargetHeight

getFrameBuffer w h = do
  glFinish
  glBindFramebuffer GL_READ_FRAMEBUFFER 0
  glReadBuffer GL_FRONT_LEFT
  glBlitFramebuffer 0 0 (fromIntegral w) (fromIntegral h) 0 (fromIntegral h) (fromIntegral w) 0 GL_COLOR_BUFFER_BIT GL_NEAREST
  glReadBuffer GL_BACK_LEFT
  withFrameBuffer 0 0 w h $ \p -> SB.packCStringLen (castPtr p,w*h*4)

withFrameBuffer :: Int -> Int -> Int -> Int -> (Ptr Word8 -> IO a) -> IO a
withFrameBuffer x y w h fn = allocaBytes (w*h*4) $ \p -> do
    glPixelStorei GL_UNPACK_LSB_FIRST    0
    glPixelStorei GL_UNPACK_SWAP_BYTES   0
    glPixelStorei GL_UNPACK_ROW_LENGTH   0
    glPixelStorei GL_UNPACK_IMAGE_HEIGHT 0
    glPixelStorei GL_UNPACK_SKIP_ROWS    0
    glPixelStorei GL_UNPACK_SKIP_PIXELS  0
    glPixelStorei GL_UNPACK_SKIP_IMAGES  0
    glPixelStorei GL_UNPACK_ALIGNMENT    1 -- normally 4!
    glReadPixels (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) GL_RGBA GL_UNSIGNED_BYTE $ castPtr p
    fn p

data GPUData
  = GPUData
  { gpuTextures :: Vector TextureData
  , gpuMeshes   :: Vector GPUMesh
  }

allocateGPUData RenderJob{..} = GPUData <$> mapM uploadTex2D textures <*> mapM uploadMeshToGPU meshes
  where uploadTex2D = uploadTexture2DToGPU . either error id . decodeImage . either error id . B64.decode . pack

disposeGPUData GPUData{..} = mapM_ disposeTexture gpuTextures >> mapM_ disposeMesh gpuMeshes

timeDiff m = (\s e -> realToFrac $ diffUTCTime e s) <$> getCurrentTime <* m <*> getCurrentTime

setUniformValue name = \case
  VBool v   -> pack name @= return v
  VV2B v    -> pack name @= return v
  VV3B v    -> pack name @= return v
  VV4B v    -> pack name @= return v
  VWord v   -> pack name @= return v
  VV2U v    -> pack name @= return v
  VV3U v    -> pack name @= return v
  VV4U v    -> pack name @= return v
  VInt v    -> pack name @= return v
  VV2I v    -> pack name @= return v
  VV3I v    -> pack name @= return v
  VV4I v    -> pack name @= return v
  VFloat v  -> pack name @= return v
  VV2F v    -> pack name @= return v
  VV3F v    -> pack name @= return v
  VV4F v    -> pack name @= return v
  VM22F v   -> pack name @= return v
  VM23F v   -> pack name @= return v
  VM24F v   -> pack name @= return v
  VM32F v   -> pack name @= return v
  VM33F v   -> pack name @= return v
  VM34F v   -> pack name @= return v
  VM42F v   -> pack name @= return v
  VM43F v   -> pack name @= return v
  VM44F v   -> pack name @= return v
