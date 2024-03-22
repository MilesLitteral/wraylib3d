{-# LANGUAGE LambdaCase, TypeApplications, RecordWildCards, RankNTypes, ScopedTypeVariables, DataKinds #-}

module HRayLib3d.WindowSystem.Context where 

import Data.IORef
import Data.Time.Clock.System
import qualified Data.Vector.Storable   as SV
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString        as SB 

import           FRP.Elerea.Param
import           Control.Exception
import           Control.Monad
import qualified Control.Monad.Logger           as Logger
import qualified Control.Monad.Logger.CallStack as LoggerCS

import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Storable
import           Foreign.Ptr 

import           Graphics.UI.GLFW         (ClientAPI (..), WindowHint (..))
import qualified Graphics.UI.GLFW         as GLFW

import LambdaCube.IR
import LambdaCube.GL
import LambdaCube.GL.Mesh
import HRayLib3d.GameEngine.RealmViewer.Engine
import HRayLib3d.GameEngine.RealmViewer.Camera
import HRayLib3d.WindowSystem.RenderPipelineClient hiding (frames)
import Graphics.UI.GLFW as GLFW

-- FPS tracking
data State = State { frames :: IORef Int, t0 :: IORef Double }



-- data GPUData
--   = GPUData
--   { gpuTextures :: SV.Vector TextureData
--   , gpuMeshes   :: SV.Vector GPUMesh
--   }

-- allocateGPUData RenderJob{..} = GPUData <$> mapM uploadTex2D textures <*> mapM uploadMeshToGPU meshes
--   where uploadTex2D = uploadTexture2DToGPU . either error id . decodeImage . either error id . B64.decode . pack

-- disposeGPUData GPUData{..} = mapM_ disposeTexture gpuTextures >> mapM_ disposeMesh gpuMeshes


captureRate :: Double
captureRate = 30

-- -- | Low latency time in seconds since the start
-- getTime :: HRayLib3d.WindowSystem.Context.Program r Double
-- getTime = do
--     now <- liftIO getSystemTime
--     start <- startTime <$> get
--     let deltaSeconds = systemSeconds now - systemSeconds start
--         -- Have to nanoseconds convert from Word64 before subtraction to allow negative delta.
--         deltaNanoseconds :: Int64 = fromIntegral (systemNanoseconds now) - fromIntegral (systemNanoseconds start)
--         -- Seconds in Double keep at least microsecond-precision for 285 years.
--         -- Float is not good enough even for millisecond-precision over more than a few hours.
--         seconds :: Double = fromIntegral deltaSeconds + fromIntegral deltaNanoseconds / 1e9
--     return seconds

-- timeDiff m = (\s e -> realToFrac $ diffUTCTime e s) <$> getCurrentTime <* m <*> getCurrentTime

fpsState :: IO State
fpsState = State <$> newIORef 0 <*> newIORef 0

updateFPS :: State -> Double -> IO ()
updateFPS state t1 = do
  let t = 1000*t1
      fR = frames state
      tR = t0 state
  modifyIORef fR (+1)
  t0' <- readIORef tR
  writeIORef tR $ t0' + t
  when (t + t0' >= 5000) $ do
    f <- readIORef fR
    let seconds = (t + t0') / 1000
        fps = fromIntegral f / seconds
    putStrLn (show (round fps) ++ " FPS - " ++ show f ++ " frames in " ++ show seconds)
    writeIORef tR 0
    writeIORef fR 0

edge :: Signal Bool -> SignalGen p (Signal Bool)
edge s = transfer2 False (\_ cur prev _ -> cur && not prev) s =<< delay False s

upEdge :: Signal Bool -> SignalGen p (Signal Bool)
upEdge s = transfer2 False (\_ cur prev _ -> cur && prev == False) s =<< delay False s

windowWidth, windowHeight :: Num a => a
windowWidth  = 800
windowHeight = 600

-- sdl2 utility code
initWindow :: String -> Int -> Int -> IO GLFW.Window
initWindow title width height = do
    GLFW.init
    GLFW.defaultWindowHints
    mapM_ GLFW.windowHint
      [ WindowHint'ContextVersionMajor 3
      , WindowHint'ContextVersionMinor 3
      , WindowHint'OpenGLProfile OpenGLProfile'Core
      , WindowHint'OpenGLForwardCompat True
      ]
    Just win <- GLFW.createWindow width height title Nothing Nothing
    GLFW.makeContextCurrent $ Just win
    return win

-- getFrameBuffer w h = do
--   glFinish
--   glBindFramebuffer GL_READ_FRAMEBUFFER 0
--   glReadBuffer GL_FRONT_LEFT
--   glBlitFramebuffer 0 0 (fromIntegral w) (fromIntegral h) 0 (fromIntegral h) (fromIntegral w) 0 GL_COLOR_BUFFER_BIT GL_NEAREST
--   glReadBuffer GL_BACK_LEFT
--   withFrameBuffer 0 0 w h $ \p -> SB.packCStringLen (castPtr p,w*h*4)

-- withFrameBuffer :: Int -> Int -> Int -> Int -> (Ptr Word8 -> IO a) -> IO a
-- withFrameBuffer x y w h fn = allocaBytes (w*h*4) $ \p -> do
--     glPixelStorei GL_UNPACK_LSB_FIRST    0
--     glPixelStorei GL_UNPACK_SWAP_BYTES   0
--     glPixelStorei GL_UNPACK_ROW_LENGTH   0
--     glPixelStorei GL_UNPACK_IMAGE_HEIGHT 0
--     glPixelStorei GL_UNPACK_SKIP_ROWS    0
--     glPixelStorei GL_UNPACK_SKIP_PIXELS  0
--     glPixelStorei GL_UNPACK_SKIP_IMAGES  0
--     glPixelStorei GL_UNPACK_ALIGNMENT    1 -- normally 4!
--     glReadPixels (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) GL_RGBA GL_UNSIGNED_BYTE $ castPtr p
--     fn p


-- setUniformValue name = \case
--   VBool v   -> pack name @= return v
--   VV2B v    -> pack name @= return v
--   VV3B v    -> pack name @= return v
--   VV4B v    -> pack name @= return v
--   VWord v   -> pack name @= return v
--   VV2U v    -> pack name @= return v
--   VV3U v    -> pack name @= return v
--   VV4U v    -> pack name @= return v
--   VInt v    -> pack name @= return v
--   VV2I v    -> pack name @= return v
--   VV3I v    -> pack name @= return v
--   VV4I v    -> pack name @= return v
--   VFloat v  -> pack name @= return v
--   VV2F v    -> pack name @= return v
--   VV3F v    -> pack name @= return v
--   VV4F v    -> pack name @= return v
--   VM22F v   -> pack name @= return v
--   VM23F v   -> pack name @= return v
--   VM24F v   -> pack name @= return v
--   VM32F v   -> pack name @= return v
--   VM33F v   -> pack name @= return v
--   VM34F v   -> pack name @= return v
--   VM42F v   -> pack name @= return v
--   VM43F v   -> pack name @= return v
--   VM44F v   -> pack name @= return v

scene win levelData graphicsData mousePosition fblrPress capturePress waypointPress capRef = do
    time  <- stateful 0 (+)
    last2 <- transfer ((0,0),(0,0)) (\_ n (_,b) -> (b,n)) mousePosition
    let mouseMove = (\((ox,oy),(nx,ny)) -> (nx-ox,ny-oy)) <$> last2
        bsp = getBSP levelData
        p0 = head . drop 1 . cycle $ getSpawnPoints levelData
    fblrPress' <- do
      j' <- upEdge $ (\(w,a,s,d,t,j) -> j) <$> fblrPress
      return $ (\(w,a,s,d,t,_) j' -> (w,a,s,d,t,j')) <$> fblrPress <*> j'
    controlledCamera <- userCamera (getTeleportFun levelData) bsp p0 mouseMove fblrPress'

    frameCount <- stateful (0 :: Int) (\_ c -> c + 1)
    capture <- transfer2 False (\_ cap cap' on -> on /= (cap && not cap')) capturePress =<< delay False capturePress
    
    {-
    [clearWaypoints, setWaypoint, stopPlayback, startPlayback, incPlaybackSpeed, decPlaybackSpeed] <-
        forM (zip [edge, edge, edge, edge, return, return] [0..]) $ \(process, i) -> process (fmap (!! i) waypointPress)

    waypoints <- recordSignalSamples setWaypoint clearWaypoints ((\(camPos, targetPos, _, _) -> (camPos, targetPos)) <$> controlledCamera)
    playbackSpeed <- transfer2 100 (\dt inc dec speed -> speed + 10*dt*(if inc then 1 else if dec then -1 else 0)) incPlaybackSpeed decPlaybackSpeed
    splineCamera <- playbackCamera startPlayback stopPlayback playbackSpeed waypoints
    let activeCamera = do
            camData <- splineCamera
            case camData of
                Nothing -> controlledCamera
                Just camData -> return camData
    -}
    let activeCamera = controlledCamera
        setupGFX (camPos,camTarget,camUp,brushIndex) time (capturing,frameCount) = do
            (w,h) <- getFramebufferSize win
            -- hack
            let keyIsPressed k = fmap (==KeyState'Pressed) $ getKey win k
            noBSPCull <- keyIsPressed (Key'X)
            debugRender <- keyIsPressed (Key'C)
            updateRenderInput graphicsData (camPos,camTarget,camUp) w h time noBSPCull
            {-
            when (not $ null brushIndex) $ do
              putStrLn $ "brush collision: " ++ show (map (getModelIndexFromBrushIndex levelData) brushIndex)
            -}
-- #ifdef CAPTURE
--             let  captureA = do
--                   when capturing $ do
--                       glFinish
--                       withFrameBuffer 0 0 w h $ \p -> writeImageFromPtr (printf "frame%08d.jpg" frameCount) (h,w) p
--                   writeIORef capRef capturing
-- #endif
--                  return ()
--            return (captureA,debugRender)
    r <- effectful3 setupGFX activeCamera time ((,) <$> capture <*> frameCount)
    return r

readInput compileRequest compileReady pplName rendererRef storage win s mousePos fblrPress capturePress waypointPress capRef = do
    let keyIsPressed k = fmap (==KeyState'Pressed) $ getKey win k
    t <- maybe 0 id <$> getTime
    setTime 0

    (x,y) <- getCursorPos win
    mousePos (realToFrac x,realToFrac y)

    fblrPress =<< ((,,,,,) <$> keyIsPressed Key'A <*> keyIsPressed Key'W <*> keyIsPressed Key'S <*> keyIsPressed Key'D
                           <*> keyIsPressed Key'RightShift <*> keyIsPressed Key'Space)
    capturePress =<< keyIsPressed Key'P
    waypointPress =<< mapM keyIsPressed [Key'R,Key'E,Key'1,Key'2,Key'F,Key'G]

    isCapturing <- readIORef capRef
    let dt = if isCapturing then recip captureRate else realToFrac t

    updateFPS s dt

    reload <- keyIsPressed Key'L
    when reload $ writeIORef compileRequest True
    readIORef compileReady >>= \case
      False -> return ()
      True -> do
        writeIORef compileReady False
        loadQuake3Graphics storage pplName >>= \case
          Nothing -> return ()
          Just a  -> do
            readIORef rendererRef >>= disposeRenderer
            writeIORef rendererRef a
    k <- keyIsPressed Key'Escape
    return $ if k then Nothing else Just (min 0.1 $ realToFrac dt) -- simulation must run at least 10 FPS, under 10 FPS won't be realtime

-- -- 
-- main :: IO ()
-- main = withGLFWWindow windowWidth windowHeight "05-GraphicsPipeline-Window"
--           $ \window ->
--        withGLFWVulkanInstance "05-GraphicsPipeline" $ \vulkanInstance ->
--        withSurface vulkanInstance window $ \vulkanSurface -> do
--         (Just scsd, pdev)
--           <- pickPhysicalDevice vulkanInstance (Just vulkanSurface)
--         withGraphicsDevice pdev vulkanSurface $ \dev queues ->
--           withSwapChain dev scsd queues vulkanSurface $ \swInfo ->
--           withImageViews dev swInfo $ \imgViews ->
--           withVkShaderStageCI dev
--               $(compileGLSL "shaders/triangle.vert")
--               VK_SHADER_STAGE_VERTEX_BIT
--               $ \shaderVert ->
--           withVkShaderStageCI dev
--               $(compileGLSL "shaders/triangle.frag")
--               VK_SHADER_STAGE_FRAGMENT_BIT
--               $ \shaderFrag ->
--           withRenderPass dev swInfo $ \renderPass ->
--           withGraphicsPipeline dev swInfo [shaderVert, shaderFrag] renderPass
--               $ \graphicsPipeline ->
--           withFramebuffers dev renderPass swInfo imgViews
--               $ \framebuffers ->
--           withCommandPool dev queues $ \commandPool ->
--           withCommandBuffers dev graphicsPipeline commandPool
--                              renderPass swInfo framebuffers
--               $ \cmdBuffers ->
--           withSemaphore dev $ \rendFinS ->
--           withSemaphore dev $ \imAvailS ->
--           alloca $ \imgIPtr -> do
--             let rdata = RenderData
--                   { renderFinished = rendFinS
--                   , imageAvailable = imAvailS
--                   , device         = dev
--                   , swapChainInfo  = swInfo
--                   , deviceQueues   = queues
--                   , imgIndexPtr    = imgIPtr
--                   , commandBuffers = cmdBuffers
--                   }
--             putStrLn $ "Selected physical device: " ++ show pdev
--             putStrLn $ "Createad surface: " ++ show vulkanSurface
--             putStrLn $ "Createad device: " ++ show dev
--             putStrLn $ "Createad queues: " ++ show queues
--             putStrLn $ "Createad swapchain: " ++ show swInfo
--             putStrLn $ "Createad image views: " ++ show imgViews
--             putStrLn $ "Createad vertex shader module: " ++ show shaderVert
--             putStrLn $ "Createad fragment shader module: " ++ show shaderFrag
--             putStrLn $ "Createad renderpass: " ++ show renderPass
--             putStrLn $ "Createad pipeline: " ++ show graphicsPipeline
--             putStrLn $ "Createad framebuffers: " ++ show framebuffers
--             putStrLn $ "Createad command pool: " ++ show commandPool
--             putStrLn $ "Createad command buffers: " ++ show cmdBuffers
--             glfwMainLoop window $ do
--               return () -- do some app logic
--               throwingVK "vkQueueWaitIdle failed!"
--                 $ vkQueueWaitIdle . presentQueue $ deviceQueues rdata
--               drawFrame rdata
--             throwingVK "vkDeviceWaitIdle failed!"
--               $ vkDeviceWaitIdle dev

-- -- Handle Events
-- appLoop :: Renderer -> IO ()
-- appLoop renderer = do
--     events <- pollEvents
--     let eventIsQPress event =
--             case eventPayload event of
--             KeyboardEvent keyboardEvent -> keyboardEventKeyMotion keyboardEvent == Pressed && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
--             _ -> False
--         qPressed = any eventIsQPress events
--     rendererDrawColor renderer $= V4 100 0 255 255
--     -- renderPrimitive  renderer $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
--     -- SDL.glSwapWindow window
--     clear   renderer
--     present renderer
--     unless qPressed (appLoop renderer)

-- createSDLWindow :: IO ()
-- createSDLWindow = do
--     initializeAll
--     window   <- createWindow   "WRL3D (SDL2)" glWindowConfig -- vkWindowConfig --defaultWindow
--     renderer <- createRenderer window (-1)    defaultRenderer
--     icon     <- SDL.loadBMP "./assets/WRL3D.bmp"
--     setWindowIcon window icon
--     appLoop renderer
--     destroyWindow window
--     -- glSwapWindow window
--     quit

