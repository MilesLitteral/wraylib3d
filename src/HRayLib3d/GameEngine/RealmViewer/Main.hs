{-# LANGUAGE PackageImports, CPP, LambdaCase, TupleSections, RecordWildCards #-}

module HRayLib3d.GameEngine.RealmViewer.Main where 
  
import Data.IORef
import Data.Maybe
import Data.Char (toLower)
import qualified Data.Map as Map
import Control.Monad
import Control.Lens
import Control.Concurrent

import System.IO
import System.Exit
import System.FilePath
import System.Directory
import System.Environment
import qualified System.Mem

import FRP.Elerea.Param
import Sound.ProteaAudio
import Graphics.GL.Core33
import Graphics.UI.GLFW as GLFW

import LambdaCube.GL as GL
import HRayLib3d.GameEngine.Loader.Zip
import HRayLib3d.GameEngine.RealmViewer.Camera
import HRayLib3d.GameEngine.RealmViewer.Engine
import qualified Data.ByteString.Char8 as SB8

import Monomer
import qualified Monomer.Lens as L

#ifdef CAPTURE
import Codec.Image.DevIL
import Text.Printf
import Foreign
#endif

type Sink a = a -> IO ()

#ifdef CAPTURE
-- framebuffer capture function
withFrameBuffer :: Int -> Int -> Int -> Int -> (Ptr Word8 -> IO ()) -> IO ()
withFrameBuffer x y w h fn = allocaBytes (w*h*4) $ \p -> do
    glReadPixels (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) GL_RGBA GL_UNSIGNED_BYTE $ castPtr p
    fn p
#endif

captureRate :: Double
captureRate = 30

main :: IO ()
main = run

run :: IO ()
run = do
    hSetBuffering stdout NoBuffering
    --hSetBuffering stdin NoBuffering
#ifdef CAPTURE
    ilInit
#endif

    noPak0_pk3 <- null . filter (\n -> "pak0.pk3" == map toLower n) <$> getDirectoryContents "."
    when noPak0_pk3 $ die "Could not find pak0.pk3. See how to run: https://github.com/lambdacube3d/lambdacube-quake3/blob/master/README.md"

    pk3Data <- loadPK3
    args    <- getArgs
    let bspNames = [n | n <- Map.keys pk3Data, ".bsp" == takeExtension n]
    fullBSPName <- head <$> case args of
      (n:xs) -> return $ filter ((== n) . takeBaseName) bspNames
      _ -> do
            let maps = map takeBaseName bspNames
            putStrLn $ "Available maps:"
            putStrLn $ unwords maps
            putStrLn "Enter map name:"
            name <- getLine
            return $ filter ((name ==) . takeBaseName) bspNames
    let bspName = takeBaseName fullBSPName

    win <- initWindow "LC DSL Quake 3 Demo" 800 600

    -- loading screen, check this block for bugs as something its doing is segfaulting the Ruby Interpreter
    loadingScreen <- createLoadingScreen
    (w,h)         <- getFramebufferSize win
    drawLoadingScreen w h loadingScreen pk3Data bspName
    swapBuffers win
    pollEvents

    -- _ <- initAudio 64 44100 1024 -- BUG FOUND! The Audio Allocation is leading to a segfault in the Ruby Interpreter

    (inputSchema, levelData) <- engineInit pk3Data fullBSPName

    -- compile graphics pipeline
    let pplName = bspName ++ "_ppl.json"
    compileRequest <- newIORef False
    compileReady   <- newIORef False
    _ <- forkIO $ forever $ do -- start compile thread
      putStrLn "start to compile"
      writeIORef compileRequest False
      writeIORef compileReady   False
      compileQuake3GraphicsCached pplName >>= writeIORef compileReady
      putStrLn "compile finished"
      let loop = do
            req <- readIORef compileRequest
            threadDelay 100000 -- 10 / sec
            unless req loop
      loop

    -- upload graphics data to GPU
    storage <- allocStorage inputSchema
    graphicsData <- setupStorage pk3Data levelData storage
    putStrLn "storage created"

    simpleRenderer <- fromJust <$> loadQuake3Graphics storage "SimpleGraphics.json"
    setStorage simpleRenderer storage
    rendererRef <- newIORef =<< fromJust <$> loadQuake3Graphics storage "SimpleGraphics.json"

    -- play level music
    case getMusicFile levelData of
      Nothing -> return ()
      Just musicFName' -> let musicFName = map f musicFName'
                              f '\\' = '/'
                              f c = c
                          in case Map.lookup musicFName pk3Data of
        Nothing -> return ()
        Just e  -> do
          buf  <- readEntry e
          -- load from memory buffer
          smp' <- case takeExtension musicFName of
           ".ogg" -> sampleFromMemoryOgg buf 1
           ".wav" -> sampleFromMemoryWav buf 1
          soundPlay smp' 1 1 0 1
          pure ()

    (mousePosition,mousePositionSink) <- unsafeExternal (0,0)
    (fblrPress,fblrPressSink)         <- unsafeExternal (False,False,False,False,False,False)
    (capturePress,capturePressSink)   <- unsafeExternal False
    (waypointPress,waypointPressSink) <- unsafeExternal []

    putStrLn "script engine initialized"

    let draw (captureA,debugRender) = do
          if debugRender
            then renderFrame simpleRenderer
            else readIORef rendererRef >>= renderFrame
          captureA
          swapBuffers win
          System.Mem.performMinorGC
          pollEvents

        cleanupResources = do
          -- render the first frame to force resource loading
          renderFrame simpleRenderer
          readIORef rendererRef >>= renderFrame
          -- cleanup dead data
          --System.Mem.performGC


    capRef <- newIORef False
    sc     <- start $ do
        u  <- scene win levelData graphicsData mousePosition fblrPress capturePress waypointPress capRef
        return (draw <$> u)
    s <- fpsState

    -- finish up resource loading
    cleanupResources

    setTime 0
    driveNetwork sc (readInput compileRequest compileReady pplName rendererRef storage win s mousePositionSink fblrPressSink capturePressSink waypointPressSink capRef)

    disposeRenderer =<< readIORef rendererRef
    putStrLn "storage destroyed"

    finishAudio
    destroyWindow win

{-
doInScissor :: Size -> Double -> Point -> Rect -> IO () -> IO ()
doInScissor winSize dpr offset vp action = do

  -- OpenGL's Y axis increases from bottom to top
  glScissor (round (rx + ox)) (round $ winH - ry - oy - rh) (round rw) (round rh)
  action

-}

runAsWidget :: Size -> Double -> Point -> Rect -> IO ()
runAsWidget winSize dpr offset vp = do
  glEnable GL_SCISSOR_TEST
  hSetBuffering stdout NoBuffering
  --hSetBuffering stdin NoBuffering
#ifdef CAPTURE
  ilInit
#endif
  -- Load Assets, implement Asset Bundle Unpacking here
  noPak0_pk3 <- null . filter (\n -> "pak0.pk3" == map toLower n) <$> getDirectoryContents "."
  when noPak0_pk3 $ die "Could not find pak0.pk3. See how to run: https://github.com/lambdacube3d/lambdacube-quake3/blob/master/README.md"

  pk3Data <- loadPK3
  loadAssetBundle
  args    <- getArgs
  let bspNames = [n | n <- Map.keys pk3Data, ".bsp" == takeExtension n]
  fullBSPName <- head <$> case args of
    (n:xs) -> return $ filter ((== n) . takeBaseName) bspNames
    _ -> do
          let maps = map takeBaseName bspNames
          putStrLn $ "Available maps:"
          putStrLn $ unwords maps
          putStrLn "Enter map name:"
          name <- getLine
          return $ filter ((name ==) . takeBaseName) bspNames
  let bspName = takeBaseName fullBSPName

  win <- initWindow "LC DSL Quake 3 Demo" 800 600

  -- loading screen, check this block for bugs as something its doing is segfaulting the Ruby Interpreter
  loadingScreen <- createLoadingScreen
  (w,h)         <- getFramebufferSize win
  drawLoadingScreen w h loadingScreen pk3Data bspName
  swapBuffers win
  pollEvents

  _ <- initAudio 64 44100 1024

  -- Initialize Engine
  (inputSchema,levelData) <- engineInit pk3Data fullBSPName

  -- compile graphics pipeline
  let pplName = bspName ++ "_ppl.json"
  compileRequest <- newIORef False
  compileReady   <- newIORef False
  _ <- forkIO $ forever $ do -- start compile thread
    putStrLn "start to compile"
    writeIORef compileRequest False
    writeIORef compileReady   False
    compileQuake3GraphicsCached pplName >>= writeIORef compileReady
    putStrLn "compile finished"
    let loop = do
          req <- readIORef compileRequest
          threadDelay 100000 -- 10 / sec
          unless req loop
    loop

  -- upload graphics data to GPU
  storage <- allocStorage inputSchema
  graphicsData <- setupStorage pk3Data levelData storage
  putStrLn "storage created"

  simpleRenderer <- fromJust <$> loadQuake3Graphics storage "SimpleGraphics.json"
  _              <- setStorage simpleRenderer storage --Storage can potentially be manipulated?
  rendererRef    <- newIORef . fromJust =<< loadQuake3Graphics storage "SimpleGraphics.json"

  -- play level music
  case getMusicFile levelData of
    Nothing -> return ()
    Just musicFName' -> let musicFName = map f musicFName'
                            f '\\' = '/'
                            f c = c
                        in case Map.lookup musicFName pk3Data of
      Nothing -> return ()
      Just e  -> do
        buf  <- readEntry e
        -- load from memory buffer
        smp' <- case takeExtension musicFName of
         ".ogg" -> sampleFromMemoryOgg buf 1
         ".wav" -> sampleFromMemoryWav buf 1
         _      -> error "Unsupported Format"
        _ <- soundPlay smp' 1 1 0 1
        pure ()

  (mousePosition,mousePositionSink) <- unsafeExternal (0,0)
  (fblrPress,fblrPressSink)         <- unsafeExternal (False,False,False,False,False,False)
  (capturePress,capturePressSink)   <- unsafeExternal False
  (waypointPress,waypointPressSink) <- unsafeExternal []

  glScissor (round (rx + ox)) (round $ winH - ry - oy - rh) (round rw) (round rh)
  let draw (captureA, debugRender) = do
        if debugRender
          then renderFrame simpleRenderer
          else readIORef rendererRef >>= renderFrame
        _ <- captureA
        swapBuffers win
        System.Mem.performMinorGC
        pollEvents

      cleanupResources = do
        -- render the first frame to force resource loading
        renderFrame simpleRenderer
        readIORef rendererRef >>= renderFrame
        -- cleanup dead data
        System.Mem.performGC


  capRef <- newIORef False
  sc     <- start $ do
      u  <- scene win levelData graphicsData mousePosition fblrPress capturePress waypointPress capRef
      return $ (draw <$> u)
  s <- fpsState

  -- finish up resource loading
  cleanupResources

  setTime 0
  driveNetwork sc (readInput compileRequest compileReady pplName rendererRef storage win s mousePositionSink fblrPressSink capturePressSink waypointPressSink capRef)

  disposeRenderer =<< readIORef rendererRef
  putStrLn "storage destroyed"

  finishAudio
  -- destroyWindow win
  glDisable GL_SCISSOR_TEST
  where
    winH = winSize ^. L.h * dpr
    Point ox oy = mulPoint dpr offset
    Rect rx ry rw rh = mulRect dpr vp

edge :: Signal Bool -> SignalGen p (Signal Bool)
edge s = transfer2 False (\_ cur prev _ -> cur && not prev) s =<< delay False s

upEdge :: Signal Bool -> SignalGen p (Signal Bool)
upEdge s = transfer2 False (\_ cur prev _ -> cur && prev == False) s =<< delay False s

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
            let  captureA = do
#ifdef CAPTURE
                  when capturing $ do
                      glFinish
                      withFrameBuffer 0 0 w h $ \p -> writeImageFromPtr (printf "frame%08d.jpg" frameCount) (h,w) p
                  writeIORef capRef capturing
#endif
                  return ()
            return (captureA,debugRender)
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

-- FRP boilerplate
driveNetwork :: (p -> IO (IO a)) -> IO (Maybe p) -> IO ()
driveNetwork network driver = do
    dt <- driver
    case dt of
        Just dt -> do
            join $ network dt
            driveNetwork network driver
        Nothing -> return ()

-- OpenGL/GLFW boilerplate
initWindow :: String -> Int -> Int -> IO Window
initWindow title width height = do
    GLFW.init
    defaultWindowHints
    mapM_ windowHint
      [ WindowHint'ContextVersionMajor 3
      , WindowHint'ContextVersionMinor 3
      , WindowHint'OpenGLProfile OpenGLProfile'Core
      , WindowHint'OpenGLForwardCompat True
      ]
    Just win <- createWindow width height title Nothing Nothing
    makeContextCurrent $ Just win
    glEnable GL_FRAMEBUFFER_SRGB
    swapInterval 0
    return win

-- FPS tracking
data State = State { frames :: IORef Int, t0 :: IORef Double }

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