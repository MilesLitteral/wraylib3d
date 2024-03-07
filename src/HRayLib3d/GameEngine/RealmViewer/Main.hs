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

import LambdaCube.GL as GL -- # #ifdef <OS_Macro> and conditions will be your friend here when using the other rendering backends.
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

-- FRP boilerplate
driveNetwork :: (p -> IO (IO a)) -> IO (Maybe p) -> IO ()
driveNetwork network driver = do
    dt <- driver
    case dt of
        Just dt -> do
            join $ network dt
            driveNetwork network driver
        Nothing -> return ()

#ifdef CAPTURE
-- framebuffer capture function
withFrameBuffer :: Int -> Int -> Int -> Int -> (Ptr Word8 -> IO ()) -> IO ()
withFrameBuffer x y w h fn = allocaBytes (w*h*4) $ \p -> do
    glReadPixels (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) GL_RGBA GL_UNSIGNED_BYTE $ castPtr p
    fn p
#endif

captureRate :: Double
captureRate = 30

run :: IO ()
run = do
    hSetBuffering stdout NoBuffering
    --hSetBuffering stdin NoBuffering
#ifdef CAPTURE
    ilInit
#endif
    -- TODO: replace with pak0.assetBundle, that bundle is a copy of the .pk3
    noPak0_pk3 <- null . filter (\n -> "pak0.pk3" == map toLower n) <$> getDirectoryContents "."
    when noPak0_pk3 $ die "Could not find pak0.pk3. See how to run: https://github.com/lambdacube3d/lambdacube-quake3/blob/master/README.md"

    pk3Data <- loadPK3
    args    <- getArgs
    let bspNames = [n | n <- Map.keys pk3Data, ".bsp" == takeExtension n] -- load the .bsp from the .assetBundle instead
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

    -- loading screen
    loadingScreen <- createLoadingScreen
    (w,h)         <- getFramebufferSize win
    drawLoadingScreen w h loadingScreen pk3Data bspName
    swapBuffers win
    pollEvents

    _ <- initAudio 64 44100 1024

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
  glEnable GL_SCISSOR_TEST -- This must happen for it to render correctly as a Widget
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

  -- loading screen
  loadingScreen <- createLoadingScreen
  (w,h)         <- getFramebufferSize win
  drawLoadingScreen w h loadingScreen pk3Data bspName
  swapBuffers win
  pollEvents

  initAudio 64 44100 1024

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
        soundPlay smp' 1 1 0 1
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
        captureA
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
