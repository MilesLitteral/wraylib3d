{-# LANGUAGE RecordWildCards, OverloadedStrings, PackageImports #-}
module HRayLib3d.GameEngine.Realm.Main where

import GHC.Base (failIO)
import Control.Monad
import Control.Arrow
import Control.Monad.State.Strict
import System.Random.Mersenne.Pure64

import Lens.Micro.Platform
import Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Data.Binary (encode, decode)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BL 

import System.FilePath
import System.Directory
import System.Environment
import qualified System.Mem

import Data.Char
import Data.Map (Map)
import Data.List (find)
import Data.Vect.Float.Base hiding(Vector)
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as Map

import HRayLib3d.GameEngine.Utils (lc_q3_cache, clamp)
import HRayLib3d.GameEngine.Content (loadPK3)
import HRayLib3d.GameEngine.Data.BSP
import HRayLib3d.GameEngine.Loader.BSP
import HRayLib3d.GameEngine.Loader.Zip
import qualified HRayLib3d.GameEngine.Loader.Entity as E

import HRayLib3d.GameEngine.Realm.World
import HRayLib3d.GameEngine.Realm.GameLogic
import HRayLib3d.GameEngine.Realm.LoadEntities
import HRayLib3d.GameEngine.Realm.LoadResources
import HRayLib3d.GameEngine.Realm.RenderGame
import HRayLib3d.GameEngine.Realm.Entities
import HRayLib3d.GameEngine.RenderSystem as RenderSystem

import Debug.Trace
import Data.Maybe
import System.Directory
import System.Process

data Event
  = Event
  { ksMoveForward   :: !Bool
  , ksMoveBackward  :: !Bool
  , ksMoveRight     :: !Bool
  , ksMoveLeft      :: !Bool
  , ksSpace         :: !Bool
  , ksNumKey        :: !(Maybe Int)
  , ksHoldableKey   :: !(Maybe Int)
  , ksMousePosition :: !(Float,Float)
  }

inputFun :: Event -> World -> World
inputFun Event{..} w = w & wInput .~ i' where
  f True = 300
  f False = 0

  oldMU = mouseU i
  oldMV = mouseV i
  dx = newMouseX - mouseX i
  dy = newMouseY - mouseY i
  newMouseX = fst ksMousePosition
  newMouseY = snd ksMousePosition
  i = w^.wInput
  i' = i
    { forwardmove = f ksMoveForward - f ksMoveBackward
    , sidemove    = f ksMoveRight - f ksMoveLeft
    , shoot       = maybe False (==2) ksHoldableKey
    , mouseX      = newMouseX
    , mouseY      = newMouseY
    , mouseU      = oldMU  - dx / 100
    , mouseV      = clamp 0.1 3.1 $ oldMV + dy / 100
    , changeWeapon  = do { key <- ksNumKey; Map.lookup key weaponKeys }
    , toggleHoldable = do { key <- ksHoldableKey; Map.lookup key holdableKeys }
    , HRayLib3d.GameEngine.Realm.World.jump        = ksSpace
    }

mapTuple :: (a -> b) -> (a,a) -> (b,b)
mapTuple = join (***)

main :: IO ()
main = do
  (pk3, ents, mapfile, bsp) <- loadMap
  putStrLn $ "entity count: " ++ show (length ents)
  play pk3 (initWorld ents mapfile $ pureMT 123456789) renderFun inputFun (stepFun bsp) logPlayerChange

noLog :: p1 -> p2 -> Maybe a
noLog _ _ = Nothing

play :: Map String Entry
     -> World
     -> (RenderSettings -> WorldSnapshot -> Scene)
     -> (Event -> World -> World)
     -> (ResourceCache -> RenderSystem -> Float -> World -> World)
     -> (World -> World -> Maybe String)
     -> IO ()
play pk3 world0 getScene processInput stepWorld logWorldChange = do
  -- init graphics
  win <- initWindow "LambdaCube 3D Shooter" 1920 1080
  renderSystem <- initRenderSystem pk3
  loadResources renderSystem (worldResources world0 ++ hudResources) []

  let keyIsPressed k = fmap (==KeyState'Pressed) $ getKey win k

      mapKeyPressed keyAssoc = (fmap fst . find snd) <$> mapM (\(n, k) -> (,) n <$> keyIsPressed k) keyAssoc

      numKeyPressed = mapKeyPressed
        [ (0,Key'0), (1,Key'1), (2,Key'2), (3,Key'3), (4,Key'4), (5,Key'5), (6,Key'6)
        , (7,Key'7), (8,Key'8), (9,Key'9)
        ]

      holdableKeyPressed = mapKeyPressed
        [ (0, Key'Q), (1, Key'W), (2, Key'E), (3, Key'R), (4, Key'T) ]

      log oldWorld newWorld = maybe (pure ()) putStrLn $ logWorldChange oldWorld newWorld
      deltaTime = 1/60
      loop firstFrame oldFraction oldTime oldWorld = do
        -- process input
        pollEvents
        newTime <- maybe 0 realToFrac <$> getTime
        ks <- Event <$> keyIsPressed Key'W
                    <*> keyIsPressed Key'S
                    <*> keyIsPressed Key'D
                    <*> keyIsPressed Key'A
                    <*> keyIsPressed Key'Space
                    <*> numKeyPressed
                    <*> holdableKeyPressed
                    <*> (mapTuple realToFrac <$> getCursorPos win)
        quit <- keyIsPressed Key'Escape

        -- step simulation
        let frameTime = newTime - oldTime
            batchedTime = frameTime + oldFraction
        when (batchedTime > 10) $ putStrLn $ "WARNING: slow machine!"
        resourceCache <- RenderSystem.getResourceCache renderSystem
        let stepSimulation t w | t < deltaTime = (t,w)
                               | otherwise = stepSimulation (t - deltaTime) (stepWorld resourceCache renderSystem deltaTime w)
            (newFractionTime,newWorld) = stepSimulation batchedTime (processInput ks oldWorld)

        -- render current state
        (windowWidth, windowHeight) <- getFramebufferSize win
        let renderSettings = RenderSettings
              { windowWidth   = windowWidth
              , windowHeight  = windowHeight
              , sceneTime     = (newWorld ^. wInput . to time)
              , mapFile       = (newWorld ^. wMapFile)
              }
            worldSnapshot = WorldSnapshot
              { gameEntities = (newWorld ^. wEntities)
              }
            worldSnapshotData = encode worldSnapshot
            receivedWorldSnapshot = decode worldSnapshotData
        renderScene renderSystem newTime $ getScene renderSettings receivedWorldSnapshot
        swapBuffers win
        log oldWorld newWorld

        if firstFrame -- cleanup dead data
          then System.Mem.performGC
          else pure () -- System.Mem.performMinorGC

        unless quit $ loop False newFractionTime newTime newWorld
  time0 <- maybe 0 realToFrac <$> getTime
  loop True 0 time0 world0

  destroyWindow win

entitiesToJSON :: String -> String -> IO ()
entitiesToJSON inputS outputS = do
  cwrd <- getCurrentDirectory 
  _    <- createProcess (proc "py" [cwrd </> "read.py", inputS, outputS]){ std_out = CreatePipe }   {-(_, Just hout, _, _) <- -}
  return ()


loadMap :: IO
  (Map String Entry, [HRayLib3d.GameEngine.Realm.Entities.Entity],
   FilePath, BSPLevel)
loadMap = do
  -- init PK3 database
  noPak0_pk3 <- null . filter (\n -> "pak0.pk3" == map toLower n) <$> getDirectoryContents "."
  when noPak0_pk3 $ failIO "Could not find pak0.pk3. See how to run: https://github.com/lambdacube3d/lambdacube-quake3/blob/master/README.md"

  pk3Data <- loadPK3
  args <- getArgs
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
      bspEntry = case Map.lookup fullBSPName pk3Data of
        Nothing -> error $ "file not found: " ++ fullBSPName
        Just bspd -> bspd

  -- load bsp data
  bsp <- readBSP . LB.fromStrict <$> readEntry bspEntry

  createDirectoryIfMissing True lc_q3_cache -- create cache

  SB.writeFile (lc_q3_cache </> bspName ++ ".entities") $ blEntities bsp
  cwrd <- getCurrentDirectory 
  jsonExists <- doesFileExist (lc_q3_cache </> bspName ++ ".json")  

  let entitiesPath     = lc_q3_cache </> bspName ++ ".entities"
      jsonEntitiesPath = lc_q3_cache </> bspName ++ ".json" 
  createDirectoryIfMissing True lc_q3_cache -- create cache 
  if not jsonExists
    then entitiesToJSON (cwrd </> entitiesPath) (cwrd </> jsonEntitiesPath)
    else print $ "File Exists: " ++ bspName ++ ".json" 
  jsonEntities <- BL.readFile jsonEntitiesPath  --JSON.decodeFileStrict -- :: Maybe (E.EntityContainer)
  BL.writeFile (cwrd </> lc_q3_cache </> bspName ++ "_test.json" ) $ JSON.encode E.emptyEntityData  
  jsonETest    <- BL.readFile (cwrd  </> lc_q3_cache </> bspName ++ "_test.json" )  
  print (fromJust $ JSON.decode  "{\"classname \":\"any\", \"_color\":null,\"angles\":{\"x\":0,\"y\":0,\"z\":0}}\"" :: Maybe E.EntityData)
  let entities = fromJust $ JSON.decode jsonEntities :: E.EntityContainer
  -- extract spawn points
  -- let ents = case E.parseEntities bspName $ SB.unpack $ blEntities bsp of
  --         Left err -> error err
  --         Right x -> x
  return (pk3Data,loadEntities (E.ecData entities), fullBSPName, bsp)

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
