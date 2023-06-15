{-# LANGUAGE OverloadedStrings #-}
module Main (Main.main) where

import HRayLib3d
import HRayLib3d.Utils.Project
import HRayLib3d.Core.MobileInterface
import Data.Attoparsec.ByteString.Char8
import HRayLib3d.GameEngine.RealmViewer.Main
import qualified Data.ByteString as BS

-- Otherwise: defaultWindow
main :: IO ()
main = do
    HRayLib3d.GameEngine.RealmViewer.Main.main
    -- let d = DClass 1 2 3
    -- print d --"Hello World" 

    -- initializeAll
    -- window   <- createWindow   "WRL3D (SDL2)" glWindowConfig -- vkWindowConfig --defaultWindow
    -- renderer <- createRenderer window (-1)    defaultRenderer
    -- icon     <- SDL.loadBMP "./assets/WRL3D.bmp"
    -- setWindowIcon window icon
    -- appLoop renderer
    -- destroyWindow window
    -- -- glSwapWindow window
    -- quit

    -- rb <- BS.fromFilePath "C:/Users/Manda/OneDrive/Desktop/hraylib3d/script-engine/hello.rb"
    -- runRubyInterpreter
    -- runScriptServiceSTM 10
    -- irb <- startRubyScriptService 
    -- loadScriptComponentFile irb "C:/Users/Manda/OneDrive/Desktop/hraylib3d/script-engine/simple.rb"
    -- endRubyScriptService    irb 
    -- print "Ruby Test"

    -- print "Testing SDL2 Window"
    -- C.withSDL $ C.withWindow "WRL3D (SDL2)" (640, 480) $ \w -> do
    --   screen <- SDL.getWindowSurface w
    --   image  <- SDL.loadBMP "./assets/hello_world.bmp"
  
    --   C.renderSurfaceToWindow w screen image
  
    --   SDL.delay 2000
    --   SDL.freeSurface image
    --   SDL.freeSurface screen
