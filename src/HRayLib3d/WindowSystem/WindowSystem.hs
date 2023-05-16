{-# LANGUAGE OverloadedStrings #-}
module HRayLib3d.Core.WindowSystem ({- createSDLWindow -}) where

    -- SDL2
    -- Channel Monomer here, It's going to be necessary xD
    -- Maybe even take some inspiration from it?
    -- How about forking and modifying it for WRayLib3d's purposes
    
    -- import SDL
    -- import SDL.Video
    -- import Linear        (V4(..))
    -- import Control.Monad (unless)

    -- -- defaultWindowConfig
    -- glWindowConfig :: WindowConfig
    -- glWindowConfig = WindowConfig
    --     { windowBorder          = True
    --     , windowHighDPI         = False
    --     , windowInputGrabbed    = False
    --     , windowMode            = Windowed
    --     , windowGraphicsContext = OpenGLContext defaultOpenGL 	
    --     , windowPosition        = Wherever
    --     , windowResizable       = False
    --     , windowInitialSize     = V2 800 600
    --     , windowVisible         = True
    --     }

    -- vkWindowConfig :: WindowConfig
    -- vkWindowConfig = WindowConfig
    --     { windowBorder          = True
    --     , windowHighDPI         = False
    --     , windowInputGrabbed    = False
    --     , windowMode            = Windowed
    --     , windowGraphicsContext = VulkanContext
    --     , windowPosition        = Wherever
    --     , windowResizable       = False
    --     , windowInitialSize     = V2 800 600
    --     , windowVisible         = True
    --     }

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