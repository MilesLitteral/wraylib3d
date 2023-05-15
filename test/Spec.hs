
{-# LANGUAGE OverloadedStrings #-}

import qualified Common as C
import qualified SDL

main :: IO ()
main = C.withSDL $ C.withWindow "SDL Test" (640, 480) $
  \w -> do
    screen <- SDL.getWindowSurface w
    image  <- SDL.loadBMP "./test-assets/hello_world.bmp"
    C.renderSurfaceToWindow w screen image

    SDL.delay 2000
    SDL.freeSurface image
    SDL.freeSurface screen

-- main :: IO ()
-- main = putStrLn "Test suite not yet implemented"
