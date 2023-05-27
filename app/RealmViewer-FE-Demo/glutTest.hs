module GLUTTest where
    
    import Data.IORef
    import Graphics.UI.GLUT

    display :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> DisplayCallback
    display angle pos = do 
        clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
        clear [ColorBuffer]
        loadIdentity
        (x',y') <- get pos
        translate $ Vector3 x' y' 0
        preservingMatrix $ do
            a <- get angle
            rotate a $ Vector3 0 0 1
            rotate a $ Vector3 0 0.1 1 -- changed y-component a bit to show off cube corners
            scale 0.7 0.7 (0.7::GLfloat)
            forM_ (points 7) $ \(x,y,z) -> preservingMatrix $ do
            color $ Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2)
            translate $ Vector3 x y z
            cube 0.1
            color $ Color3 (0::GLfloat) 0 0 -- set outline color to black
            cubeFrame 0.1 -- draw the outline
        swapBuffers

    idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
    idle angle delta = do
    d <- get delta
    angle $~! (+ d)
    postRedisplay Nothing

    reshape :: ReshapeCallback
    reshape size = do 
    viewport $= (Position 0 0, size)

    keyboardMouse :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
    keyboardMouse a p key Down _ _ = case key of
                                        (Char ' ') -> a $~! negate
                                        (Char '+') -> a $~! (* 2)
                                        (Char '-') -> a $~! (/ 2)
                                        (SpecialKey KeyLeft ) -> p $~! \(x,y) -> (x-0.1,y)
                                        (SpecialKey KeyRight) -> p $~! \(x,y) -> (x+0.1,y)
                                        (SpecialKey KeyUp   ) -> p $~! \(x,y) -> (x,y+0.1)
                                        (SpecialKey KeyDown ) -> p $~! \(x,y) -> (x,y-0.1)
                                        _ -> return ()
                                        keyboardMouse _ _ _ _ _ _ = return ()

    cubeFrame :: GLfloat -> IO ()
    cubeFrame w = renderPrimitive Lines $ mapM_ vertex3f
    [ ( w,-w, w), ( w, w, w),  ( w, w, w), (-w, w, w),
        (-w, w, w), (-w,-w, w),  (-w,-w, w), ( w,-w, w),
        ( w,-w, w), ( w,-w,-w),  ( w, w, w), ( w, w,-w),
        (-w, w, w), (-w, w,-w),  (-w,-w, w), (-w,-w,-w),
        ( w,-w,-w), ( w, w,-w),  ( w, w,-w), (-w, w,-w),
        (-w, w,-w), (-w,-w,-w),  (-w,-w,-w), ( w,-w,-w) ]

    main :: IO ()
    main = do
        (_progName, _args) <- getArgsAndInitialize
        initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
        _window <- createWindow "Hello World"
        reshapeCallback $= Just reshape
        depthFunc $= Just Less -- the comparison function for depth the buffer
        angle <- newIORef 0
        delta <- newIORef 0.1
        pos <- newIORef (0, 0)
        keyboardMouseCallback $= Just (keyboardMouse delta pos)
        idleCallback $= Just (idle angle delta)
        displayCallback $= display angle pos
        mainLoop