module LambdaCube.XR.FrameLoop where

    import OpenXR
    import OpenXR.Spaces            (xrLocateSpace)
    import OpenXR.Rendering         (xrAcquireSwapchainImage, xrWaitSwapchainImage, xrWaitFrame, xrBeginFrame, xrEndFrame, xrLocateViews)
    import OpenXR.InputActions      (xrSyncActions, xrApplyHapticFeedback, xrStopHapticFeedback, xrGetActionStateFloat, xrGetActionStateBoolean, xrGetActionStateVector2f, xrGetActionStatePose)
    import OpenXR.InstanceLifecycle (xrRequestExitSession)

    -- frameLoop :: XrSwapchain -> XrSession -> XrResult
    -- frameLoop swapchain session = do
    --     --not focused
    --     xrSyncActions            session syncInfo
    --     xrGetActionStateBoolean  session getInfo   state
    --     xrGetActionStateFloat    session getInfo   state
    --     xrGetActionStateVector2f session getInfo   state
    --     xrGetActionStatePose     session getInfo   state
    --     xrLocateSpace            space   baseSpace time  location

    --     xrApplyHapticFeedback    session hapticActionInfo hapticFeedback 
    --     xrStopHapticFeedback     session hapticActionInfo
    --     xrRequestExitSession     session

    --     -- XR_SESSION_STATE SYNCHRONIZED | VISIBLE | FOCUSED
    --     xrWaitFrame  session frameWaitInfo frameState
    --     xrBeginFrame session frameWaitInfo

    --     --not visible
    --     xrAcquireSwapchainImage swapchain acquireInfo index 
    --     xrWaitSwapchainImage    swapchain waitInfo
    --     xrLocateViews           session   viewLocateInfo viewState viewCapacityInput viewCountOutput views 
    --     xrLocateSpace           space baseSpace time location

    --     --Execute Graphics Work
    --     xrReleaseSwapchainImage swapchain releaseInfo -- XrSwapchain -> XrSwapchainImageReleaseInfo
    --     xrEndFrame              session   frameEndInfo

-- Compare to examples from examples/
-- where loop = do
--     -- update graphics input
--     GLFW.getWindowSize win >>= \(w,h) -> LambdaCubeGL.setScreenSize storage (fromIntegral w) (fromIntegral h)
--     LambdaCubeGL.updateUniforms storage $ do
--         "time" @= do
--                     Just t <- GLFW.getTime
--                     return (realToFrac t :: Float)
--     -- render
--     LambdaCubeGL.renderFrame renderer
--     GLFW.swapBuffers win
--     GLFW.pollEvents

--     let keyIsPressed k = fmap (==KeyState'Pressed) $ GLFW.getKey win k
--     escape <- keyIsPressed Key'Escape
--     if escape then return () else loop