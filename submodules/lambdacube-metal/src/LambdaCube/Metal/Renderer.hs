module LambdaCube.Metal.Renderer where 

setupConnection win = withSocketsDo $ WS.runClient "192.168.0.12" 9160 "/" $ \conn -> catchAll (execConnection win conn) $ \e -> do
  WS.sendTextData conn . encode $ RenderJobError $ displayException e

execConnection win conn = do
  putStrLn "Connected!"
  -- register backend
  WS.sendTextData conn . encode $ ClientInfo
    { clientName    = "Haskell DirectX 11"
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
