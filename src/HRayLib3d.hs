module HRayLib3d 
    ( module HRayLib3d.Core 
    , module HRayLib3d.Network
    , module HRayLib3d.ScriptEngine
    , module HRayLib3d.Benchmarks
    -- GameEngine, Game, MapViewer Go here
    , module HRayLib3d.Utils.Md3Show
    )
where

import HRayLib3d.Core
import HRayLib3d.Network
import HRayLib3d.Benchmarks
import HRayLib3d.ScriptEngine
import HRayLib3d.Utils.Md3Show


-- mainMVAR = do
--   m <- CC.newEmptyMVar
--   CC.forkIO $ CC.putMVar m $ sum [1..10000000]
--   print =<< CC.takeMVar m  -- takeMVar will block 'til m is non-empty!

-- main2MVAR = loop
--   where 
--     loop = do
--         m <- CC.newEmptyMVar
--         n <- getLine
--         putStrLn "Calculating. Please wait"
--         -- In another thread, parse the user input and sum
--         CC.forkIO $ CC.putMVar m $ sum [1..(read n :: Int)]
--         -- In another thread, wait 'til the sum's complete then print it
--         CC.forkIO $ print =<< CC.takeMVar m
--         loop
