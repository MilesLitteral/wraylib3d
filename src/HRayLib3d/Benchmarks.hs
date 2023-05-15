module HRayLib3d.Benchmarks (
  mapViewerBenchmark
  , gameBenchmark
  , customBenchmarkIO
  , customBenchmarkF
  ) where

import Criterion.Main
import HRayLib3d.GameEngine.Realm.Main
import HRayLib3d.GameEngine.RealmViewer.Main

-- Our benchmark harness.
mapViewerBenchmark :: IO ()
mapViewerBenchmark = defaultMain [
  bgroup "MapViewer.Main" [ bench "Run"  $ whnfIO  HRayLib3d.GameEngine.RealmViewer.Main.run]
  ]

-- Game and Realm refer to the same thing in engine
gameBenchmark :: IO ()
gameBenchmark = defaultMain [
  bgroup "GameEngine.Realm.Main" [ bench "Main"  $ whnfIO HRayLib3d.GameEngine.Realm.Main.main]
  ]
  
customBenchmarkIO :: String -> String -> IO () -> IO ()
customBenchmarkIO title benchName function = defaultMain [
  bgroup title [ bench benchName  $ whnfIO function]
  ]

customBenchmarkF  :: String -> String -> (a0 -> b0) -> a0 -> IO ()
customBenchmarkF title benchName function arg = defaultMain [
  bgroup title [ bench benchName  $ whnf function arg]
  ]

-- The function we're benchmarking.
-- fib m | m < 0     = error "negative!"
--       | otherwise = go m
--   where
--     go 0 = 0
--     go 1 = 1
--     go n = go (n-1) + go (n-2)

-- -- Our benchmark harness.
-- mainBenchmark :: IO ()
-- mainBenchmark = defaultMain [
--   bgroup "fib" [ bench "1"  $ whnf fib 1
--                , bench "5"  $ whnf fib 5
--                , bench "9"  $ whnf fib 9
--                , bench "11" $ whnf fib 11
--                ]
--   ]
