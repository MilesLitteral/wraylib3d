module HRayLib3d.Utils.Benchmarks (
  mapViewerBenchmark
  , gameBenchmark
  , customBenchmarkIO
  , customBenchmarkF
  ) where

import Criterion.Main
import HRayLib3d.GameEngine.Realm.Main
import HRayLib3d.GameEngine.RealmViewer.Main

-- This module is used to benchmark functions. It's use is entirely internal
-- though in the future it will be desireable to use Custom Benchmarks with
-- user generated scripts

-- Our benchmark harness.
mapViewerBenchmark :: IO ()
mapViewerBenchmark = defaultMain [
  bgroup "MapViewer.Main" [ bench "Run"  $ whnfIO  HRayLib3d.GameEngine.RealmViewer.Main.run]
  ]

-- Game and Realm virtually refer to the same thing in engine
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
