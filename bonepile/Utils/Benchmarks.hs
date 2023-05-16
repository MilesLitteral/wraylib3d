module Manifest.Utils.Benchmarks(
    defaultBenchmarkConfig,
    benchmarkAppWith,
    defaultBenchmark
) where

import Criterion
import Criterion.Types
import Criterion.Main 
import Criterion.Main.Options

import Data.String
import System.IO
import GHC.Num  (fromInteger)
import Data.Function (($))

-- | Manifest Benchmark Utilities
-- Default Benchmarking Configuration
-- You may edit resamples at will
defaultBenchmarkConfig :: Config
defaultBenchmarkConfig = defaultConfig {
              -- Resample 10 times for bootstrapping
              resamples = 10
           }

-- | throw a Manifest function at this to benchmark it's performance
benchmarkAppWith :: IO a -> IO ()
benchmarkAppWith entry = defaultMainWith defaultBenchmarkConfig [
         bench "manifest 10" $ whnfIO entry
       ]

 -- The Default Benchmark of Manifest Main
 -- generalized for use against any function
 -- in Manifold Ecosystem
defaultBenchmark :: IO a -> IO ()
defaultBenchmark entry = defaultMain [
       bgroup "main" [ bench "10" $ whnfIO entry
                     , bench "35" $ whnfIO entry
                     , bench "37" $ whnfIO entry
                    ]
                   ]