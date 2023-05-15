module HRayLib3d.Utils.Md3Show where

    import Control.Monad
    import Text.Show.Pretty
    import System.Environment
    import HRayLib3d.GameEngine.Loader.MD3

    md3Show :: IO ()
    md3Show = getArgs >>= mapM_ (loadMD3 >=> pPrint)
