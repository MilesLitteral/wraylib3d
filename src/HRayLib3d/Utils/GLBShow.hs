module HRayLib3d.Utils.GLBShow where
    
    import Control.Monad
    import Text.Show.Pretty
    import System.Environment
    import HRayLib3d.GameEngine.Loader.GLB

    glbShow :: IO ()
    glbShow = getArgs >>= mapM_ (loadGLB >=> pPrint)
