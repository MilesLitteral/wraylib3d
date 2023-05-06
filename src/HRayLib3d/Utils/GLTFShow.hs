module HRayLib3d.Utils.GLTFShow where

    import Control.Monad
    import Text.Show.Pretty
    import System.Environment
    import HRayLib3d.GameEngine.Loader.GlTF

    GLTFShow = getArgs >>= mapM_ (loadGLTF >=> pPrint)
