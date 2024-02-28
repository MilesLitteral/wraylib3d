module HRayLib3d.Utils.GLTFShow where

    import Control.Monad
    import Text.Show.Pretty
    import System.Environment
    import HRayLib3d.GameEngine.Loader.GlTF

    gltfShow :: IO ()
    gltfShow = getArgs >>= mapM_ (loadGLTF >=> pPrint)
