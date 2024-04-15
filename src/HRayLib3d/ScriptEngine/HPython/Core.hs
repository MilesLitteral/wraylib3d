{-# LANGUAGE FlexibleInstances #-}
module HRayLib3d.ScriptEngine.HPython.Core where

    import System.IO ()
    import GHC.Real  ()
    import GHC.Float (float2Int, int2Float)
    import Data.Int  (Int16)
    import Data.Word (Word8)
    import Data.Aeson (Value (Array, Bool, Number, String), object)
    import Data.Aeson.Key (fromText)
    import qualified Data.Text as T
    import qualified Data.Vector as V

    import Control.Lens            ()
    import Control.Monad (unless, when)
    import Control.Monad.Identity  ()
    import Control.Monad.IO.Class
    import HRayLib3d.Utils.Exception 
    import Control.Monad.Trans.State.Lazy (StateT)

    import Control.Concurrent      (ThreadId, forkOS)
    import Control.Concurrent.Chan ( Chan, newChan, readChan, writeChan )
    import Control.Concurrent.MVar ( MVar, newMVar, putMVar, takeMVar )
    import Control.Concurrent.QSem (QSem, newQSem, waitQSem, signalQSem)

    import Foreign.Ruby.Helpers (ToRuby, FromRuby)
    import System.Exit (exitFailure)
    import System.Process
    import System.Directory
    import System.FilePath
    import HRayLib3d.ScriptEngine.HPython.HPythonHS

    readPythonFile :: FilePath -> IO String
    readPythonFile cont = do
        pyContents <- readFile cont
        return pyContents
        
    compilePythonPackage :: String -> IO ()
    compilePythonPackage pyProj = do
        --ex: pyProj = "<filename>.pypi"
        cwrd <- getCurrentDirectory 
        _    <- createProcess (proc "python3" ["-m", "build"]){ cwd = Just ("./" </> pyProj), std_out = CreatePipe } -- Remember to Set Token
        return ()

    compileToPythonHS :: ToPythonHS a => [a] -> [PyValueHS]
    compileToPythonHS input = map toPythonHS input
