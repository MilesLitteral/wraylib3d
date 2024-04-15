{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module HRayLib3d.ScriptEngine.HLua.Core where

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
    import Control.Monad.Trans.State.Lazy (StateT)

    import Control.Concurrent      (ThreadId, forkOS)
    import Control.Concurrent.Chan ( Chan, newChan, readChan, writeChan )
    import Control.Concurrent.MVar ( MVar, newMVar, putMVar, takeMVar )
    import Control.Concurrent.QSem (QSem, newQSem, waitQSem, signalQSem)

    -- import Foreign.Ruby.Safe ( loadFile,
    --   withRubyInterpreter,
    --   fromRuby,
    --   safeMethodCall,
    --   toRuby,
    --   freezeGC,
    --   RubyInterpreter 
    --   )
    -- import Foreign.Ruby.Helpers (ToRuby, FromRuby)

    import System.IO ()
    import System.Process
    import System.Directory
    import System.FilePath
    import System.Exit (exitFailure)

    import HRayLib3d.Utils.Exception 
    import HRayLib3d.ScriptEngine.HLua.HLuaHS

    import Test.Tasty
    import Test.Tasty.HUnit
    import HsLua 

    testCase :: String -> IO () -> IO ()
    testCase n io = putStrLn ("test " ++ n) >> io

    luaOpen :: String -> Lua () -> TestTree
    luaOpen lib openfn = Test.Tasty.HUnit.testCase ("open" ++ lib) $
        assertBool "opening the library failed" =<<
        HsLua.run (openfn *> istable (-1))

    luaRun :: Lua () -> IO ()
    luaRun prog = HsLua.run prog
        where
            prog :: Lua ()
            prog = do
                HsLua.openlibs  -- load Lua libraries so we can use 'print'
                HsLua.call (NumArgs 1) (NumResults 1) --callFunc/callFunction "print" "Hello, World!"



    readLuaFile :: FilePath -> IO String
    readLuaFile cont = do
        luaContents <- readFile cont
        return luaContents

    compileToLuaModule :: String -> IO ()
    compileToLuaModule gemName = do
        --install luabundler
        cwrd <- getCurrentDirectory 
        _    <- createProcess (proc "luabundler" ["bundle", "input.lua", "-p", ("cwrd </> build/?.lua"), "-o", "bundle.lua"]){ std_out = CreatePipe }   {-(_, Just hout, _, _) <- -}
        return ()

    compileToLuaHS :: ToLuaHS a => [a] -> [LuaValueHS]
    compileToLuaHS input = map toLuaHS input
