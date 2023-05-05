module HRayLib3d.ScriptEngine.Functions (
  openRSTM 
  , runScriptServiceSTM
  , startRubyScriptService
  , endRubyScriptService
  , loadScriptComponentFile
  , toRubyForeign
  , fromRubyForeign 
) where

import GHC.Conc.Sync (unsafeIOToSTM) -- WARNING: This is only for testing STM functions, and even then, to experiment
import Control.Concurrent.STM.TMVar (TMVar(..))
import qualified Control.Concurrent.STM    as T

import Data.Text 
import HRayLib3d.ScriptEngine.Bindings
import HRayLib3d.ScriptEngine.Internal
import qualified Control.Monad             as CM
import qualified Control.Concurrent        as CC

toRubyForeign :: ForeignToRuby    a => RubyInterpreter      -> a         -> IO (Either RubyError FRValue)
toRubyForeign =  toRubyFFI

fromRubyForeign :: ForeignFromRuby a => RubyInterpreter     -> FRValue   -> IO (Either RubyError a)
fromRubyForeign =  fromRubyFFI

-- STM Version (Has Issues)
openRSTM :: ForeignToRuby   a => RubyInterpreter -> a -> RubyValueSTM
openRSTM irb value     = do 
    rb <- unsafeIOToSTM $ toRubyForeign irb value
    T.newTMVar (rb)
    -- T.putTMVar value rb
    -- T.takeTMVar 

runScriptServiceSTM :: Int -> IO ()
runScriptServiceSTM value = do
    irb        <- startRubyScriptService
    rbSTM      <- T.atomically  (openRSTM irb  value)
    output     <- T.atomically  (T.takeTMVar rbSTM)
    print $ rvalueOrError output

startRubyScriptService  :: IO RubyInterpreter
startRubyScriptService  = startRubyInterpreter 

endRubyScriptService    :: RubyInterpreter -> IO () 
endRubyScriptService rb = closeRubyInterpreter rb  

loadScriptComponentFile :: RubyInterpreter    -> FilePath   -> IO (Either RubyError ())
loadScriptComponentFile =  loadFile
