module Main (main) where

import HRayLib3d ()
import HRayLib3d.ScriptEngine
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS

main :: IO ()
main = do
    print "Hello World!"
    rb <- BS.fromFilePath "C:/Users/Manda/OneDrive/Desktop/hraylib3d/script-engine/hello.rb"
    runRubyInterpreter
    -- runScriptServiceSTM 10
    irb <- startRubyScriptService 
    -- loadScriptComponentFile irb "C:/Users/Manda/OneDrive/Desktop/hraylib3d/script-engine/simple.rb"
    endRubyScriptService    irb 
    print "Ruby Test"


