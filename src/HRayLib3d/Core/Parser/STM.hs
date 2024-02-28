module HRayLib3d.Core.Parser.STM (parseScriptSTM) where

-- Compile with -threaded
import System.IO ()
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM ( atomically, retry, STM, TVar, newTVarIO, readTVar, writeTVar )
import qualified Text.XML.Light.Input  as XML
import qualified Text.XML.Light.Lexer  as XML
import qualified Text.XML.Light.Types  as XML

-- ([Sum of Loaded XML], <Number of finished transactions>)
type Result = TVar ([XML.Content], Int)

-- Adds x to result and increments the number of finished transactions
addContentToResult :: Result -> XML.Content -> STM ()
addContentToResult result x = do
  (resContent, endCtr) <- readTVar result
  writeTVar result (resContent <> [x], endCtr+1)

-- Waits for the number of finished transactions to reach a limit
-- Then returns the sum of the result
waitForParse :: Result -> Int -> STM [XML.Content]
waitForParse result limit = do
  (resContent, endCtr) <- readTVar result
  if endCtr < limit
    then retry
    else return resContent

parseScriptSTM :: XML.XmlSource s => s -> IO ()
parseScriptSTM source = do
  let n  = 100 :: Int                                       -- Number of threads to spawn    
  result <- newTVarIO ([], 0)                               -- Set up TVar
  mapM_ (forkIO . atomically . addContentToResult result) $ XML.parseXML source--[1..n]   -- Spawn threads
  resSum <- atomically $ waitForParse result n              -- Wait for threads to finish and get sum
  putStrLn $ "Sum [1..n] = " ++ show resSum                 -- Print sum