{-# LANGUAGE FlexibleInstances #-}
module Manifest.Utils.Continuation where 

import Prelude
import System.IO
import System.Environment

import Control.Monad.Cont
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad.Identity (Identity)
import Control.Monad.Trans.State.Lazy

import Data.Function (($), flip)

executeAction :: (Floating a) => a -> (a -> r) -> r
executeAction a k = (k (a))

executeM :: (Monad m) => a -> (a -> r) -> m r
executeM a k = return $ k (a)

{-
executeJSON :: J.FromJSON a => a -> (a -> r) -> r
executeJSON a k = (k (a))

executeRequest :: J.FromJSON a => a -> (a -> r) -> Maybe (r)
executeRequest a k = Just (k (a))
-}

type MBinT m a = StateT m a
--type MBin      = StateT Identity
--type MBIO      = StateT IO

data Context = Context --(MVar Int) (ForeignPtr Raw.Futhark_context)

data BinaryHandle 
  = BinaryHandle {
      threadID :: ThreadId,
      logSem   :: MVar (Chan BinaryCommand) 
    } deriving(Eq, Show)

data BinaryCommand
  = BConnect     --{imgA    :: MP.Image MP.RGBA Float, imgB :: MP.Image MP.RGBA Float}
  | BDisconnect  --{imgResp :: MP.Image MP.RGBA Float}
  | BRefresh
  | BExit
  deriving (Eq, Show)

data BinaryOption
  = BinaryOptions [String]
  | Debug Int
  | Log Int
  | Device String
  | LoadProgram String
  | DumpProgram String
  | LoadPtx String
  | DumpPtx String
  | DefaultGroupSize Int
  | DefaultGroupNum Int
  | Size String Int

instance (Show (MVar (Chan BinaryCommand))) where 
  show x = "MVar (Chan BinaryCommand)"

getContext :: [BinaryOption] -> IO ()
getContext options = undefined --do
--     config <- Raw.context_config_new
--     mapM_ (setOption config) options
--     context <- Raw.context_new config
--     childCount <- newMVar 0
--     fmap (Context childCount)
--         $ FC.newForeignPtr context
--         $ (forkIO $ freeContext childCount config context)
--         >> return ()

-- createBinaryHandle :: IO (BinaryHandle)
-- createBinaryHandle = do
--     chan <- newChan
--     mvar <- newMVar chan
--     tid  <- forkOS $ binaryProcess chan
--     return $ BinaryHandle tid mvar

handleBinaryCommand :: BinaryCommand -> IO BinaryCommand
handleBinaryCommand command = do 
    case command of
        BConnect     -> undefined --
        BRefresh     -> undefined --B.get
        BDisconnect  -> undefined
        BExit        -> undefined

runBinaryCommand :: BinaryHandle -> BinaryCommand -> IO BinaryCommand 
runBinaryCommand fut gcommand = do
    commChan <- takeMVar $ logSem fut
    writeChan commChan gcommand
    result <- readChan commChan
    putMVar (logSem fut) commChan
    return result

-- binaryProcess :: Chan BinaryCommand -> IO ()
-- binaryProcess chan = do 
--     context <- getContext []
--     runStateT context $ do 
--       let {doBinaryProcess = do
--               command <- liftIO $ readChan chan
--               case command of
--                   BExit -> return () 
--                   _ -> do 
--                       result <- handleBinaryCommand command
--                       liftIO $ writeChan chan result --writeChan
--                       doBinaryProcess
--           }
--       doBinaryProcess

-- | An Interface for using the Continuation Monad Transformer
-- It is intended for use when Manifest has gone through a cycle
-- of IO actions and needs to return the results of those actions
-- to the Main Window. Think of it as a way of chaining tasks to
-- a finite terminus whereas producer continues indefinitely till
-- error and fails silently.
-- continue :: ContT r m a -> (a -> m r) -> m r
continue = flip runContT return $ do
  args <- liftIO getArgs
  handles <- forM args $ \arg ->
    ContT $ withFile arg ReadMode
  liftIO $ print_interleaved_from_handles handles

print_interleaved_from_handles :: [Handle] -> IO ()
print_interleaved_from_handles h = print (Prelude.show h)