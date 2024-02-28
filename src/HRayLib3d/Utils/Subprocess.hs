{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module HRayLib3d.Utils.Subprocess (
    append,
    appendLens,
    appendHandles,
    appendIO,
    startLocalManiapi,
    runManiex,
    rebootSystemManiapi,
    startManieye,
    startManigram,
    startTensorBoard,
    startManigramControl,
    startIDE,
    configureManiex,
    installManiex,
    uninstallManiex
)
where

import Data.Text (Text, unpack, pack)
import Control.Lens
import Control.Exception
import Control.Monad.IO.Class
import GHC.IO.Handle (hGetLine)

import System.Environment
import System.Directory (getHomeDirectory)
import System.Process (StdStream(CreatePipe), CreateProcess, createProcess, readProcess, proc, cwd, env, std_in, std_out, std_err, shell, CmdSpec(RawCommand))

import HRayLib3d.Utils.LogMessage
import Monomer.Widgets.Composite

-- | Test if a Subprocess succeeds or fails on startup
-- This specific version will throw an IOException for 
-- the Subprocess (if it fails) which does not terminate 
-- the main window.
-- Example Use:
-- @
--   orderedMessage $ ManiLogMessage MANI_LOG_HEAD MANI_LOG_ZONE  ("Booting Maniex")
--   result <- try' $ (Manifest.Utils.Subprocess.startManiex mdl)
--   case result of
--     Left ex  -> orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ERROR ("Maniex: " ++ show ex)
--     Right () -> orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE  ("Maniex: CHECKLIST OK")
-- @
append :: a -> [a] -> [a]
append a [] = [a]
append a (x:xs) = x : append a xs

appendLens    model a lens    = return [Model $ model & lens %~  a]
appendHandles model a lens    = return [Model $ model & lens %~  a]
appendIO      model a lens    = return [Model $ model & lens .~  a]

-- | Boots the user specified IDE via console command ("atom ./",  "code ./")
-- NOTE: All 'start' functions appends a reference of the process' pipe to the
-- AppModel subprocess Lens so they can be closed all at once when the main 
-- window is, or be communicated to later
startIDE :: AppModel -> Text -> IO ()
startIDE mdl txt = do
    result <- try' $ createProcess (proc (unpack txt) ["./assets/projects/default"]){ cwd = Just $ (mdl ^. userHome) ++ "/Software/manimer", std_out = CreatePipe } --(_, Just hout, _, _) 
    orderedMessage $ LogMessage LOG_HEAD LOG_ZONE ("Booting IDE")
    case result of
        Left ex         -> orderedMessage $ LogMessage LOG_TAIL LOG_ERROR ("IDE: " ++ show ex)
        Right (_,_,_,p) -> do
            liftIO $ appendLens mdl (append p) (subprocesses)  
            orderedMessage $ LogMessage LOG_TAIL LOG_ZONE  ("IDE: CHECKLIST OK")
