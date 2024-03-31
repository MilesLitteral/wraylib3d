{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings                    #-}
module HRayLib3d.Utils.Subprocess (
    append,
    appendLens,
    appendHandles,
    appendIO,
    startIDE,
)
where

import GHC.IO.Handle (hGetLine)
import Control.Exception ()
import Control.Monad.IO.Class ()
import Control.Lens ((^.), (&), (%~), (.~), ASetter )

import Data.Text          (Text, unpack, pack)
import System.Environment ()
import System.Directory   (getHomeDirectory)
import System.Process     (
    StdStream(CreatePipe),
    CreateProcess, 
    createProcess, 
    readProcess, 
    proc, 
    cwd, 
    env, 
    std_in, 
    std_out, 
    std_err, 
    shell, 
    CmdSpec(RawCommand)
 )

import Monomer.Widgets.Composite ( EventResponse(Model) )
import HRayLib3d.Utils.Exception
import HRayLib3d.Utils.LogMessage
import HRayLib3d.WindowSystem.Types

-- | Test if a Subprocess succeeds or fails on startup
-- This specific version will throw an IOException for 
-- the Subprocess (if it fails) which does not terminate 
-- the main window.
-- Example Use:
-- @
--   orderedMessage $ LogMessage LOG_HEAD LOG_ZONE  ("Booting libpq")
--   result <- try' $ (queryRealm mdl)
--   case result of
--     Left ex  -> orderedMessage $ ManiLogMessage LOG_TAIL LOG_ERROR ("libpq: " ++ show ex)
--     Right () -> orderedMessage $ ManiLogMessage LOG_TAIL LOG_ZONE  ("libpq: CHECKLIST OK")
-- @
append :: a -> [a] -> [a]
append a [] = [a]
append a (x:xs) = x : append a xs

appendLens :: Monad m    => a1 -> (a2 -> b) -> ASetter a1 s a2 b -> m [EventResponse s e sp ep]
appendLens    model a lens    = return [Model $ model & lens %~  a]

appendHandles :: Monad m => a1 -> (a2 -> b) -> ASetter a1 s a2 b -> m [EventResponse s e sp ep]
appendHandles model a lens    = return [Model $ model & lens %~  a]

appendIO :: Monad m => a1 -> b -> ASetter a1 s a2 b -> m [EventResponse s e sp ep]
appendIO      model a lens    = return [Model $ model & lens .~  a]

-- | Boots the user specified IDE via console command ("atom ./",  "code ./")
-- NOTE: All 'start' functions appends a reference of the process' pipe to the
-- AppModel subprocess Lens so they can be closed all at once when the main 
-- window is, or be communicated to later
startIDE :: IO () -- BooksModel -> Text -> 
startIDE  = do --mdl txt (unpack txt) ["./assets/projects/default"]
    result <- try' $ createProcess (shell "code"){ cwd = Just "./", std_out = CreatePipe } --(_, Just hout, _, _) 
    putStrLn "Booting IDE"
    case result of
        Left ex -> putStrLn ("IDE: " ++ show ex) -- orderedMessage $ LogMessage LOG_TAIL LOG_ERROR 
        Right _ -> putStrLn "IDE: CHECKLIST OK" -- orderedMessage $ LogMessage LOG_TAIL LOG_ZONE 
