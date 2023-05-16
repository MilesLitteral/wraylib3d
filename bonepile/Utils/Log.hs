{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Manifest.Utils.Log where

import Data.List
import Data.String
import Control.Monad.IO.Class
import Control.Monad.State
import System.Exit

data ManiLogMessage = 
  ManiLogMessage { 
    order :: ManiLogPrefix, 
    level :: ManiLogLevel, 
    body  :: String 
  }deriving(Show, Eq) 

data ManiLogLevel =
  MANI_LOG_ZONE
  |MANI_LOG_INFO
  |MANI_LOG_DEBUG
  |MANI_LOG_MESSAGE
  |MANI_LOG_WARNING
  |MANI_LOG_CONFIRMATION
  |MANI_LOG_EXCEPTION
  |MANI_LOG_ERROR
  deriving(Show, Eq, Enum)

data ManiLogPrefix =
  MANI_LOG_HEAD
  |MANI_LOG_BODY
  |MANI_LOG_TAIL 
  deriving(Show, Eq, Enum)

instance (MonadState ManiLogPrefix IO) => MonadState ManiLogPrefix IO where
  get   = liftIO get
  put k = liftIO (put k)
  
--terminate: \033[0m
log_color :: ManiLogLevel -> [Char] -> [Char]
log_color level messageBody = 
    case level of
      MANI_LOG_INFO         -> "\x1b[96m" ++ messageBody -- ++ "\033"  --[0m
      MANI_LOG_MESSAGE      -> "\x1b[34m" ++ messageBody -- ++ "\033" 
      MANI_LOG_ZONE         -> "\x1b[32m" ++ messageBody -- ++ "\033"
      MANI_LOG_DEBUG        -> "\x1b[33m" ++ messageBody -- ++ "\033"
      MANI_LOG_WARNING      -> "\x1b[35m" ++ messageBody -- ++ "\033"
      MANI_LOG_ERROR        -> "\x1b[31m" ++ messageBody -- ++ "\033"
      MANI_LOG_EXCEPTION    -> "\x1b[31m" ++ messageBody -- ++ "\033"
      MANI_LOG_CONFIRMATION -> "\x1b[36m" ++ messageBody -- ++ "\033"

log_symbol :: IsString p => ManiLogLevel -> p
log_symbol sym =
  case sym of 
      MANI_LOG_INFO         -> "🈳️" --ℹ️ 
      MANI_LOG_MESSAGE      -> "💬" 
      MANI_LOG_ZONE         -> "🈯"
      MANI_LOG_DEBUG        -> "🚧"
      MANI_LOG_WARNING      -> "🚸"
      MANI_LOG_ERROR        -> "🈲"
      MANI_LOG_EXCEPTION    -> "🈵"
      MANI_LOG_CONFIRMATION -> "🉐️"
    
log_prefix :: ManiLogLevel -> ManiLogPrefix -> [Char]
log_prefix level ord =
  case ord of
      MANI_LOG_HEAD   -> "╔(" ++ log_symbol level ++ ")"
      MANI_LOG_BODY   -> "╠(" ++ log_symbol level ++ ")" 
      MANI_LOG_TAIL   -> "╚(" ++ log_symbol level ++ ")"
      --_      -> error "\x1b[31m ✖ INVALID LOG PREFIX ✖ \033[0m"

--Util Functions for managing message formatting
logMessage :: MonadIO m => String -> m ()
logMessage text = liftIO $ putStr text

logMessageBar :: MonadIO m => String -> [String] -> m ()
logMessageBar color = logMessage . intercalate (" | " ++ color)

logMessageLines :: MonadIO m =>  [String] -> m ()
logMessageLines = logMessage . unlines

-- A chained message brick, pass anything to it, and manage it's order outside of Mani-Log
orderedMessage :: MonadIO m => ManiLogMessage -> m ()
orderedMessage message@ManiLogMessage{..} = do
  let prefix  = log_prefix (level) (order)
  let msg     = log_color  (level) (prefix ++ " " ++ (body))
  liftIO $ putStrLn msg

-- An unchained message; Be careful this may break formatting if put in a list with an orderedMessage.
eventMessage :: MonadIO m => ManiLogMessage -> m ()
eventMessage message@ManiLogMessage{..} = do
  let event   = "\x1b[45m▨ " ++ body ++ " ▨"
  liftIO $ putStrLn event

confirmMessage :: MonadIO m => ManiLogMessage -> m ()
confirmMessage message@ManiLogMessage{..} = do
  let event   = "\x1b[45m⬤ " ++ body ++ " ⬤"
  liftIO $ putStrLn event
  answer <- liftIO getLine
  if answer == "Yes" then (liftIO $ putStrLn "Understood, Continue") else liftIO exitSuccess
  
-- An unchained Error Message which does not stop execution but just displays a message.
errorMessage :: MonadIO m => ManiLogMessage -> m ()
errorMessage message@ManiLogMessage{..} = do
  let msg     = ("\x1b[31m⚠  " ++ body ++ " ⚠")
  liftIO $ putStrLn msg

-- An unchained System Exception; Be careful this stops execution and displays an error message.
exceptionMessage :: MonadIO m => ManiLogMessage -> m ()
exceptionMessage message@ManiLogMessage{..} = do
  let err     = error "\x1b[31m✖ " ++ body ++ " ✖"
  liftIO $ putStrLn err

-- |Simple Messages, Equivalent to Print but in ManiLog Style
-- A chained message brick, pass anything to it, and manage it's order outside of Mani-Log
orderedMessageS :: MonadIO m => ManiLogPrefix -> String  -> m ()
orderedMessageS p s = orderedMessage (ManiLogMessage p MANI_LOG_MESSAGE s)

-- Simple Messages, Equivalent to Print but in ManiLog Style
-- A chained message brick, pass anything to it, and manage it's order outside of Mani-Log
infoMessageS :: MonadIO m => ManiLogPrefix -> String  -> m ()
infoMessageS p s = orderedMessage (ManiLogMessage p MANI_LOG_INFO s)

-- An unchained message; Be careful this may break formatting if put in a list with an orderedMessage.
eventMessageS :: MonadIO m => ManiLogPrefix -> String -> m ()
eventMessageS p s = eventMessage (ManiLogMessage p MANI_LOG_ZONE s)

confirmMessageS :: MonadIO m => ManiLogPrefix -> String -> m ()
confirmMessageS p s = confirmMessage (ManiLogMessage p MANI_LOG_CONFIRMATION s)
  
-- Simple Messages, Equivalent to Print but in ManiLog Style
-- A chained message brick, pass anything to it, and manage it's order outside of Mani-Log
warnMessageS :: MonadIO m => ManiLogPrefix -> String  -> m ()
warnMessageS p s = orderedMessage (ManiLogMessage p MANI_LOG_WARNING s)

-- An unchained Error Message which does not stop execution but just displays a message.
errorMessageS :: MonadIO m => ManiLogPrefix -> String -> m ()
errorMessageS p s = errorMessage (ManiLogMessage p MANI_LOG_ERROR s)

-- An unchained System Exception; Be careful this stops execution and displays an error message.
exceptionMessageS :: MonadIO m => ManiLogPrefix -> String -> m ()
exceptionMessageS p s = exceptionMessage (ManiLogMessage p MANI_LOG_EXCEPTION s)
