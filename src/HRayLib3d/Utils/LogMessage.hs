{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module HRayLib3d.Utils.LogMessage (
    LogLevel   (..), 
    LogPrefix  (..),
    LogMessage (..), 
    logColor,
    logSymbol,
    logPrefix,
    logMessage,
    logMessageBar,
    logMessageLines,
    orderedMessage,
    exceptionMessage,
    eventMessage,
    errorMessage
    -- logOrderedMessage,
  ) where 


  import Data.List
  import Data.String
  import Control.Monad.State 

  -- Ternary Function Helper
  data Ternary a = a :? a

  infixl 0 ?
  infixl 1 :?

  (?) :: Bool -> Ternary a -> a
  True  ? (x :? _) = x
  False ? (_ :? y) = y
  -- Ternary Function Helper

  data LogLevel   =
    LOG_ZONE
    |LOG_INFO
    |LOG_DEBUG
    |LOG_MESSAGE
    |LOG_WARNING
    |LOG_ERROR
    deriving (Show, Eq, Enum)

  data LogPrefix  =
    LOG_HEAD
    |LOG_BODY
    |LOG_TAIL 
    deriving (Show, Eq, Enum)

  data LogMessage = 
    LogMessage { 
      order :: LogPrefix, 
      level :: LogLevel, 
      body  :: String 
    } deriving (Show, Eq) 

  -- instance (MonadState LogPrefix IO) => MonadState LogPrefix IO where
  --   get   = liftIO   get
  --   put k = liftIO $ put k
    
  -- terminate: \033[0m
  logColor :: LogLevel -> [Char] -> [Char]
  logColor level messageBody = 
      case level of
        LOG_INFO    -> "\x1b[96m" ++ messageBody -- ++ "\033"  --[0m
        LOG_MESSAGE -> "\x1b[34m" ++ messageBody -- ++ "\033" 
        LOG_ZONE    -> "\x1b[32m" ++ messageBody -- ++ "\033"
        LOG_DEBUG   -> "\x1b[33m" ++ messageBody -- ++ "\033"
        LOG_WARNING -> "\x1b[35m" ++ messageBody -- ++ "\033"
        LOG_ERROR   -> "\x1b[31m" ++ messageBody -- ++ "\033"

  logSymbol :: IsString p => LogLevel -> p
  logSymbol sym =
    case sym of 
        LOG_INFO    -> "🈳"  -- ℹ️
        LOG_MESSAGE -> "💬" 
        LOG_ZONE    -> "🈯"
        LOG_DEBUG   -> "🚧"
        LOG_WARNING -> "🚸"
        LOG_ERROR   -> "🈲"
      
  logPrefix :: LogLevel -> LogPrefix -> [Char]
  logPrefix level ord =
    case ord of
        LOG_HEAD   -> "╔(" ++ logSymbol level ++ ")"
        LOG_BODY   -> "╠(" ++ logSymbol level ++ ")" 
        LOG_TAIL   -> "╚(" ++ logSymbol level ++ ")"
        _          -> error "\x1b[31m ✖ INVALID LOG PREFIX ✖ \033[0m"

  -- | Util Functions for managing message formatting
  logMessage :: MonadIO m => String -> m ()
  logMessage text = liftIO $ putStr text

  logMessageBar :: MonadIO m => String -> [String] -> m ()
  logMessageBar color = logMessage . intercalate (" | " ++ color)

  logMessageLines :: MonadIO m =>  [String] -> m ()
  logMessageLines = logMessage . unlines

  -- | A chained message brick, pass anything to it, and manage it's order outside of Mani-Log
  orderedMessage :: MonadIO m => LogMessage -> m ()
  orderedMessage message@LogMessage{..} = do
    let prefix  = logPrefix level order
        msg     = logColor  level (prefix ++ " " ++ body)
    liftIO $ putStrLn msg

  -- | An unchained message; Be careful this may break formatting if put in a list with an orderedMessage.
  eventMessage :: MonadIO m => LogMessage -> m ()
  eventMessage message@LogMessage{..} = do
    let event   = "\x1b[45m▨ " ++ body ++ " ▨"
    liftIO $ putStrLn event

  -- | An unchained Error Message which does not stop execution but just displays a message.
  exceptionMessage :: MonadIO m => LogMessage -> m ()
  exceptionMessage message@LogMessage{..} = do
    let msg     = "\x1b[31m✖  " ++ body ++ " ✖"
    liftIO $ putStrLn msg

  -- | An unchained System Exception; Be careful this stops execution and displays an error message.
  errorMessage :: MonadIO m => LogMessage -> m ()
  errorMessage message@LogMessage{..} = do
    let err     = error "\x1b[31m✖ " ++ body ++ " ✖"
    liftIO $ putStrLn err