{-# LANGUAGE LinearTypes, UnicodeSyntax, GADTs #-}
module HRayLib3d.WindowSystem.Streaming where

import Prelude hiding (IO, Show, fromInteger, (>>=), (>>), return, fail, ($), (.))
import Prelude.Linear ( Show, IO )
  
type Path   = String
type FileIO = LState (S [String])

-- |WRayLib3d's Data (File) Streaming Module (utilizing LinearTypes)
-- The State Machine has one function, Run, which it acts on stream 
-- with.
newtype LState s a = LState { runLState :: s ⊸ (s, a) }

data LHandle where Handle :: Int -> LHandle

-- | Record Keeper of the current State (S)
data S a where S :: a -> S a
  deriving (Show, Eq)

-- | Open (Stream) behavior, provide a file path
openFile  :: Path -> LHandle
openFile _ = Handle 0

-- | Close (Stream) behavior, provide a handle
closeFile :: LHandle ⊸ ()
closeFile (Handle _) = ()

-- | Read (Stream) behavior, will 
getRow :: Prelude.Linear.IO String
getRow = getLine                                                                                                                                                               