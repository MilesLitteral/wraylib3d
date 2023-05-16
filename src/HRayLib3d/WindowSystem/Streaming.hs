{-# LANGUAGE LinearTypes, UnicodeSyntax #-}
module Manifest.Utils.Streaming where

import Prelude hiding (IO, Show, fromInteger, (>>=), (>>), return, fail, ($), (.))
import Prelude.Linear hiding (Eq)
import Control.Functor.Linear 

-- |Manifest's Data (File) Streaming Module (utilizing LinearTypes)
-- The State Machine has one function, Run, which it acts on stream 
-- with.
newtype LState s a = LState { runLState :: s ⊸ (s, a) }

-- | Record Keeper of the current State (S)
data S a where S :: a -> S a
  deriving (Show, Eq)

data LHandle where Handle :: Int -> LHandle
type Path   = String
type FileIO = LState (S [String])

-- | Open (Stream) behavior, provide a file path
openFile  :: Path -> LHandle
openFile _ = Handle 0

-- | Close (Stream) behavior, provide a handle
closeFile :: LHandle ⊸ ()
closeFile (Handle _) = ()

-- | Read (Stream) behavior, will 
getRow :: Prelude.Linear.IO String
getRow = getLine                                                                                                                                                               