{-#LANGUAGE RankNTypes #-}

module Manifest.Utils.Prisms ((##), (#?), (#^?)) where

import Data.Maybe
import Data.Monoid
import Control.Lens 

-- | "Review" Operator
(##) :: AReview t s -> s -> t
x ## y = y ^. re x
--End

-- |"Preview Lens" Operator
x #^? y = y ^. x --(pre x)
--End

-- |"Preview Value" Operator
(#?) :: Getting (First a) s a -> s -> Maybe a 
x #? y = y ^? x --(pre x)
--End
