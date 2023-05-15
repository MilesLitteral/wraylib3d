{-# LANGUAGE FlexibleInstances #-}
module HRayLib3d.Network.Web where 

import GHC.Exts
import Data.Maybe
import Data.Monoid
import Control.Lens 
import Data.Hashable (Hashable(..))

data (IsList a) => WebRequest a = WebRequest { request :: String, response :: a } deriving (Ord, Eq, Show)

instance Hashable (WebRequest String) where
    hashWithSalt salt lb =
        hashWithSalt salt (lb) 
    hash lb = 
        hash (lb)