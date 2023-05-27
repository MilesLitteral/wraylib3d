{-# LANGUAGE CPP #-}

module HRayLib3d.GameEngine.Data.OBJ (
   WavefrontOBJ
  , importObjFromFile
  ) where

import Data.Either
import Data.String
import System.IO
import Codec.Wavefront
import Codec.Wavefront.IO
import Control.Monad.IO.Class
import qualified Data.Binary as BW

importObjFromFile :: MonadIO m => FilePath -> m (Either String WavefrontOBJ) 
importObjFromFile = fromFile 

exportObjAsFile :: BW.Binary a => FilePath -> a -> IO () 
exportObjAsFile = BW.encodeFile
