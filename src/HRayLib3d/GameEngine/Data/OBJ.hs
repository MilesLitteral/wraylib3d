module HRayLib3d.GameEngine.Data.OBJ (
  module Codec.Wavefront.Object
  , module Codec.Wavefront 
  , importObjFromFile
  , exportObjAsFile
  -- , Frame(..)
  -- , Surface(..)
  -- , Shader(..)
  ) where

import Data.Either            ()
import Data.String            ()
import System.IO              ()
import Codec.Wavefront.Object
import Codec.Wavefront        ( fromFile )
import Control.Monad.IO.Class ( MonadIO )
import qualified Data.Binary as BW

importObjFromFile :: MonadIO m => FilePath -> m (Either String WavefrontOBJ) 
importObjFromFile = fromFile 

exportObjAsFile :: BW.Binary a => FilePath -> a -> IO () 
exportObjAsFile = BW.encodeFile
