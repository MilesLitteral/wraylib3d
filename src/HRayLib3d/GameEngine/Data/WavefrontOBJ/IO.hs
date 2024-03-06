module HRayLib3d.GameEngine.Data.WavefrontOBJ.IO (
  module HRayLib3d.GameEngine.Data.WavefrontOBJ.Object
  , module HRayLib3d.GameEngine.Data.WavefrontOBJ 
  , importObjFromFile
  , exportObjAsFile
  -- , Frame(..)
  -- , Surface(..)
  -- , Shader(..)
  ) where

import Data.Either            ()
import Data.String            ()
import System.IO              ()
import Control.Monad.IO.Class ( MonadIO(..) )
import qualified Data.Binary as BW
import qualified Data.Text.IO as T ( readFile )

import HRayLib3d.GameEngine.Data.WavefrontOBJ.Lexer  ( lexer )
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Token  ( tokenize )
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Object ( WavefrontOBJ, ctxtToWavefrontOBJ )

-- |Extract a 'WavefrontOBJ' from a Wavefront OBJ formatted file.
fromFile :: (MonadIO m) => FilePath -> m (Either String WavefrontOBJ)
fromFile fd = liftIO $ fmap (fmap (ctxtToWavefrontOBJ . lexer) . tokenize) (T.readFile fd)

importObjFromFile :: MonadIO m => FilePath -> m (Either String WavefrontOBJ) 
importObjFromFile = fromFile 

exportObjAsFile :: BW.Binary a => FilePath -> a -> IO () 
exportObjAsFile = BW.encodeFile
