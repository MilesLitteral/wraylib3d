-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-----------------------------------------------------------------------------

module HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.IO where

import HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.Lexer ( lexer )
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.Object ( WavefrontOBJ, ctxtToWavefrontOBJ )
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.Token ( tokenize )
import Control.Monad.IO.Class ( MonadIO(..) )
import qualified Data.Text.IO as T ( readFile )

-- |Extract a 'WavefrontOBJ' from a Wavefront OBJ formatted file.
fromFile :: (MonadIO m) => FilePath -> m (Either String WavefrontOBJ)
fromFile fd = liftIO $ fmap (fmap (ctxtToWavefrontOBJ . lexer) . tokenize) (T.readFile fd)
