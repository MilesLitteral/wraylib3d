{-# LANGUAGE LambdaCase, TemplateHaskell #-}
module HRayLib3d.GameEngine.Realm.Visuals where

import Data.Vect ( Vec3 )
import Lens.Micro.Platform ( makeLenses )
import Control.Monad ()

-- visual effects for game graphics
data Particle
  = Particle
  { _vpPosition   :: Vec3
  , _vpDirection  :: Vec3
  , _vpLifeTime   :: Float
  } deriving Show

newtype Visual = VParticle Particle deriving Show

concat <$> mapM makeLenses [''Particle]
