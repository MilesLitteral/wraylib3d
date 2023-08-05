{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
module HRayLib3d.GameEngine.Realm.World where

import GHC.Generics ( Generic )
import Data.Binary  ( Binary )
import Lens.Micro.Platform ( makeLenses )
import System.Random.Mersenne.Pure64 ( PureMT )

import HRayLib3d.GameEngine.Realm.Visuals ( Visual )
import HRayLib3d.GameEngine.Realm.Entities ( Entity )
import qualified HRayLib3d.GameEngine.Realm.Items

data Input
  = Input
  { forwardmove    :: !Float
  , sidemove       :: !Float
  , shoot          :: !Bool
  , dtime          :: !Float
  , time           :: !Float
  , mouseX         :: !Float
  , mouseY         :: !Float
  , mouseU         :: !Float
  , mouseV         :: !Float
  , changeWeapon   :: !(Maybe HRayLib3d.GameEngine.Realm.Items.Weapon)
  , toggleHoldable :: !(Maybe HRayLib3d.GameEngine.Realm.Items.Holdable)
  , jump           :: Bool
  } deriving (Show, Generic)

data World
  = World -- server side
  { _wEntities  :: ![Entity]  -- snapshot
  , _wVisuals   :: ![Visual]  -- move to client
  , _wInput     :: !Input     -- recived input (for all clients)
  , _wRandomGen :: !PureMT    -- snapshot
  , _wMapFile   :: !String
  } deriving Show

makeLenses ''World

initWorld :: [Entity] -> String -> PureMT -> World
initWorld ents mapfile random = World
  { _wEntities  = ents
  , _wVisuals   = []
  , _wInput     = initInput
  , _wRandomGen = random
  , _wMapFile   = mapfile
  }

initInput :: Input
initInput = Input
  { forwardmove    = 0
  , sidemove       = 0
  , shoot          = False
  , dtime          = 0
  , time           = 0
  , mouseX         = 0
  , mouseY         = 100 * pi / 2
  , mouseU         = 0
  , mouseV         = 0
  , changeWeapon   = Nothing
  , toggleHoldable = Nothing
  , jump           = False
  }

newtype WorldSnapshot = WorldSnapshot { gameEntities :: [Entity] } deriving (Show, Generic)

instance Binary Input
instance Binary WorldSnapshot
