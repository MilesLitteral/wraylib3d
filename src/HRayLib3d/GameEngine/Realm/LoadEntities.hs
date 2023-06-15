{-# LANGUAGE RecordWildCards #-}
module HRayLib3d.GameEngine.Realm.LoadEntities
  ( loadEntities
  ) where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vect
import Data.Vect.Float.Util.Quaternion

import HRayLib3d.GameEngine.Realm.Items
import HRayLib3d.GameEngine.Realm.Entities
import qualified HRayLib3d.GameEngine.Loader.Entity as E

loadEntities :: [E.EntityData] -> [Entity]
loadEntities = catMaybes . map loadEntity

itemMap :: Map String Item
itemMap = Map.fromList [(itClassName i,i) | i <- items]

loadEntity :: E.EntityData -> Maybe Entity
loadEntity E.EntityData{..} = case Map.lookup classname itemMap of
  Just Item{..} -> case itType of
    IT_HEALTH t -> Just . EHealth $ Health
      { _hPosition  = fromJust origin
      , _hQuantity  = itQuantity
      , _hType      = t
      , _hTime      = 0.0
      }
    IT_WEAPON w -> Just . EWeapon $ Weapon
      { _wPosition  = fromJust origin
      , _wDropped   = False
      , _wType      = w
      , _wTime      = 0.0
      }
    IT_AMMO w -> Just . EAmmo $ Ammo
      { _aPosition  = fromJust origin
      , _aQuantity  = itQuantity
      , _aDropped   = False
      , _aType      = w
      , _aTime      = 0.0
      }
    IT_ARMOR t -> Just . EArmor $ Armor
      { _rPosition  = fromJust origin
      , _rQuantity  = itQuantity
      , _rDropped   = False
      , _rType      = t
      , _rTime      = 0.0
      }
    IT_POWERUP p -> Just . EPowerup $ Powerup
      { _puPosition = fromJust origin
      , _puType     = p
      , _puTime     = 0.0
      }
    IT_HOLDABLE h -> Just . EHoldable $ Holdable
      { _hoPosition = fromJust origin
      , _hoType     = h
      , _hoTime     = 0.0
      }
    _ -> Nothing
  Nothing -> case classname of
    "info_player_start"      -> spawnPoint
    "info_player_deathmatch" -> spawnPoint
    "team_CTF_bluespawn"     -> spawnPoint
    "team_CTF_redspawn"      -> spawnPoint
    "team_CTF_blueplayer"    -> spawnPoint
    "team_CTF_redplayer"     -> spawnPoint
    "trigger_teleport" -> do
      target_ <- target
      Just . ETeleport $ Teleport
        { _tPosition  = fromJust origin
        , _tTarget    = target_
        }
    "trigger_push" -> do 
      target_ <- target
      Just . ETeleport $ Teleport -- HACK
        { _tPosition  = fromJust origin
        , _tTarget    = target_
        }
    _ | classname `elem` ["target_position","misc_teleporter_dest","info_notnull"] -> do
      targetname_ <- targetname
      Just . ETarget $ Target
        { _ttPosition   = fromJust origin
        , _ttTargetName = targetname_
        }
    _ -> Nothing
  where
    spawnPoint = Just . ESpawnPoint $ SpawnPoint
      { _spPosition = fromJust origin
      , _spAngles   = fromJust angles
      }
