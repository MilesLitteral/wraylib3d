{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings, ViewPatterns #-}
module HRayLib3d.GameEngine.RealmViewer.Entity where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as SB
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vect
import Data.Maybe

import HRayLib3d.GameEngine.Loader.Entity

data Entity
  = TriggerTeleport     SB.ByteString Int
  | TargetPosition      SB.ByteString Vec3
  deriving Show

loadTeleports :: [EntityData] -> ([Entity],Map ByteString Entity)
loadTeleports t = (teleports,targets) where
  teleports =
    [ TriggerTeleport (SB.pack target_) (read model_)
    | EntityData{..} <- t
    , classname `elem` ["trigger_teleport","trigger_push"] -- HACK
    , target_ <- maybeToList target
    , ('*':model_) <- maybeToList model
    ]
  targets = Map.fromList
    [ (targetName_,TargetPosition targetName_ (fromJust origin))
    | EntityData{..} <- t
    , classname `elem` ["target_position","misc_teleporter_dest","info_notnull"]
    , targetName <- maybeToList targetname
    , let targetName_ = SB.pack targetName
    ]
