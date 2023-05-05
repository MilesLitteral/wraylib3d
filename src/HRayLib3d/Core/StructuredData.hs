{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}

module HRayLib3d.Core.StructuredData where

    import Data.Aeson
    import Data.Vect
    import HRayLib3d.GameEngine.Realm.Entities 
    import HRayLib3d.GameEngine.Realm.Items

    instance FromJSON Player
    instance ToJSON   Player

    instance FromJSON Vec3
    instance ToJSON   Vec3

    instance FromJSON    HRayLib3d.GameEngine.Realm.Items.Holdable
    instance ToJSON      HRayLib3d.GameEngine.Realm.Items.Holdable
    instance ToJSONKey   HRayLib3d.GameEngine.Realm.Items.Holdable
    instance FromJSONKey HRayLib3d.GameEngine.Realm.Items.Holdable
    instance FromJSON    HRayLib3d.GameEngine.Realm.Items.Armor
    instance ToJSON      HRayLib3d.GameEngine.Realm.Items.Armor

    instance FromJSON    HRayLib3d.GameEngine.Realm.Entities.Weapon
    instance ToJSON      HRayLib3d.GameEngine.Realm.Entities.Weapon
    instance ToJSONKey   HRayLib3d.GameEngine.Realm.Entities.Weapon
    instance FromJSONKey HRayLib3d.GameEngine.Realm.Entities.Weapon
    instance FromJSON    HRayLib3d.GameEngine.Realm.Entities.Powerup
    instance ToJSON      HRayLib3d.GameEngine.Realm.Entities.Powerup

    instance FromJSON    HRayLib3d.GameEngine.Realm.Items.Weapon
    instance ToJSON      HRayLib3d.GameEngine.Realm.Items.Weapon
    instance ToJSONKey   HRayLib3d.GameEngine.Realm.Items.Weapon
    instance FromJSONKey HRayLib3d.GameEngine.Realm.Items.Weapon
    instance FromJSON    HRayLib3d.GameEngine.Realm.Items.Powerup
    instance ToJSON      HRayLib3d.GameEngine.Realm.Items.Powerup