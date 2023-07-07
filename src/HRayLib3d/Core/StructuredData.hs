{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}

module HRayLib3d.Core.StructuredData where

    import Data.Aeson           ( FromJSON, ToJSON, FromJSONKey, ToJSONKey )
    import Data.Yaml.Parser     ( readYamlFile, FromYaml )
    import Text.XML.Light       ( Content, parseXML )
    import Text.XML.Light.Lexer ( XmlSource(..) )
    import Data.Vect            ( Vec3 )
    import HRayLib3d.GameEngine.Realm.Entities ( Player, Powerup,  Weapon ) 
    import HRayLib3d.GameEngine.Realm.Items    ( Armor,  Holdable, Powerup, Weapon )

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

    loadXML  :: XmlSource s => s -> [Content]
    loadXML  = parseXML 

    loadYaml :: FromYaml a => FilePath -> IO a
    loadYaml = readYamlFile