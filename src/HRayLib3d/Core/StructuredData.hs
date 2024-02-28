{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}

module HRayLib3d.Core.StructuredData where

    import Data.Vect            ( Vec3 )
    import Data.Aeson           ( FromJSON, ToJSON, FromJSONKey, ToJSONKey )
    import Data.Yaml.Parser     ( readYamlFile, FromYaml )

    import Text.XML.Light       ( Content, parseXML )
    import Text.XML.Light.Lexer ( XmlSource(..) )
    
    import HRayLib3d.GameEngine.Realm.Entities ( Player, Powerup,  Weapon ) 
    import HRayLib3d.GameEngine.Realm.Items    ( Armor,  Holdable, Powerup, Weapon )

    -- Defines the use of JSON, YAML, and XML files,
    -- all datatype instances of JSON and XML are here.
    instance ToJSON   Player
    instance FromJSON Player

    instance FromJSON Vec3
    instance ToJSON   Vec3

    instance ToJSON      HRayLib3d.GameEngine.Realm.Items.Holdable
    instance FromJSON    HRayLib3d.GameEngine.Realm.Items.Holdable
    instance ToJSONKey   HRayLib3d.GameEngine.Realm.Items.Holdable
    instance FromJSONKey HRayLib3d.GameEngine.Realm.Items.Holdable

    instance ToJSON      HRayLib3d.GameEngine.Realm.Items.Armor
    instance FromJSON    HRayLib3d.GameEngine.Realm.Items.Armor

    instance ToJSON      HRayLib3d.GameEngine.Realm.Entities.Weapon
    instance FromJSON    HRayLib3d.GameEngine.Realm.Entities.Weapon
    instance ToJSONKey   HRayLib3d.GameEngine.Realm.Entities.Weapon
    instance FromJSONKey HRayLib3d.GameEngine.Realm.Entities.Weapon
    instance ToJSON      HRayLib3d.GameEngine.Realm.Entities.Powerup
    instance FromJSON    HRayLib3d.GameEngine.Realm.Entities.Powerup

    instance ToJSON      HRayLib3d.GameEngine.Realm.Items.Weapon
    instance FromJSON    HRayLib3d.GameEngine.Realm.Items.Weapon
    instance ToJSONKey   HRayLib3d.GameEngine.Realm.Items.Weapon
    instance FromJSONKey HRayLib3d.GameEngine.Realm.Items.Weapon
    instance ToJSON      HRayLib3d.GameEngine.Realm.Items.Powerup
    instance FromJSON    HRayLib3d.GameEngine.Realm.Items.Powerup

    loadXML  :: XmlSource s => s -> [Content]
    loadXML  = parseXML 

    loadYaml :: FromYaml a => FilePath -> IO a
    loadYaml = readYamlFile