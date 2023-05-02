{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}

module HRayLib3d.Core.StructuredData where

    import Data.Aeson
    import HRayLib3d.GameEngine.Realm.Entities 

    --Bring This back when you make some meaningful Serialized data types
    -- instance FromJSON Player where
    --     parseJSON (Object v) = Player <$> v .: "_pPosition" <*>  v .: "_pDirection" <*> v .: "_pVelocity" <*> v .: "_pRotationUV" <*> v .: "_pHealth" <*> v .: "_pArmor" <*> v .: "_pArmorType" <*> v .: "_pShootTime" <*> v .: "_pDamageTimer"    <*> v .: "_pName"           <*> v .: "_pId"             <*> v .: "_pAmmos"          <*> v .: "_pWeapons"        <*> v .: "_pSelectedWeapon" <*> v .: "_pHoldables"      <*> v .: "_pPowerups"       <*> v .: "_pCanJump"
    --     --parseJSON _          = object []

    -- instance ToJSON   Player where 
    --     toJSON  Player{..} = 
    --         object [
    --             "_pPosition"      .= _pPosition
    --             , "_pDirection"   .= _pDirection
    --             , "_pVelocity"    .= _pVelocity
    --             , "_pRotationUV"  .= _pRotationUV
    --             , "_pHealth"      .= _pHealth
    --             , "_pArmor"       .= _pArmor
    --             , "_pArmorType"   .= _pArmorType
    --             , "_pShootTime"   .= _pShootTime
    --             , "_pDamageTimer" .= _pDamageTimer
    --             , "_pName"        .= _pName
    --             , "_pId"          .= _pId
    --             , "_pAmmos"       .= _pAmmos
    --             , "_pWeapons"     .= _pWeapons
    --             , "_pSelectedWeapon" .= _pSelectedWeapon 
    --             , "_pHoldables"   .= _pHoldables
    --             , "_pPowerups"    .= _pPowerups
    --             , "_pCanJump"     .= _pCanJump
    --         ]

    