{-# LANGUAGE OverloadedStrings #-}

module HRayLib3d.Network.Realms (
    Query(..)
    , realmsLoadRealmList
    , realmsLoadRealmContentList
    , sendRealmQuery
    , sendRealmQueryBS
    ) where 

    import HRayLib3d.Network.Requests
    import HRayLib3d.Network.Database
    import qualified Data.ByteString.Lazy as LS

    -- #Realms
    -- Realms are Project files represented as networked spaces.
    -- By default, all realms are scaffolled but set to a 'Closed'
    -- state. These functions allow the application to load Realms
    -- associated information based on given IP.

    type Host = String 

    realmsLoadRealmList :: Host -> String -> IO String
    realmsLoadRealmList ip query         = sendHTTPRequest $ "http://" ++ ip ++ "/realms/query/" ++ query

    realmsLoadRealmContentList :: Host -> String -> IO String
    realmsLoadRealmContentList  ip realm = sendHTTPRequest $ "http://" ++ ip ++ "/realms/" ++ realm

    sendRealmQuery   :: Host -> Query -> IO Int
    sendRealmQuery host query   = sendDBQuery "host=" ++ host ++ " port=5432 dbname=hrlRealms connect_timeout=10" query

    sendRealmQueryBS :: Host -> Query -> IO LS.ByteString
    sendRealmQueryBS host query = sendDBQueryBS "host=" ++ host ++ " port=5432 dbname=hrlRealms connect_timeout=10" query