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

    realmsLoadRealmList :: String -> IO String
    realmsLoadRealmList query        = sendHTTPRequest $ "http://127.0.0.1/realms/query/" ++ query

    realmsLoadRealmContentList :: String -> IO String
    realmsLoadRealmContentList realm = sendHTTPRequest $ "http://127.0.0.1/realms/" ++ realm

    sendRealmQuery   :: Query -> IO Int
    sendRealmQuery query = sendDBQuery "host=localhost port=5432 dbname=hrlRealms connect_timeout=10" query

    sendRealmQueryBS :: Query -> IO LS.ByteString
    sendRealmQueryBS query = sendDBQueryBS "host=localhost port=5432 dbname=hrlRealms connect_timeout=10" query