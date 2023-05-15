{-# LANGUAGE OverloadedStrings #-}
module HRayLib3d.Network.Database (sendDBQueryBS, sendDBQuery, Query(..)) where

    import Database.PostgreSQL.Simple
    import qualified Data.ByteString as BS
    import qualified Data.ByteString.Lazy as LS  

    -- the PostgresSQL Service must be installed (running)
    -- this is otherwise something of an internal function
    -- unless a user is building out a Cloud Service connected
    -- to something that aren't Realms
    sendDBQuery :: BS.ByteString -> Query  -> IO Int
    sendDBQuery libpq query = do
        conn     <- connectPostgreSQL libpq -- ex: host=localhost port=5432 dbname=mydb connect_timeout=10
        [Only i] <- query_ conn query       -- ex: "select 2 + 2"
        return i

    
    sendDBQueryBS :: BS.ByteString -> Query  -> IO LS.ByteString
    sendDBQueryBS libpq query = do
        conn     <- connectPostgreSQL libpq -- ex: host=localhost port=5432 dbname=mydb connect_timeout=10
        [Only i] <- query_ conn query       -- ex: "select 2 + 2"
        return i