{-# LANGUAGE OverloadedStrings #-}
module HRayLib3d.Network.Database (sendDBQueryBS, sendDBQuery, Query(..)) where

    import Data.Text
    import Text.Printf
    import Data.UUID

    import Database.PostgreSQL.Simple
    import qualified Data.ByteString as BS
    import qualified Data.ByteString.Lazy as LS  

    -- #9395
    -- the PostgresSQL Service must be installed (running)
    -- this is otherwise something of an internal function
    -- unless a user is building out a Cloud Service connected
    -- to something that aren't Realms
    
    createDBQuery :: BS.ByteString -> IO Int
    createDBQuery libpq = do
        conn     <- connectPostgreSQL libpq -- ex: "host=localhost port=5432 dbname=mydb connect_timeout=10"
        [Only i] <- query_ conn ("CREATE DATABASE wrlp WITH ENCODING 'UTF8' LC_COLLATE='English_United Kingdom' LC_CTYPE='English_United Kingdom';")      
        return i

    selectDBQuery :: BS.ByteString -> Query -> IO Int
    selectDBQuery libpq table_name = do
        conn     <- connectPostgreSQL libpq -- ex: "host=localhost port=5432 dbname=mydb connect_timeout=10"
        [Only i] <- query_ conn (table_name) --"SELECT * FROM table_name;"
        return i
        
    sendDBQuery :: BS.ByteString -> Query  -> IO Int
    sendDBQuery libpq query = do
        conn     <- connectPostgreSQL libpq -- ex: "host=localhost port=5432 dbname=mydb connect_timeout=10"
        [Only i] <- query_ conn query       -- ex: "select 2 + 2"
        return i

    sendDBQueryBS :: BS.ByteString -> Query  -> IO LS.ByteString
    sendDBQueryBS libpq query = do
        conn     <- connectPostgreSQL libpq -- ex: "host=localhost port=5432 dbname=mydb connect_timeout=10"
        [Only i] <- query_ conn query       -- ex: "select 2 + 2"
        return i