{-# LANGUAGE OverloadedStrings #-}
module HRayLib3d.Network.Database (startPostgresService, sendDBQuery) where
    
    import System.IO
    import System.Process 
    import Database.PostgreSQL.Simple
    import qualified Data.ByteString as BS

    startPostgresService :: IO Handle
    startPostgresService = do
        (_, Just hout, _, _) <- createProcess (proc "ls" []){ cwd = Just "./", std_out = CreatePipe }
        return hout
        
    -- the PostgresSQL Service must be running
    sendDBQuery :: BS.ByteString -> Query  -> IO Int
    sendDBQuery libpq query = do
        conn     <- connectPostgreSQL libpq -- ex: host=localhost port=5432 dbname=mydb connect_timeout=10
        [Only i] <- query_ conn query       -- ex: "select 2 + 2"
        return i

    