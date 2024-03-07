
{-# LANGUAGE OverloadedStrings #-}
module HRayLib3d.Network.Cloud (
        Supabase(..), 
        Firebase(..),
        Azure(..)
    ) where

    import HRayLib3d.Network.Database
    import qualified Data.ByteString as BS

    -- All Cloud Based communication happens here
    -- there are necessary accompanying functions 
    -- but it is also possible to condone communication
    -- entirely with the Requests module to the appropriate
    -- endpoints

    type ApiKey            = String
    type AppId             = String
    type AuthDomain        = String
    type DatabaseURL       = String
    type HTTPAddress       = String
    type MessagingSenderId = String
    type MeasurementId     = String
    type ProjectId         = String      
    type StorageBucket     = String     

    data Supabase    = 
        SupabaseClient { 
            supaAddr :: HTTPAddress, 
            supaKey  :: ApiKey 
        } deriving (Eq, Show)

    data Firebase    = 
         FirebaseClient {     
            apiKey            :: ApiKey,           
            appId             :: AppId,            
            projectId         :: ProjectId,        
            authDomain        :: AuthDomain,      
            databaseURL       :: DatabaseURL,      
            storageBucket     :: StorageBucket,         
            messagingSenderId :: MessagingSenderId,
            measurementId     :: MeasurementId      
        } deriving (Eq, Show)

    -- use this object with an api call like this 
    -- postRequest https://login.microsoftonline.com/<tenantId>/oauth2/token (Azure _ _ _ _)
    -- it will return an Authentication Token and can be used alongside TCP services 
    data Azure =
        AzureClient {
            grant_type    :: String,     -- =client_credentials \
            client_id     :: String,     -- =<appId> \
            client_secret :: String,     -- =<password> \
            resource      :: HTTPAddress -- resource=https://api.kusto.windows.net
        } deriving (Eq, Show)