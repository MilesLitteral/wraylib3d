
{-# LANGUAGE OverloadedStrings #-}
module HRayLib3d.Network.Cloud (Supabase(..), Firebase(..)) where

    import HRayLib3d.Network.Database
    import qualified Data.ByteString as BS

    type HTTPAddress       = String
    type SupabaseKey       = String

    type ApiKey            = String
    type AppId             = String
    type ProjectId         = String
    type AuthDomain        = String
    type DatabaseURL       = String
    type StorageBucket     = String     
    type MessagingSenderId = String
    type MeasurementId     = String      

    data Supabase    = 
        SupabaseClient { 
            supaAddr :: HTTPAddress, 
            supaKey  :: SupabaseKey 
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
