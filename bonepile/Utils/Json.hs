{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-#OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Manifest.Utils.Json
where

import GHC.Generics
import Data.Aeson
import Data.Default
import Data.String( IsString(..) )
import qualified Data.Map as M
import qualified Data.Massiv.Array as A
import qualified Data.Text as T
import qualified Data.ByteString as BS

import Manipipe.Type
import Manipipe.Property
import Manipipe.Client.Type
import qualified Text.Read as R

import Control.Lens ((^.))

-- Raw ReconSpec
-- {
--   "property_kind": "location", 
--   "property_span": 3, 
--   "property_range": [[-0.5, 0.5], [-6, -5], [-0.5, 0.5]], 
--   "learnable": [true, true, true]}, 
--   {"property_kind": "rotation_euler", 
--    "property_span": 3, 
--    "property_range": [[-0.7853981633974483, 0.0], [0, 0], [0, 0]], 
--    "learnable": [true, false, false]
--    }
-- }

    -- "property_kind  :: "rotation_euler", 
    -- "property_span  :: 3, 
    -- "property_range :: [[-0.7853981633974483, 0.0], [0, 0], [0, 0]], 
    -- "learnable      :: [true, false, false]

data ReconSpec 
  = ReconSpecification {
    rspec :: [ReconSpecSlice]
  } deriving(Eq, Show)

data ReconSpecSlice 
  = ReconSpecSlice {
    recon_property_kind       :: String, 
    recon_property_span       :: Int, 
    recon_property_range      :: [[Float]], 
    recon_learnable           :: [Bool],
    recon_property_descriptor :: String
  } deriving(Eq, Show)

instance Default ReconSpec where 
  def = ReconSpecification []

-- | IntakeSlice instance for FromJSON typeclass
instance FromJSON ReconSpecSlice where
  parseJSON (Object v) = ReconSpecSlice
                         <$> v .: "property_kind"
                         <*> v .: "property_span"
                         <*> v .: "property_range"
                         <*> v .: "learnable"
                         <*> v .: "descriptor"

-- | IntakeSlice instance for FromJSON typeclass
instance FromJSON ReconSpec where
  parseJSON v = ReconSpecification <$> parseJSON v

-- | All Intake JSON deserialize to this type: a list of IntakeSlices
data IntakeSequence =
    IntakeSequence {
        intakeSlices    :: [IntakeSlice]
    } deriving (Show, Eq)

-- | All Indicies of an IntakeSequence are of this type
data IntakeSlice =
    IntakeSlice {
        sliceImages       :: [T.Text],
        sliceLightingMeta :: T.Text
    } deriving (Show, Eq)

-- | An Uncompiled UI Widget based on an IntakeSequence 
data Album = Album {
  _albName           :: T.Text,
  _albVisible        :: Bool,
  _albYear           :: Int,
  _albDirectoryPath  :: T.Text,
  _albContents       :: [(T.Text, Bool)]
} deriving (Eq, Show)

newtype MLConfig = MLConfig { experiment_name :: String } deriving (Show, Eq, Generic)

instance FromJSON MLConfig where 
  parseJSON (Object v) = MLConfig <$> v .: "EXPERIMENT_NAME" 

-- | IntakeSlice instance for ToJSON typeclass
instance ToJSON IntakeSlice  where
  toJSON s = object
    [ "images"                      .= sliceImages s,
      "lighting_metadata"           .= sliceLightingMeta s
    ]

-- | IntakeSlice instance for FromJSON typeclass
instance FromJSON IntakeSlice where
  parseJSON (Object v) = IntakeSlice
                         <$> v .: "images"
                         <*> v .: "lighting_metadata"

-- | IntakeSequence instance for ToJSON typeclass
instance ToJSON  IntakeSequence where
  toJSON s    = object [ "intake" .= intakeSlices s ]

-- | IntakeSequence instance for FromJSON typeclass
instance FromJSON IntakeSequence where
  parseJSON (Object v) = IntakeSequence <$> v .: "intake" 

-- Convenience TypeClass Instance that (Album -> IntakeSequence)
-- Export Applicable ONLY!
instance ToJSON Album where 
   toJSON s   = object
    [   "images"                      .= [ x | x <- (_albContents s), snd x == True], --sliceImages s,
        "lighting_metadata"           .= _albVisible s --"N/A" --sliceLightingMeta s
    ]


showJson :: (Show a) => a -> Value
showJson =  String . T.pack . show

emptyJson :: [IntakeSlice]
emptyJson = [ IntakeSlice ["./assets/images/Initial.png"] "L-Null.mani" ]

createAlbumBasic :: [T.Text] -> Album
createAlbumBasic   content = Album "Default" True 2022 "./assets/Default/" $ zip content $ repeat False     --  [ False  | x <- [0..(length content)]] --[False, False, False] --id

createAlbumCustom :: String -> Int -> FilePath -> [T.Text] -> Album
createAlbumCustom title year path content  = Album (T.pack title) True year (T.pack path) $ zip content $ repeat False

-- [(property_descriptor: String, property_span: Int)] in hierarchical order 
-- charts will be assigned based on the order of the labels in the list by property_span times
loadLabelsFromReconSpec :: FilePath -> IO (Maybe ReconSpec)
loadLabelsFromReconSpec path = do
  js <- eitherDecodeFileStrict (path) -- full/path/to/recon_spec.json
  case js of 
    Left  e -> error e
    Right j -> return (Just j)