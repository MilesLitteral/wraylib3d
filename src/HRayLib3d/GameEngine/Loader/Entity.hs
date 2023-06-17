{-# LANGUAGE LambdaCase, DeriveGeneric, OverloadedStrings, FlexibleInstances, TypeSynonymInstances #-}
module HRayLib3d.GameEngine.Loader.Entity
  ( emptyEntityData
  , EntityContainer(..)
  , EntityData(..)
  ) where

import Data.Aeson
import GHC.Generics

import Data.Vect
import Data.Void ()
import Data.Vect.Float.Instances ()
import qualified Data.Vector as V

import Text.XML (Document)
import Text.XML.Writer (document, element, XML, ToXML)
import Data.Maybe
-- import Data.Linear

newtype EntityContainer = EntityContainer {ecData :: [EntityData]} deriving (Eq, Show)

data EntityData
  = EntityData
  { classname           :: String
  , spawnflags          :: Maybe Int
  , origin              :: Maybe Vec3
  , angles              :: Maybe Vec3
  , notsingle           :: Maybe Bool
  , notteam             :: Maybe Bool
  , notfree             :: Maybe Bool
  , notq3a              :: Maybe Bool
  , speed               :: Maybe Float
  , wait                :: Maybe Float
  , random              :: Maybe Float
  , gravity             :: Maybe Float
  , roll                :: Maybe Float
  , light               :: Maybe Float
  , lip                 :: Maybe Float
  , height              :: Maybe Float
  , phase               :: Maybe Float
  , delay               :: Maybe Float
  , _color              :: Maybe EngineColor
  , count               :: Maybe Int
  , damage              :: Maybe Int
  , nobots              :: Maybe Int
  , nohumans            :: Maybe Int
  , health              :: Maybe Int
  , noglobalsound       :: Maybe Int
  , model               :: Maybe String
  , model2              :: Maybe String
  , target              :: Maybe String
  , targetname          :: Maybe String
  , team                :: Maybe String
  , gametype            :: Maybe String
  , message             :: Maybe String
  , noise               :: Maybe String
  , music               :: Maybe String
  , targetShaderName    :: Maybe String
  , targetShaderNewName :: Maybe String
  --, targetRubyScript  :: Maybe FilePath
  }
  deriving (Eq, Show, Generic)
  
instance ToJSON   EntityContainer where
  toJSON s = object ["ecData" .= ecData s]

instance FromJSON EntityContainer where
  parseJSON (Object v) =  EntityContainer <$> v.: "ecData" 

instance ToJSON Vec3 where
    toJSON (Vec3 x y z) = object ["x" .= x, "y" .= y, "z" .= z ]
  
instance ToJSON EngineColor where
    toJSON (EngineColor r g b) = object ["r" .= r, "g" .= g, "b" .= b ]
      
instance ToJSON EntityData where
    toJSON s = object
      [ "classname"                   .= classname  s,
        "model"                       .= model      s,
        "model2"                      .= model2     s,
        "target"                      .= target     s,
        "targetname"                  .= targetname s,
        "team"                        .= team       s,
        "targetshadername"            .= targetShaderName s,
        "targetshadernewname"         .= targetShaderNewName s,
        -- "targetRubyScript" -> (\v e -> e {targetRubyScript    = Just v}) <$> stringLiteral

        "spawnflags"  .= spawnflags s,

        "origin"      .= origin s,
        "angles"      .= angles s,

        "angle"       .= Vec3 0 (if isNothing (angles s)
                                  then 0
                                  else _2 $ fromJust $ angles s) 
                                  0, --s s,

        "notsingle"   .= notsingle s,
        "notteam"     .= notteam s,
        "notfree"     .= notfree s,
        "notq3a"      .= notq3a  s,
        "gametype"    .= gametype s,

      -- custom; varying defaults
        "message"     .= message s,
        "noise"       .= noise s,
        "music"       .= music s,

        "speed"       .= speed s,
        "wait"        .= wait  s,
        "random"      .= random s,
        "gravity"     .= gravity s,
        "roll"        .= roll s,
        "light"       .= light s,
        "lip"         .= lip s,
        "height"      .= height s,
        "phase"       .= phase s,
        "delay"       .= delay s,

        "_color"       .= _color s,

        "count"       .= count  s,
        "dmg"         .= damage s,
        "nobots"      .= nobots s,
        "nohumans"    .= nohumans s,
        "health"      .= health s,
        "noglobalsound" .= noglobalsound s
      ]

data EngineColor = EngineColor {r :: Float, g :: Float, b :: Float} deriving (Show, Eq)

instance FromJSON EngineColor where
  parseJSON (Object v) = EngineColor <$> v .: "r"
                                     <*> v .: "g"
                                     <*> v .: "b"

instance FromJSON Vec3 where
  parseJSON (Object v) = Vec3 <$> v .: "x"
                              <*> v .: "y"
                              <*> v .: "z"
                              
parseOriginJSON (Array v) | not (null v) = do
      x <- parseJSON $ v V.! 0
      y <- parseJSON $ v V.! 1
      z <- parseJSON $ v V.! 2
      return $ Just $ Vec3 x y z

instance FromJSON EntityData where
    parseJSON (Object v) = EntityData <$> v .:  "classname"
                                      <*> v .:? "spawnflags"
                                      <*> v .:? "origin"
                                      <*> v .:? "angles"
                                      <*> v .:? "notsingle"
                                      <*> v .:? "notteam"
                                      <*> v .:? "notfree"
                                      <*> v .:? "notq3a"

                                      <*> v .:? "speed"
                                      <*> v .:? "wait"
                                      <*> v .:? "random"
                                      <*> v .:? "gravity"
                                      <*> v .:? "roll"
                                      <*> v .:? "light"
                                      <*> v .:? "lip"
                                      <*> v .:? "height"
                                      <*> v .:? "phase"
                                      <*> v .:? "delay"

                                      <*> v .:? "_color"
                                      <*> v .:? "count"
                                      <*> v .:? "dmg"
                                      <*> v .:? "nobots"
                                      <*> v .:? "nohumans"
                                      <*> v .:? "health"
                                      <*> v .:? "noglobalsound"

                                      <*> v .:? "model"
                                      <*> v .:? "model2"
                                      <*> v .:? "target"
                                      <*> v .:? "targetname"
                                      <*> v .:? "team"
                                      <*> v .:? "gametype"
                                      <*> v .:? "message"
                                      <*> v .:? "noise"
                                      <*> v .:? "music"

                                      <*> v .:? "targetshadername"
                                      <*> v .:? "targetshadernewname"
                                      -- <*> v .:? "angle"

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start + 1) . drop start

-- quake 3 entity parser (xml)
-- element "hello" $ content "world"
-- {-# LANGUAGE OverloadedStrings #-}
-- let doc = document "root" $ do
--     element "hello" $ content "world"
--     element "hierarchy" $ do
--         element "simple" True
--         element "as" ("it should be" :: Text)
--         toXML $ Just . T.pack $ "like this"
--     comment "that's it!"

instance Semigroup XML

createXML :: ToXML a => [(String, a)] -> Document
createXML vxml = document "root" $ foldl1 (<>) $ map (uncurry valueXML) vxml

valueXML :: ToXML  a => String -> a -> Text.XML.Writer.XML
valueXML xml v =  case xml of
                    "classname"           -> element "classname"  v
                    "model"               -> element "model"      v
                    "model2"              -> element "model2"     v
                    "target"              -> element "target"     v
                    "targetname"          -> element "targetname" v
                    "team"                -> element "team" v
                    "targetshadername"    -> element "targetshadername"    v
                    "targetshadernewname" -> element "targetshadernewname" v

                    "spawnflags"    -> element "spawnflags" v
                    "origin"        -> element "origin" v
                    "angles"        -> element "angles" v
                    "angle"         -> element "angle" $ do 
                                                        element "x" (0.0 :: Float)
                                                        element "y" v 
                                                        element "z" (0.0 :: Float)

                    "notsingle"     -> element "notsingle" v
                    "notteam"       -> element "notteam"   v
                    "notfree"       -> element "notfree"   v
                    "notq3a"        -> element "notq3a"    v
                    "gametype"      -> element "gametype" v

                  -- custom; varying defaults
                    "message"       -> element "message" v
                    "noise"         -> element "noise" v
                    "music"         -> element "music" v
                    "speed"         -> element "speed" v
                    "wait"          -> element "wait" v
                    "random"        -> element "random" v
                    "gravity"       -> element "gravity" v
                    "roll"          -> element "roll"  v
                    "light"         -> element "light" v
                    "lip"           -> element "lip"   v
                    "height"        -> element "height" v
                    "phase"         -> element "phase" v
                    "delay"         -> element "delay"  v
                    "color"         -> element "color" v

                    "count"         -> element "count" v
                    "dmg"           -> element "dmg" v
                    "nobots"        -> element "nobots" v
                    "nohumans"      -> element "nohumans" v
                    "health"        -> element "health" v
                    "noglobalsound" -> element "noglobalsound" v
                    _               -> element "unknown" v

-- replace with: instance Default EntityData where
emptyEntityData :: EntityData
emptyEntityData = EntityData
  { classname     = ""
  , spawnflags    = Nothing --0
  , origin        = Nothing --zero
  , angles        = Nothing --zero
  , notsingle     = Nothing --False
  , notteam       = Nothing --False
  , notfree       = Nothing --False
  , notq3a        = Nothing --False
  , speed         = Nothing
  , wait          = Nothing
  , random        = Nothing
  , gravity       = Nothing
  , roll          = Nothing
  , light         = Nothing
  , lip           = Nothing
  , height        = Nothing
  , phase         = Nothing
  , delay         = Nothing
  , _color        = Nothing
  , count         = Nothing
  , damage        = Nothing
  , nobots        = Nothing
  , nohumans      = Nothing
  , health        = Nothing
  , noglobalsound = Nothing
  , model         = Nothing
  , model2        = Nothing
  , target        = Nothing
  , targetname    = Nothing
  , team          = Nothing
  , gametype      = Nothing
  , message       = Nothing
  , noise         = Nothing
  , music         = Nothing
  , targetShaderName    = Nothing
  , targetShaderNewName = Nothing
  -- , targetRubyScript = Nothing
  }