{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards  #-}

module HRayLib3d.GameEngine.Data.BSP where

import Text.XML.Writer ()

import GHC.Generics    (Generic)
import Data.Aeson      --(FromJSON(parseJSON), ToJSON(toJSON), Object, object, KeyValue((.=)))
import Data.Binary     ()
import Data.Word       (Word8)
import Data.ByteString (ByteString)
import Data.Vector     (Vector)
import Data.Vect       (Vec3(..), Vec2(..), Vec4(..) )
import Data.Vect.Float.Instances ()

{-
    Information:
        http://graphics.stanford.edu/~kekoa/q3/
        http://www.mralligator.com/q3/
-}

data Model
    = Model
    { mdMins         :: !Vec3
    , mdMaxs         :: !Vec3
    , mdFirstSurface :: !Int
    , mdNumSurfaces  :: !Int
    , mdFirstBrush   :: !Int
    , mdNumBrushes   :: !Int
    } deriving (Eq, Show, Generic)

data Shader
    = Shader
    { shName         :: !ByteString
    , shSurfaceFlags :: !Int
    , shContentFlags :: !Int
    } deriving (Eq, Show, Generic)

data Plane
    = Plane
    { plNormal :: !Vec3
    , plDist   :: !Float
    } deriving (Eq, Show, Generic)

data Node
    = Node
    { ndPlaneNum :: !Int
    , ndChildren :: !(Int,Int)
    , ndMins     :: !Vec3
    , ndMaxs     :: !Vec3
    } deriving (Eq, Show, Generic)

data Leaf
    = Leaf
    { lfCluster          :: !Int
    , lfArea             :: !Int
    , lfMins             :: !Vec3
    , lfMaxs             :: !Vec3
    , lfFirstLeafSurface :: !Int
    , lfNumLeafSurfaces  :: !Int
    , lfFirstLeafBrush   :: !Int
    , lfNumLeafBrushes   :: !Int
    } deriving (Eq, Show, Generic)

data BrushSide
    = BrushSide
    { bsPlaneNum  :: !Int
    , bsShaderNum :: !Int
    } deriving (Eq, Show)

data Brush
    = Brush
    { brFirstSide :: !Int
    , brNumSides  :: !Int
    , brShaderNum :: !Int
    } deriving (Eq, Show)

data Fog
    = Fog
    { fgName        :: !ByteString
    , fgBrushNum    :: !Int
    , fgVisibleSide :: !Int
    } deriving (Eq, Show, Generic)

data DrawVertex
    = DrawVertex
    { dvPosition    :: !Vec3
    , dvDiffuseUV   :: !Vec2
    , dvLightmaptUV :: !Vec2
    , dvNormal      :: !Vec3
    , dvColor       :: !Vec4
    } deriving (Eq, Show, Generic)

data SurfaceType
    = Planar
    | Patch
    | TriangleSoup
    | Flare
    deriving (Eq, Show, Enum, Generic)

data Surface
    = Surface
    { srShaderNum      :: !Int
    , srFogNum         :: !Int
    , srSurfaceType    :: !SurfaceType
    , srFirstVertex    :: !Int
    , srNumVertices    :: !Int
    , srFirstIndex     :: !Int
    , srNumIndices     :: !Int
    , srLightmapNum    :: !Int
    , srLightmapPos    :: !Vec2
    , srLightmapSize   :: !Vec2
    , srLightmapOrigin :: !Vec3
    , srLightmapVec1   :: !Vec3
    , srLightmapVec2   :: !Vec3
    , srLightmapVec3   :: !Vec3
    , srPatchSize      :: !(Int,Int)
    } deriving (Eq, Show, Generic)

newtype Lightmap = Lightmap { lmMap :: ByteString } deriving (Eq, Show, Generic)

data LightGrid   = LightGrid deriving (Eq, Show, Generic)

data Visibility
    = Visibility
    { vsNumVecs     :: !Int
    , vsSizeVecs    :: !Int
    , vsVecs        :: !(Vector Word8)
    } deriving (Eq, Show, Generic)

data BSPLevel
    = BSPLevel
    { blEntities     :: !ByteString
    , blShaders      :: !(Vector Shader)
    , blPlanes       :: !(Vector Plane)
    , blNodes        :: !(Vector Node)
    , blLeaves       :: !(Vector Leaf)
    , blLeafSurfaces :: !(Vector Int)
    , blLeafBrushes  :: !(Vector Int)
    , blModels       :: !(Vector Model)
    , blBrushes      :: !(Vector Brush)
    , blBrushSides   :: !(Vector BrushSide)
    , blDrawVertices :: !(Vector DrawVertex)
    , blDrawIndices  :: !(Vector Int)
    , blFogs         :: !(Vector Fog)
    , blSurfaces     :: !(Vector Surface)
    , blLightmaps    :: !(Vector Lightmap)
    , blLightgrid    :: !(Vector LightGrid)
    , blVisibility   :: !Visibility
    } deriving (Eq, Show, Generic)

instance ToJSON Vec2 where
  toJSON (Vec2 x y) = object ["vec2X" .= x, "vec2Y" .= y ]

instance ToJSON Vec3 where
  toJSON (Vec3 x y z) = object ["vec3X" .= x, "vec3Y" .= y, "vec3Z" .= z ]

instance ToJSON Vec4 where
    toJSON (Vec4 w x y z) = object ["vec4W" .= w, "vec4X" .= x, "vec4Y" .= y, "vec4Z" .= z ]

instance ToJSON Shader where
    toJSON Shader{..} = object [ "shName" .= show shName, "shSurfaceFlags" .= shSurfaceFlags, "shContentFlags" .= shContentFlags]

instance ToJSON  Fog where
    toJSON Fog{..} = object ["fgName" .= (show fgName), "fgBrushNum" .= fgBrushNum, "fgVisibleSide" .= fgVisibleSide ]

instance ToJSON SurfaceType
instance ToJSON  Surface
instance ToJSON  Brush where
    toJSON Brush{..} = object[ "brFirstSide" .= brFirstSide, "brNumSides" .= brNumSides, "brShaderNum" .= brShaderNum]

instance ToJSON  Model
instance ToJSON  Leaf
instance ToJSON  Node where
    toJSON Node{..} = object[ "ndPlaneNum" .= ndPlaneNum, "ndChildren" .= ndChildren, "ndMins" .= ndMins, "ndMaxs" .= ndMaxs]

instance ToJSON  Plane
instance ToJSON  Lightmap where
    toJSON Lightmap{..} = object [ "lmMap" .= show lmMap ]

instance ToJSON  BrushSide where
    toJSON BrushSide{..} = object ["bsPlaneNum" .= bsPlaneNum, "bsShaderNum" .= bsShaderNum]

instance ToJSON  LightGrid
instance ToJSON  DrawVertex
instance ToJSON  Visibility

instance ToJSON  BSPLevel where
    toJSON BSPLevel{..} = object [ 
          "entities"     .= show blEntities
        , "shaders"      .= blShaders
        , "planes"       .= blPlanes
        , "nodes"        .= blNodes
        , "leaves"       .= blLeaves
        , "leafSurfaces" .= blLeafSurfaces
        , "leafBrushes"  .= blLeafBrushes
        , "models"       .= blModels
        , "brushes"      .= blBrushes
        , "brushSides"   .= blBrushSides
        , "drawVertices" .= blDrawVertices
        , "drawIndices"  .= blDrawIndices
        , "fogs"         .= blFogs
        , "surfaces"     .= blSurfaces
        , "lightmaps"    .= blLightmaps
        , "lightgrid"    .= blLightgrid
        , "visibility"   .= blVisibility
        ] 

instance FromJSON  LightGrid
instance FromJSON  Visibility

-- instance FromJSON  ByteString
-- instance FromJSON  Vec2
-- instance FromJSON  Vec3
-- instance FromJSON  Vec4

-- instance FromJSON  Node
-- instance FromJSON  SurfaceType
-- instance FromJSON  Surface
-- instance FromJSON  Lightmap

-- instance FromJSON  BSPLevel --where             
    -- parseJSON (Object v) = 
    --     BSPLevel <$> v     .: "entities"      
    --                 <*>  v .: "shaders" 
    --                 <*>  v .: "planes"        
    --                 <*>  v .: "nodes"         
    --                 <*>  v .: "leaves"        
    --                 <*>  v .: "leafSurfaces"  
    --                 <*>  v .: "leafBrushes"   
    --                 <*>  v .: "models"        
    --                 <*>  v .: "brushes"       
    --                 <*>  v .: "brushSides"    
    --                 <*>  v .: "drawVertices"  
    --                 <*>  v .: "drawIndices"   
    --                 <*>  v .: "fogs"          
    --                 <*>  v .: "surfaces"      
    --                 <*>  v .: "lightmaps"     
    --                 <*>  v .: "lightgrid"     
    --                 <*>  v .: "visibility"  

-- --fix this later for JSON and XML Support
-- TODO: a custom data XML object with an ToXML and FromXML allegory
-- that should rival Data.Aeson's support of JSON, this is not a priority
-- but something todo

-- instance ToXML BSPLevel where
--     toXML bspLevel = element "BSPLevel" $ many "entry" $ show $ blEntities bspLevel

-- map (\x -> element "container" $ many . show "wrapper" . blEntities)
-- instance Binary XML
-- instance (GHC.Generics.Generic XML)