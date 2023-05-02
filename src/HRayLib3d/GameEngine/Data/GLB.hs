module HRayLib3d.GameEngine.Data.GLB where

import Data.Int
import Data.Map (Map)
import Data.HashMap.Strict
import Data.Vect hiding (Vector)
import Data.Vector (Vector)
import Data.ByteString (ByteString)
import qualified Data.Vector.Storable as SV
import Codec.GLB

data Frame
    = Frame
    { frMins    :: !Vec3
    , frMaxs    :: !Vec3
    , frOrigin  :: !Vec3
    , frRadius  :: !Float
    , frName    :: !ByteString
    } deriving Show

data Tag
    = Tag
    { tgName    :: !ByteString
    , tgOrigin  :: !Vec3
    , tgRotationMat  :: !Mat3
    } deriving Show

data Shader
    = Shader
    { shName    :: !ByteString
    , shIndex   :: !Int
    } deriving Show

data Surface
    = Surface
    { srName        :: !ByteString
    , srShaders     :: !(Vector Shader)
    , srTriangles   :: !(SV.Vector Int32)
    , srTexCoords   :: !(SV.Vector Vec2)
    , srXyzNormal   :: !(Vector (SV.Vector Vec3,SV.Vector Vec3))
    } deriving Show

-- No Animation, for Static Models
data GLBModel
    = GLBModel
    { glbHeader     :: Header
    , glbChunks     :: !(Vector Chunk) -- Each GLB Chunk is potentially a glTF
    , glbFrames     :: !(Vector Frame)
    , glbTags       :: !(Vector (HashMap ByteString Tag))
    , glbSurfaces   :: !(Vector Surface)
    } deriving Show

type GLBSkin = Map String Chunk
