module HRayLib3d.GameEngine.Data.GLTF where

import Data.Int
import Data.Map (Map)
import Data.HashMap.Strict
import Data.Vect hiding (Vector)
import Data.Vector (Vector)
import Data.ByteString (ByteString)
import qualified Data.Vector.Storable as SV
import Codec.GlTF
import Codec.GlTF.Texture
import Codec.GlTF.Material
import Codec.GlTF.Animation

data Frame
    = Frame
    { frGlTFMins    :: !Vec3
    , frGlTFMaxs    :: !Vec3
    , frGlTFOrigin  :: !Vec3
    , frGlTFRadius  :: !Float
    , frGlTFName    :: !ByteString
    } deriving Show

data Tag
    = Tag
    { tgGlTFName         :: !ByteString
    , tgGlTFOrigin       :: !Vec3
    , tgGlTFRotationMat  :: !Mat3
    } deriving Show

-- Codec.GlTF Material -> Shader
data Shader
    = Shader
    { shGlTFName    :: !ByteString
    , shGlTFIndex   :: !Int
    } deriving Show

data Surface
    = Surface
    { srGlTFName        :: !ByteString
    , srGlTFMaterials   :: !(Vector Material)
    , srGlTFTriangles   :: !(SV.Vector Int32)
    , srGlTFTexCoords   :: !(SV.Vector Vec2)
    , srGlTFXyzNormal   :: !(Vector (SV.Vector Vec3,SV.Vector Vec3))
    } deriving Show

data GLTFModel
    = GLTFModel
    { gltfFrames      :: !(Vector Frame)
    , gltfTags        :: !(Vector (HashMap ByteString Tag))
    , gltfSurfaces    :: !(Vector Surface)
    } deriving Show

type GLTFSkin      = Map String Texture
type GLTFAnimation = Map String Animation