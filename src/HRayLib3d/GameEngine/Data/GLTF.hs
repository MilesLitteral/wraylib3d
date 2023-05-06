module HRayLib3d.GameEngine.Data.GLTF (
    Frame(..)
    ,Tag(..)
    ,Shader(..)
    ,Surface(..)
    ,GLTFModel(..)
    ,GLTFSkin
    ,GLTFAnimation
)where

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
import Codec.GlTF.Buffer
import Codec.GlTF.Accessor
-- GlTF	 
-- asset :: Asset	 
-- extensionsUsed :: Maybe (Vector Text)	 
-- extensionsRequired :: Maybe (Vector Text)	 
-- accessors :: Maybe (Vector Accessor)	 
-- animations :: Maybe (Vector Animation)	 
-- buffers :: Maybe (Vector Buffer)	 
-- bufferViews :: Maybe (Vector BufferView)	 
-- cameras :: Maybe (Vector Camera)	 
-- images :: Maybe (Vector Image)	 
-- materials :: Maybe (Vector Material)	 
-- meshes :: Maybe (Vector Mesh)	 
-- nodes :: Maybe (Vector Node)	 
-- samplers :: Maybe (Vector Sampler)	 
-- scenes :: Maybe (Vector Scene)	 
-- skins :: Maybe (Vector Skin)	 
-- textures :: Maybe (Vector Texture)	 
-- extensions :: Maybe Object	 
-- extras :: Maybe Value

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

-- (One Day) Codec.GlTF Material -> Shader (Material?)
-- Shaders can be pulled out of GLTFs from:
-- GlTF.extensionsUsed  
-- GlTF.extensionsRequired
-- The necessary data looks like this:
-- {
--     "extensions": {
--         "KHR_techniques_webgl": {
--             "programs": [
--                 {
--                     "fragmentShader": 0,
--                     "vertexShader": 1
--                 }
--             ],
--             "shaders": [
--                 {
--                     "type": 35632,
--                     "uri": "duck0FS.glsl"
--                 },
--                 {
--                     "type": 35633,
--                     "uri": "duck0VS.glsl"
--                 }
--             ],
--             "techniques": [
--                 {
--                     "program": 0,
--                     "attributes": {
--                         "a_position": {
--                             "semantic": "POSITION"
--                         }
--                     },
--                     "uniforms": {
--                         "u_modelViewMatrix": {
--                             "type": 35676,
--                             "semantic": "MODELVIEW"
--                         }
--                     }
--                 }
--             ]
--         }
--     }
-- }

data Shader
    = Shader
    { shGlTFName    :: !ByteString
    , shGlTFIndex   :: !Int
    } deriving Show

data Surface
    = Surface
    { srGlTFName        :: !ByteString
    , srGlTFShaders     :: !(Vector Shader) -- apply a generic, applicable, glsl shader for now -- Materials come later
    , srGlTFTriangles   :: !(SV.Vector Int32)
    , srGlTFTexCoords   :: !(SV.Vector Vec2)
    , srGlTFXyzNormal   :: !(Vector (SV.Vector Vec3,SV.Vector Vec3))
    } deriving Show

data GLTFModel
    = GLTFModel
    { gltfFrames      :: !(Vector Frame) -- GlTF.accessors :: Maybe (Vector Accessor) -> gltfFrames :: !(Vector Frame)
    , gltfTags        :: !(Vector (HashMap ByteString Tag))
    , gltfSurfaces    :: !(Vector Surface)
    } deriving Show

type GLTFSkin      = Map String Texture
type GLTFAnimation = Map String Animation