module HRayLib3d.GameEngine.Data.GLTF (
    GLTFModel(..)
    ,fromByteString 
    ,fromChunk
)where

import Codec.GlTF ( fromByteString, fromChunk, GlTF )

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

-- (One Day) Codec.GlTF Material -> Shader (Material?)
-- Shaders can be pulled out of GLTFs from:
-- GlTF.extensionsUsed  
-- GlTF.extensionsRequired
-- The necessary data looks like this:
-- {
--     "extensions": {
--         "KHR_techniques_webgl<wrl3d>": {
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
newtype GLTFModel = GLTFModel { gltfRaw :: Codec.GlTF.GlTF } deriving Show
