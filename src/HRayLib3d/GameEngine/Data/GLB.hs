module HRayLib3d.GameEngine.Data.GLB where

import Data.Text
import Data.Map (Map)
import Data.Vector (Vector)
import Data.Vect hiding (Vector)

import Codec.GLB
import Codec.GlTF.Material

-- No Animation, for Static Models or bundles of multiple individual asset
-- This could be a bundle (in Chunks) of the following:
-- GlTFs
-- Textures
-- Misc Data 
data GLBModel
    = GLBModel
    { glbHeader     :: Header
    , glbChunks     :: Vector Chunk -- Each GLB Chunk is potentially a glTF or Texture, see glbHeader for more info
    } deriving Show

type GLBSkin = Map Text Material
