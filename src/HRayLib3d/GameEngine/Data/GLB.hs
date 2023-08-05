module HRayLib3d.GameEngine.Data.GLB where

import Data.Text ( Text )
import Data.Map  ( Map )
import Codec.GLB ( GLB )
import Codec.GlTF.Material ( Material )

-- No Animation, for Static Models or bundles of multiple individual asset
-- This could be a bundle (in Chunks) of the following:
-- GlTFs
-- Textures
-- Misc Data 

type    GLBSkin  = Map Text Material
newtype GLBModel = GLBModel { rawGlb :: GLB } deriving Show
