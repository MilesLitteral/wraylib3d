module HRayLib3d.GameEngine.Data.GLB where

import Data.Text ( Text )
import Data.Map  ( Map )
import Data.Binary.Get (ByteOffset(..))
import Codec.GLB ( fromFile, GLB )
import Codec.GlTF.Material ( Material )

-- No Animation, for Static Models or bundles of multiple individual asset
-- This could be a bundle (in Chunks) of the following:
-- GlTFs
-- Textures
-- Misc Data 

type    GLBSkin  = Map Text Material
newtype GLBModel = GLBModel { rawGlb :: GLB } deriving Show

validateGLB :: Either (ByteOffset, String) GLB -> IO GLBModel
validateGLB gl = case gl of
                    Left s     -> error $ "Parse Error, Raw Parse Output:" ++ show s
                    Right glb -> return $ GLBModel glb

-- A convenience function to read GLBModel types easier
readGLB :: String -> IO GLBModel
readGLB glbFilename = do
    -- open the glb file
    glbFile <- fromFile glbFilename
    validateGLB glbFile