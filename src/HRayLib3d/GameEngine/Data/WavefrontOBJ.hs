{-# LANGUAGE PatternSynonyms #-}
module HRayLib3d.GameEngine.Data.WavefrontOBJ (
    -- * Vertex location
    Location(..)
    -- * Vertex texture coordinates
  , TexCoord(..)
    -- * Vertex normals
  , Normal(..)
    -- * Points
  , Point(..)
    -- * Lines
  , Line(..)
  , LineIndex(..)
    -- * Faces
  , Face(..)
  , FaceIndex(..)
  , pattern Triangle
  , pattern Quad
    -- * Element
  , Element(..)
    -- * Object
  , WavefrontOBJ(..)
    -- * Re-exports
  , module HRayLib3d.GameEngine.Data.WavefrontOBJ.IO
  ) where

import HRayLib3d.GameEngine.Data.WavefrontOBJ.Element
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Face
import HRayLib3d.GameEngine.Data.WavefrontOBJ.IO
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Line
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Location
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Normal
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Object
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Point
import HRayLib3d.GameEngine.Data.WavefrontOBJ.TexCoord
