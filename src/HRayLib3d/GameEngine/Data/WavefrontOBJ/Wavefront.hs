{-# LANGUAGE PatternSynonyms #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Currently, you can parse a file and get a 'WavefrontOBJ' with the 'fromFile'
-- function.
-----------------------------------------------------------------------------

module HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront (
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
  , module HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.IO
  ) where

import HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.Element
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.Face
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.IO
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.Line
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.Location
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.Normal
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.Object
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.Point
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.TexCoord
