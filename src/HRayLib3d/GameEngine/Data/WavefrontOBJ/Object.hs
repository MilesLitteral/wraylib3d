-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-----------------------------------------------------------------------------

module HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.Object where

import HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.Element
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.Face
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.Lexer ( Ctxt(..) )
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.Line
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.Location
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.Normal
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.Point
import HRayLib3d.GameEngine.Data.WavefrontOBJ.Wavefront.TexCoord
import Data.DList ( DList, toList )
import Data.Text ( Text )
import Data.Vector ( Vector, fromList )

data WavefrontOBJ = WavefrontOBJ {
    -- |Locations.
    objLocations :: Vector Location
    -- |Texture coordinates.
  , objTexCoords :: Vector TexCoord
    -- |Normals.
  , objNormals :: Vector Normal
    -- |Points.
  , objPoints :: Vector (Element Point)
    -- |Lines.
  , objLines :: Vector (Element Line)
    -- |Faces.
  , objFaces :: Vector (Element Face)
    -- |Material libraries.
  , objMtlLibs :: Vector Text
  } deriving (Eq,Show)

ctxtToWavefrontOBJ :: Ctxt -> WavefrontOBJ
ctxtToWavefrontOBJ ctxt = WavefrontOBJ {
    objLocations = fromDList (ctxtLocations ctxt)
  , objTexCoords = fromDList (ctxtTexCoords ctxt)
  , objNormals = fromDList (ctxtNormals ctxt)
  , objPoints = fromDList (ctxtPoints ctxt)
  , objLines = fromDList (ctxtLines ctxt)
  , objFaces = fromDList (ctxtFaces ctxt)
  , objMtlLibs = fromDList (ctxtMtlLibs ctxt)
  }

fromDList :: DList a -> Vector a
fromDList = fromList . toList
