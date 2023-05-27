{-# LANGUAGE CPP #-}
#define Flt Double
#define VECT_Double

module Data.Vect.Flt.Util.Dim2 where

import Data.Vect.Flt.Base

-- | Example: @structVec2 [1,2,3,4] = [ Vec2 1 2 , Vec2 3 4 ]@.
structVec2 :: [Flt] -> [Vec2]
structVec2 [] = []
structVec2 (x:y:ls) = (Vec2 x y):(structVec2 ls) 
structVec2 _ = error "structVec2"

-- | The opposite of "structVec2".
destructVec2 :: [Vec2] -> [Flt]
destructVec2 [] = []
destructVec2 ((Vec2 x y):ls) = x:y:(destructVec2 ls)  

det2 :: Vec2 -> Vec2 -> Flt
det2 u v = det (u,v)

vec2X :: Vec2
vec2Y :: Vec2

vec2X = Vec2 1 0 
vec2Y = Vec2 0 1 

translate2X :: Flt -> Vec2 -> Vec2
translate2Y :: Flt -> Vec2 -> Vec2

translate2X t (Vec2 x y) = Vec2 (x+t) y 
translate2Y t (Vec2 x y) = Vec2 x (y+t) 

-- | unit vector with given angle relative to the positive X axis (in the positive direction, that is, CCW).
-- A more precise name would be @cosSin@, but that sounds bad :)
sinCos :: Flt -> Vec2
sinCos a = Vec2 (cos a) (sin a)

sinCos' {- ' CPP is sensitive to primes -} :: Flt -> Normal2
sinCos' = toNormalUnsafe . sinCos

sinCosRadius :: Flt    -- ^ angle (in radians)
             -> Flt    -- ^ radius
             -> Vec2
sinCosRadius a r = Vec2 (r * cos a) (r * sin a)

-- | The angle relative to the positive X axis
angle2 :: Vec2 -> Flt
angle2 (Vec2 x y) = atan2 y x

angle2' {- ' CPP is sensitive to primes -} :: Normal2 -> Flt
angle2' = angle2 . fromNormal

-- | Rotation matrix by a given angle (in radians), counterclockwise.
rotMatrix2 :: Flt -> Mat2
rotMatrix2 a = Mat2 (Vec2 c s) (Vec2 (-s) c) where c = cos a; s = sin a

rotMatrixOrtho2 :: Flt -> Ortho2
rotMatrixOrtho2 = toOrthoUnsafe . rotMatrix2

rotate2 :: Flt -> Vec2 -> Vec2
rotate2 a v = v .* (rotMatrix2 a) 

-- |Rotates counterclockwise by 90 degrees.
rotateCCW :: Vec2 -> Vec2
rotateCCW (Vec2 x y) = Vec2 (-y) x

-- |Rotates clockwise by 90 degrees.
rotateCW :: Vec2 -> Vec2
rotateCW (Vec2 x y) = Vec2 y (-x)


