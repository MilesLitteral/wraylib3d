{-# LANGUAGE CPP #-}
#define Flt Float
#define VECT_Float

-- TODO: interpolation for Ortho3 matrices using the (short) quaternion 'slerpU'

-- | Interpolation of vectors. 
-- Note: we interpolate unit vectors differently from ordinary vectors.

module Data.Vect.Flt.Interpolate where

--------------------------------------------------------------------------------

import Data.Vect.Flt.Base
import Data.Vect.Flt.Util.Dim2 (sinCos',angle2')
import Data.Vect.Flt.Util.Dim3 (rotate3')

--------------------------------------------------------------------------------

class Interpolate v where
  interpolate :: Flt -> v -> v -> v
  
instance Interpolate Flt where
  interpolate t x y = x + t*(y-x)

--------------------------------------------------------------------------------

instance Interpolate Vec2 where interpolate t x y = x &+ t *& (y &- x)
instance Interpolate Vec3 where interpolate t x y = x &+ t *& (y &- x)
instance Interpolate Vec4 where interpolate t x y = x &+ t *& (y &- x)

--------------------------------------------------------------------------------

{-
instance Interpolate Normal2 where
  interpolate t nx ny = sinCos' $ ax + t*adiff where
    ax = angle2' nx
    ay = angle2' ny
    adiff = helper (ay - ax)
    helper d 
      | d < -pi   = d + twopi
      | d >  pi   = d - twopi
      | otherwise = d
    twopi = 2*pi
    
instance Interpolate Normal3 where 
  interpolate t nx ny = 
    if maxAngle < 0.001  -- more or less ad-hoc critical angle
      then mkNormal $ interpolate t x y
      else toNormalUnsafe $ rotate3' (t*maxAngle) (mkNormal axis) x where
    x = fromNormal nx
    y = fromNormal ny
    axis = (x &^ y)
    maxAngle = acos (x &. y)
-}        

instance Interpolate Normal2 where interpolate = slerp
instance Interpolate Normal3 where interpolate = slerp
instance Interpolate Normal4 where interpolate = slerp
        
--------------------------------------------------------------------------------
    
{-# SPECIALIZE slerp :: Flt -> Normal2 -> Normal2 -> Normal2 #-}
{-# SPECIALIZE slerp :: Flt -> Normal3 -> Normal3 -> Normal3 #-}
{-# SPECIALIZE slerp :: Flt -> Normal4 -> Normal4 -> Normal4 #-}
    
-- | Spherical linear interpolation.
-- See <http://en.wikipedia.org/wiki/Slerp>    
slerp :: (Interpolate v, UnitVector v u) => Flt -> u -> u -> u
slerp t n0 n1 = toNormalUnsafe v where
  v = (p0 &* y0) &+ (p1 &* y1) 
  p0 = fromNormal n0
  p1 = fromNormal n1
  omega = acos (p0 &. p1)
  s = sin omega
  y0 = sin (omega*(1-t)) / s 
  y1 = sin (omega*   t ) / s
  
--------------------------------------------------------------------------------

  