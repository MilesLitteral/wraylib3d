
-- | The unit sphere in the space of quaternions has the group structure
-- SU(2) coming from the quaternion multiplication, which is the double
-- cover of the group SO(3) of rotations in R^3. Thus, unit quaternions can
-- be used to encode rotations in 3D, which is a more compact encoding 
-- (4 floats) than a 3x3 matrix; however, there are /two/ quaternions
-- corresponding to each rotation.
--
-- See <http://en.wikipedia.org/wiki/Quaternion> and 
-- <http://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation>
-- for more information.

{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveGeneric #-}
module Data.Vect.Flt.Util.Quaternion where

--------------------------------------------------------------------------------

import Data.Vect.Flt.Base
import Data.Vect.Flt.Interpolate

import Foreign.Storable
import System.Random
import System.Random.Internal
import GHC.Generics
--------------------------------------------------------------------------------
-- * types

-- | The type for quaternions. 
newtype Quaternion = Q Vec4 
  deriving (Read,Show,Storable,AbelianGroup,Vector,DotProd,Random,Interpolate, Generic)

instance (System.Random.Internal.GUniform (Rep Double))
instance (Generic Double)
instance (Uniform Double)
instance (Uniform Vec4)
instance (Uniform Quaternion)
instance (UniformRange Quaternion)

-- | The type for unit quaternions. 
newtype UnitQuaternion = U Vec4 
  deriving (Read,Show,Storable,DotProd, Generic)

-- | An abbreviated type synonym for quaternions
type Q = Quaternion

-- | An abbreviated type synonym for unit quaternions
type U = UnitQuaternion
   
--------------------------------------------------------------------------------

instance UnitVector Quaternion UnitQuaternion where
  mkNormal (Q v) = U (normalize v)
  toNormalUnsafe (Q v) = U v
  fromNormal (U v) = Q v
  fromNormalRadius r (U v) = Q (v &* r)

--------------------------------------------------------------------------------
-- * general quaternions

unitQ :: Q
unitQ = Q (Vec4 1 0 0 0)

zeroQ :: Q
zeroQ = Q (Vec4 0 0 0 0)

multQ :: Q -> Q -> Q
multQ (Q (Vec4 a1 b1 c1 d1)) (Q (Vec4 a2 b2 c2 d2)) = Q $ Vec4 
  (a1*a2 - b1*b2 - c1*c2 - d1*d2)
  (a1*b2 + b1*a2 + c1*d2 - d1*c2)
  (a1*c2 - b1*d2 + c1*a2 + d1*b2)
  (a1*d2 + b1*c2 - c1*b2 + d1*a2)

negQ :: Q -> Q
negQ (Q v) = Q (neg v)

normalizeQ :: Q -> Q
normalizeQ (Q v) = Q (normalize v)

-- | The inverse quaternion
invQ :: Q -> Q
invQ (Q (Vec4 a b c d)) = Q (v &* (1 / normsqr v)) where 
  v = Vec4 a (-b) (-c) (-d)
  
fromQ :: Q -> Vec4
fromQ (Q v) = v 

toQ :: Vec4 -> Q
toQ = Q 
  
--------------------------------------------------------------------------------

{- 
-- we use newtype deriving instead

instance AbelianGroup Quaternion where
  (Q v1) &+ (Q v2) = Q (v1 &+ v2)   
  (Q v1) &- (Q v2) = Q (v1 &+ v2)   
  neg (Q v) = Q (neg v)
  zero = zeroQ

instance DotProd Quaternion where
  dotprod (Q v1) (Q v2) = dotprod v1 v2

-}

instance MultSemiGroup Quaternion where
  one   = unitQ  
  (.*.) = multQ


--------------------------------------------------------------------------------
-- * unit quaternions

unitU :: U
unitU = U (Vec4 1 0 0 0)

multU :: U -> U -> U
multU (U (Vec4 a1 b1 c1 d1)) (U (Vec4 a2 b2 c2 d2)) = U $ Vec4 
  (a1*a2 - b1*b2 - c1*c2 - d1*d2)
  (a1*b2 + b1*a2 + c1*d2 - d1*c2)
  (a1*c2 - b1*d2 + c1*a2 + d1*b2)
  (a1*d2 + b1*c2 - c1*b2 + d1*a2)
  
-- | The opposite quaternion (which encodes the same rotation)
negU :: U -> U
negU (U v) = U (neg v)

-- | This is no-op, up to numerical imprecision.
-- However, if you multiply together a large number of unit quaternions, 
-- it may be a good idea to normalize the end result.
normalizeU :: U -> U
normalizeU (U v) = U (normalize v)

-- | The inverse of a unit quaternion
invU :: U -> U
invU (U (Vec4 a b c d)) = U $ Vec4 a (-b) (-c) (-d)

--------------------------------------------------------------------------------
 
fromU :: U -> Vec4
fromU (U v) = v 

fromU' :: U -> Normal4
fromU' (U v) = toNormalUnsafe v

mkU :: Vec4 -> U
mkU = U . normalize

toU :: Normal4 -> U
toU = U . fromNormal

unsafeToU :: Vec4 -> U
unsafeToU = U 

--------------------------------------------------------------------------------

{-  
-- we use newtype deriving instead

instance DotProd UnitQuaternion where
  dotprod (Q v1) (Q v2) = dotprod v1 v2
  
-}
   
instance MultSemiGroup UnitQuaternion where
  one   = unitU  
  (.*.) = multU
 
instance LeftModule UnitQuaternion Vec3 where
  lmul u v = actU u v

instance Random UnitQuaternion where
  random g = let (n, h) = random g 
                 v = fromNormal n :: Vec4
             in  (U v, h)  
  randomR _ = random     
  
--------------------------------------------------------------------------------
-- * unit quaternions as rotations

-- | The /left/ action of unit quaternions on 3D vectors.
-- That is,
-- 
-- > actU q1 $ actU q2 v  ==  actU (q1 `multU` q2) v 
actU :: U -> Vec3 -> Vec3
actU (U (Vec4 a b c d)) (Vec3 x y z) = Vec3 x' y' z' where
  x' =  x*(aa + bb - cc - dd)  +  y*(  2 * (bc - ad)  )  +  z*(  2 * (bd + ac)  )
  y' =  x*(  2 * (bc + ad)  )  +  y*(aa - bb + cc - dd)  +  z*(  2 * (cd - ab)  )
  z' =  x*(  2 * (bd - ac)  )  +  y*(  2 * (cd + ab)  )  +  z*(aa - bb - cc + dd)
  --
  aa = a*a ; bb = b*b ; cc = c*c ; dd = d*d
  ab = a*b ; ac = a*c ; ad = a*d
  bc = b*c ; bd = b*d ; cd = c*d

-- | The quaternion to encode rotation around an axis. Please note
-- that quaternions act on the /left/, that is
--
-- > rotU axis1 angl1 *. rotU axis2 angl2 *. v  ==  (rotU axis1 angl1 .*. rotU axis2 angl2) *. v 
--
rotU :: Vec3 -> Flt -> U
rotU axis angle = rotU' (mkNormal axis) angle

rotU' {- ' CPP is sensitive to primes -} :: Normal3 -> Flt -> U
rotU' axis angle = U (Vec4 c (x*s) (y*s) (z*s)) where
  Vec3 x y z = fromNormal axis 
  half = 0.5 * angle
  c = cos half
  s = sin half

-- | Interpolation of unit quaternions. Note that when applied to rotations,
-- this may be not what you want, since it is possible that the shortest path
-- in the space of unit quaternions is not the shortest path in the space of
-- rotations; see 'slerpU'!
longSlerpU :: Flt -> U -> U -> U
longSlerpU t (U p0) (U p1) = U v where
  v = (p0 &* y0) &+ (p1 &* y1) 
  omega = acos (p0 &. p1)
  s = sin omega
  y0 = sin (omega*(1-t)) / s 
  y1 = sin (omega*   t ) / s

-- | This is shortest path interpolation in the space of rotations; however
-- this is achieved by possibly flipping the first endpoint in the space of
-- quaternions. Thus @slerpU 0.001 q1 q2@ may be very far from @q1@ (and very
-- close to @negU q1@) in the space of quaternions (but they are very close
-- in the space of rotations). 
slerpU :: Flt -> U -> U -> U
slerpU t (U p0') (U p1) = U v where
  v = (p0 &* y0) &+ (p1 &* y1) 
  
  d' = p0' &. p1  
  (d,p0) = if d' >= 0 
    then ( d',     p0')
    else (-d', neg p0')
    
  omega = acos d
  s = sin omega
  y0 = sin (omega*(1-t)) / s 
  y1 = sin (omega*   t ) / s
  
-- | Makes a rotation matrix (to be multiplied with on the /right/) out of a unit quaternion:
--
-- > v .* rightOrthoU (rotU axis angl)  ==  v .* rotMatrix3 axis angl
-- 
-- Please note that while these matrices act on the /right/, quaternions act on the /left/; thus
-- 
-- > rightOrthoU q1 .*. rightOrthoU q2  ==  rightOrthoU (q2 .*. q1)
--
rightOrthoU :: U -> Ortho3
rightOrthoU = toOrthoUnsafe . transpose . fromOrtho . leftOrthoU

-- | Makes a rotation matrix (to be multiplied with on the /left/) out of a unit quaternion.
--
-- > leftOrthoU (rotU axis angl) *. v  ==  v .* rotMatrix3 axis angl
-- 
leftOrthoU :: U -> Ortho3
leftOrthoU (U (Vec4 a b c d)) = toOrthoUnsafe $ Mat3 row1 row2 row3 where
  row1 = Vec3  (aa + bb - cc - dd)  (  2 * (bc - ad)  )  (  2 * (bd + ac)  )
  row2 = Vec3  (  2 * (bc + ad)  )  (aa - bb + cc - dd)  (  2 * (cd - ab)  )
  row3 = Vec3  (  2 * (bd - ac)  )  (  2 * (cd + ab)  )  (aa - bb - cc + dd)
  --
  aa = a*a ; bb = b*b ; cc = c*c ; dd = d*d
  ab = a*b ; ac = a*c ; ad = a*d
  bc = b*c ; bd = b*d ; cd = c*d
  
--------------------------------------------------------------------------------
  
  