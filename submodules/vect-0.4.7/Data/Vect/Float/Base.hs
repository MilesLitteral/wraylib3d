{-# LANGUAGE CPP #-}
#define Flt Float
#define VECT_Float

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module Data.Vect.Flt.Base
  ( AbelianGroup(..) , vecSum
  , MultSemiGroup(..) , Ring , semigroupProduct
  , LeftModule(..) , RightModule(..)
  , Vector(..) , DotProd(..) , CrossProd(..)
  , normalize , distance , angle , angle'
  , UnitVector(..)
  , Pointwise(..)
  , Extend(..) , HasCoordinates(..) , Dimension(..)
  , Matrix(..) , Tensor(..) , Diagonal (..) , Determinant(..)
  , Orthogonal(..) , Projective(..) , MatrixNorms(..)
  , Vec2(..) , Vec3(..) , Vec4(..)
  , Mat2(..) , Mat3(..) , Mat4(..)
  , Ortho2 , Ortho3 , Ortho4
  , Normal2 , Normal3 , Normal4
  , Proj3 , Proj4
  , mkVec2 , mkVec3 , mkVec4
  , project , project' , projectUnsafe , flipNormal
  , householder, householderOrtho
  )
  where

-- Modernizing
import GHC.Generics
import Data.Binary 
-- Modernizing

import Control.Monad
import System.Random  
import Foreign


--------------------------------------------------------------------------------
-- class declarations

class AbelianGroup g where
  (&+) :: g -> g -> g
  (&-) :: g -> g -> g
  neg  :: g -> g
  zero :: g

infixl 6 &+
infixl 6 &- 

vecSum :: AbelianGroup g => [g] -> g
vecSum l = foldl (&+) zero l 

class MultSemiGroup r where
  (.*.) :: r -> r -> r
  one   :: r

class (AbelianGroup r, MultSemiGroup r) => Ring r 

infixl 7 .*. 

-- was: ringProduct :: Ring r => [r] -> r
semigroupProduct :: MultSemiGroup r => [r] -> r 
semigroupProduct l = foldl (.*.) one l

class LeftModule r m where
  lmul :: r -> m -> m
  (*.) :: r -> m -> m
  (*.) = lmul

class RightModule m r where
  rmul :: m -> r -> m
  (.*) :: m -> r -> m
  (.*) = rmul

-- I'm not really sure about this.. may actually degrade the performance in some cases?  
{- RULES
"matrix multiplication left"   forall m n x.  (n .*. m) *. x = n *. (m *. x)  
"matrix multiplication right"  forall m n x.  x .* (m .*. n) = (x .* m) .* n
  -}

infixr 7 *.
infixl 7 .*

class AbelianGroup v => Vector v where
  mapVec    :: (Flt -> Flt) -> v -> v
  scalarMul :: Flt -> v -> v
  (*&) ::      Flt -> v -> v 
  (&*) ::      v -> Flt -> v 
  (*&) s v = scalarMul s v
  (&*) v s = scalarMul s v

infixr 7 *&
infixl 7 &*

{-# RULES
"scalar multiplication left"   forall s t x.  t *& (s *& x) = (t*s) *& x 
"scalar multiplication right"  forall s t x.  (x &* s) &* t = x &* (s*t)  
  #-}

class DotProd v where
  (&.) :: v -> v -> Flt
  norm    :: v -> Flt
  normsqr :: v -> Flt
  len     :: v -> Flt
  lensqr  :: v -> Flt
  len = norm
  lensqr = normsqr
  dotprod :: v -> v -> Flt
  normsqr v = (v &. v)  
  norm = sqrt . lensqr
  dotprod = (&.)

infix 7 &.

{-# RULES
"len/square 1"   forall x.  (len x)*(len x) = lensqr x
"len/square 2"   forall x.  (len x)^2 = lensqr x
"norm/square 1"  forall x.  (norm x)*(norm x) = normsqr x
"norm/square 2"  forall x.  (norm x)^2 = normsqr x
  #-}

normalize :: (Vector v, DotProd v) => v -> v
normalize v = scalarMul (1.0/(len v)) v

distance :: (Vector v, DotProd v) => v -> v -> Flt
distance x y = norm (x &- y)

-- | the angle between two vectors
angle :: (Vector v, DotProd v) => v -> v -> Flt 
angle x y = acos $ (x &. y) / (norm x * norm y)

-- | the angle between two unit vectors
angle' {- ' CPP is sensitive to primes -} :: (Vector v, UnitVector v u, DotProd v) => u -> u -> Flt 
angle' x y = acos (fromNormal x &. fromNormal y)

{-# RULES
"normalize is idempotent"  forall x. normalize (normalize x) = normalize x
  #-}

class (Vector v, DotProd v) => UnitVector v u | v->u, u->v  where
  mkNormal         :: v -> u       -- ^ normalizes the input
  toNormalUnsafe   :: v -> u       -- ^ does not normalize the input!
  fromNormal       :: u -> v
  fromNormalRadius :: Flt -> u -> v
  fromNormalRadius t n = t *& fromNormal n 

-- | Projects the first vector down to the hyperplane orthogonal to the second (unit) vector
project' :: (Vector v, UnitVector v u, DotProd v) => v -> u -> v
project' what dir = projectUnsafe what (fromNormal dir)

-- | Direction (second argument) is assumed to be a /unit/ vector!
projectUnsafe :: (Vector v, DotProd v) => v -> v -> v
projectUnsafe what dir = what &- dir &* (what &. dir)

project :: (Vector v, DotProd v) => v -> v -> v
project what dir = what &- dir &* ((what &. dir) / (dir &. dir))

-- | Since unit vectors are not a group, we need a separate function.
flipNormal :: UnitVector v n => n -> n 
flipNormal = toNormalUnsafe . neg . fromNormal 

-- | Cross product
class CrossProd v where
  crossprod :: v -> v -> v
  (&^)      :: v -> v -> v
  (&^) = crossprod
 
-- | Pointwise multiplication 
class Pointwise v where
  pointwise :: v -> v -> v
  (&!)      :: v -> v -> v
  (&!) = pointwise 

infix 7 &^
infix 7 &!

class HasCoordinates v x | v->x where
  _1 :: v -> x
  _2 :: v -> x
  _3 :: v -> x
  _4 :: v -> x      

-- | conversion between vectors (and matrices) of different dimensions
class Extend u v where
  extendZero :: u -> v          -- ^ example: @extendZero (Vec2 5 6) = Vec4 5 6 0 0@
  extendWith :: Flt -> u -> v   -- ^ example: @extendWith 1 (Vec2 5 6) = Vec4 5 6 1 1@
  trim :: v -> u                -- ^ example: @trim (Vec4 5 6 7 8) = Vec2 5 6@

-- | makes a diagonal matrix from a vector
class Diagonal s t | t->s where
  diag :: s -> t

class Matrix m where
  transpose :: m -> m 
  inverse :: m -> m
  idmtx :: m

{-# RULES
"transpose is an involution"  forall m. transpose (transpose m) = m
"inverse is an involution"    forall m. inverse (inverse m) = m
  #-}
  
class Matrix m => Orthogonal m o | m->o, o->m where  
  fromOrtho     :: o -> m 
  toOrthoUnsafe :: m -> o
  
class (AbelianGroup m, Matrix m) => MatrixNorms m where
  frobeniusNorm  :: m -> Flt       -- ^ the frobenius norm (= euclidean norm in the space of matrices)
  matrixDistance :: m -> m -> Flt  -- ^ euclidean distance in the space of matrices
  operatorNorm   :: m -> Flt       -- ^ (euclidean) operator norm (not implemented yet)
  matrixDistance m n = frobeniusNorm (n &- m)
  operatorNorm = error "operatorNorm: not implemented yet"
  
-- | Outer product (could be unified with Diagonal?)
class Tensor t v | t->v where
  outer :: v -> v -> t
    
class Determinant m where
  det :: m -> Flt    

class Dimension a where
  dim :: a -> Int
     
-- | Householder matrix, see <http://en.wikipedia.org/wiki/Householder_transformation>.  
-- In plain words, it is the reflection to the hyperplane orthogonal to the input vector.
householder :: (Vector v, UnitVector v u, Matrix m, Vector m, Tensor m v) => u -> m
householder u = idmtx &- (2 *& outer v v) 
  where v = fromNormal u

householderOrtho :: (Vector v, UnitVector v u, Matrix m, Vector m, Tensor m v, Orthogonal m o) => u -> o
householderOrtho = toOrthoUnsafe . householder

-- | \"Projective\" matrices have the following form: the top left corner
-- is an any matrix, the bottom right corner is 1, and the top-right
-- column is zero. These describe the affine orthogonal transformation of
-- the space one dimension less.
class (Vector v, Orthogonal n o, Diagonal v n) => Projective v n o m p 
    | m->p, p->m, p->o, o->p, p->n, n->p, p->v, v->p, n->o, n->v, v->n where
  fromProjective     :: p -> m
  toProjectiveUnsafe :: m -> p
  orthogonal         :: o -> p
  linear             :: n -> p
  translation        :: v -> p
  scaling            :: v -> p

--------------------------------------------------------------------------------
-- Vec / Mat datatypes
 
data Vec2 = Vec2 {-# UNPACK #-} !Flt {-# UNPACK #-} !Flt 
  deriving (Read,Show, Generic, Binary)
data Vec3 = Vec3 {-# UNPACK #-} !Flt {-# UNPACK #-} !Flt {-# UNPACK #-} !Flt 
  deriving (Read,Show, Generic, Binary)
data Vec4 = Vec4 {-# UNPACK #-} !Flt {-# UNPACK #-} !Flt {-# UNPACK #-} !Flt {-# UNPACK #-} !Flt 
  deriving (Read,Show, Generic, Binary)

-- | The components are /row/ vectors 
data Mat2 = Mat2 !Vec2 !Vec2              deriving (Read,Show, Generic)
data Mat3 = Mat3 !Vec3 !Vec3 !Vec3        deriving (Read,Show, Generic)
data Mat4 = Mat4 !Vec4 !Vec4 !Vec4 !Vec4  deriving (Read,Show, Generic)

-- | The assumption when dealing with these is always that they are of unit length.
-- Also, interpolation works differently.
newtype Normal2 = Normal2 Vec2 deriving (Read,Show,Storable,DotProd,Dimension, Generic) 
newtype Normal3 = Normal3 Vec3 deriving (Read,Show,Storable,DotProd,Dimension, Generic) 
newtype Normal4 = Normal4 Vec4 deriving (Read,Show,Storable,DotProd,Dimension, Generic) 

mkVec2 :: (Flt,Flt) -> Vec2
mkVec3 :: (Flt,Flt,Flt) -> Vec3
mkVec4 :: (Flt,Flt,Flt,Flt) -> Vec4

mkVec2 (x,y)     = Vec2 x y 
mkVec3 (x,y,z)   = Vec3 x y z
mkVec4 (x,y,z,w) = Vec4 x y z w

-- | Orthogonal matrices.
--
-- Note: the "Random" instances generates orthogonal matrices with determinant 1
-- (that is, orientation-preserving orthogonal transformations)!
newtype Ortho2 = Ortho2 Mat2 deriving (Read,Show,Storable,MultSemiGroup,Determinant,Dimension)
newtype Ortho3 = Ortho3 Mat3 deriving (Read,Show,Storable,MultSemiGroup,Determinant,Dimension)
newtype Ortho4 = Ortho4 Mat4 deriving (Read,Show,Storable,MultSemiGroup,Determinant,Dimension)

-- | Projective matrices, encoding affine transformations in dimension one less.
newtype Proj3 = Proj3 Mat3 deriving (Read,Show,Storable,MultSemiGroup)
newtype Proj4 = Proj4 Mat4 deriving (Read,Show,Storable,MultSemiGroup)

--------------------------------------------------------------------------------
-- Unit vectors
  
instance UnitVector Vec2 Normal2 where
  mkNormal v = Normal2 (normalize v)
  fromNormal (Normal2 v) = v 
  toNormalUnsafe = Normal2

instance UnitVector Vec3 Normal3 where
  mkNormal v = Normal3 (normalize v)
  fromNormal (Normal3 v) = v 
  toNormalUnsafe = Normal3

instance UnitVector Vec4 Normal4 where
  mkNormal v = Normal4 (normalize v)
  fromNormal (Normal4 v) = v 
  toNormalUnsafe = Normal4

_rndUnit :: (RandomGen g, Random v, Vector v, DotProd v) => g -> (v,g)
_rndUnit g = 
  if d > 0.01
    then ( v &* (1.0/d) , h )
    else _rndUnit h
  where
    (v,h) = random g
    d = norm v
    
instance Random Normal2 where
  random g = let (v,h) = _rndUnit g in (Normal2 v, h)  
  randomR _ = random

instance Random Normal3 where
  random g = let (v,h) = _rndUnit g in (Normal3 v, h)  
  randomR _ = random

instance Random Normal4 where
  random g = let (v,h) = _rndUnit g in (Normal4 v, h)  
  randomR _ = random

instance CrossProd Normal3 where
  crossprod (Normal3 v) (Normal3 w) = mkNormal (crossprod v w)

--------------------------------------------------------------------------------
-- Orthogonal matrices

instance Orthogonal Mat2 Ortho2 where
  fromOrtho (Ortho2 o) = o
  toOrthoUnsafe = Ortho2

instance Orthogonal Mat3 Ortho3 where
  fromOrtho (Ortho3 o) = o
  toOrthoUnsafe = Ortho3 

instance Orthogonal Mat4 Ortho4 where
  fromOrtho (Ortho4 o) = o
  toOrthoUnsafe = Ortho4

------

instance Matrix Ortho2 where
  transpose (Ortho2 o) = Ortho2 (transpose o)
  idmtx = Ortho2 idmtx
  inverse = transpose

instance Matrix Ortho3 where
  transpose (Ortho3 o) = Ortho3 (transpose o)
  idmtx = Ortho3 idmtx
  inverse = transpose

instance Matrix Ortho4 where
  transpose (Ortho4 o) = Ortho4 (transpose o)
  idmtx = Ortho4 idmtx
  inverse = transpose

------

instance Random Ortho2 where
  random g = let (o,h) = _rndOrtho2 g in (toOrthoUnsafe (_flip1stRow2 o), h)
  randomR _ = random

instance Random Ortho3 where
  random g = let (o,h) = _rndOrtho3 g in (toOrthoUnsafe (             o), h)
  randomR _ = random

instance Random Ortho4 where
  random g = let (o,h) = _rndOrtho4 g in (toOrthoUnsafe (_flip1stRow4 o), h)
  randomR _ = random

------

-- determinant will be -1
_rndOrtho2 :: RandomGen g => g -> (Mat2, g)
_rndOrtho2 g = (h2, g1) where
  h2 = householder u2 :: Mat2 
  (u2,g1) = random g   

-- generates a uniformly random orthogonal 3x3 matrix 
-- /with determinant +1/, with respect to the Haar measure of SO3.
--
-- see Theorem 4 in:
-- Francesco Mezzadri: How to Generate Random Matrices from the Classical Compact Groups 
-- Notices of the AMS, May 2007 issue
-- <http://www.ams.org/notices/200705/fea-mezzadri-web.ps>
_rndOrtho3 :: RandomGen g => g -> (Mat3, g) 
_rndOrtho3 g = ( (h3 .*. m3), g2) where
  m3 = (extendWith :: Flt -> Mat2 -> Mat3) 1 o2 
  h3 = householder u3 :: Mat3
  (u3,g1) = random g
  (o2,g2) = _rndOrtho2 g1

-- determinant will be -1
_rndOrtho4 :: RandomGen g => g -> (Mat4, g) 
_rndOrtho4 g = ( (h4 .*. m4), g2) where
  m4 = (extendWith :: Flt -> Mat3 -> Mat4) 1 o3 
  h4 = householder u4 :: Mat4
  (u4,g1) = random g
  (o3,g2) = _rndOrtho3 g1

------

_flip1stRow2 :: Mat2 -> Mat2
_flip1stRow2 (Mat2 a b) = Mat2 (neg a) b

_flip1stRow3 :: Mat3 -> Mat3
_flip1stRow3 (Mat3 a b c) = Mat3 (neg a) b c

_flip1stRow4 :: Mat4 -> Mat4
_flip1stRow4 (Mat4 a b c d) = Mat4 (neg a) b c d

--------------------------------------------------------------------------------
-- projective matrices
  
instance Projective Vec2 Mat2 Ortho2 Mat3 Proj3 where
  fromProjective (Proj3 m) = m
  toProjectiveUnsafe = Proj3
  orthogonal = Proj3 . extendWith 1 . fromOrtho
  linear     = Proj3 . extendWith 1
  translation v = Proj3 $ Mat3 (Vec3 1 0 0) (Vec3 0 1 0) (extendWith 1 v)
  scaling     v = Proj3 $ diag (extendWith 1 v)
  
instance Projective Vec3 Mat3 Ortho3 Mat4 Proj4 where
  fromProjective (Proj4 m) = m
  toProjectiveUnsafe = Proj4
  orthogonal = Proj4 . extendWith 1 . fromOrtho 
  linear     = Proj4 . extendWith 1
  translation v = Proj4 $ Mat4 (Vec4 1 0 0 0) (Vec4 0 1 0 0) (Vec4 0 0 1 0) (extendWith 1 v)
  scaling     v = Proj4 $ diag (extendWith 1 v)

instance Matrix Proj3 where
  idmtx = Proj3 idmtx
  transpose (Proj3 m) = Proj3 (transpose m)
  inverse = _invertProj3

instance Matrix Proj4 where
  idmtx = Proj4 idmtx
  transpose (Proj4 m) = Proj4 (transpose m)
  inverse = _invertProj4

_invertProj3 :: Proj3 -> Proj3
_invertProj3 (Proj3 mat@(Mat3 _ _ t)) = 
  Proj3 $ Mat3 (extendZero a) (extendZero b) (extendWith 1 t') 
  where
    t' = neg $ (trim t :: Vec2) .* invm2 
    invm2@(Mat2 a b) = inverse $ (trim mat :: Mat2)

-- Inverts a projective 4x4 matrix. But you can simply use "inverse" instead.
-- We assume that the bottom-right corner is 1.
_invertProj4 :: Proj4 -> Proj4
_invertProj4 (Proj4 mat@(Mat4 _ _ _ t)) = 
  Proj4 $ Mat4 (extendZero a) (extendZero b) (extendZero c) (extendWith 1 t') 
  where
    t' = neg $ (trim t :: Vec3) .* invm3 
    invm3@(Mat3 a b c) = inverse $ (trim mat :: Mat3)

--------------------------------------------------------------------------------
-- Vec2 instances

instance HasCoordinates Vec2 Flt where
  _1 (Vec2 x _) = x
  _2 (Vec2 _ y) = y
  _3 _ = error "has only 2 coordinates"
  _4 _ = error "has only 2 coordinates"

instance AbelianGroup Vec2 where
  (&+) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1+x2) (y1+y2) 
  (&-) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1-x2) (y1-y2)
  neg  (Vec2 x y)                = Vec2 (-x) (-y)
  zero = Vec2 0 0
  
instance Vector Vec2 where
  scalarMul s (Vec2 x y) = Vec2 (s*x) (s*y)
  mapVec    f (Vec2 x y) = Vec2 (f x) (f y)
  
instance DotProd Vec2 where
  (&.) (Vec2 x1 y1) (Vec2 x2 y2) = x1*x2 + y1*y2

instance Pointwise Vec2 where
  pointwise (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1*x2) (y1*y2)

instance Determinant (Vec2,Vec2) where
  det (Vec2 x1 y1 , Vec2 x2 y2) = x1*y2 - x2*y1  

{-     
instance Show Vec2 where
  show (Vec2 x y) = "( " ++ show x ++ " , " ++ show y ++ " )"
-}

instance Random Vec2 where
  random = randomR (Vec2 (-1) (-1),Vec2 1 1)
  randomR (Vec2 a b, Vec2 c d) gen = 
    let (x,gen1) = randomR (a,c) gen
        (y,gen2) = randomR (b,d) gen1
    in (Vec2 x y, gen2)
     
instance Storable Vec2 where
  sizeOf    _ = 2 * sizeOf (undefined::Flt)
  alignment _ = sizeOf (undefined::Flt)
  
  peek q = do
    let p = castPtr q :: Ptr Flt
        k = sizeOf (undefined::Flt)
    x <- peek        p 
    y <- peekByteOff p k
    return (Vec2 x y)
    
  poke q (Vec2 x y) = do
    let p = castPtr q :: Ptr Flt
        k = sizeOf (undefined::Flt)
    poke        p   x
    pokeByteOff p k y

instance Dimension Vec2 where dim _ = 2

--------------------------------------------------------------------------------                    
-- Mat2 instances

instance HasCoordinates Mat2 Vec2 where
  _1 (Mat2 x _) = x
  _2 (Mat2 _ y) = y
  _3 _ = error "has only 2 coordinates"
  _4 _ = error "has only 2 coordinates"

instance Matrix Mat2 where
  transpose (Mat2 row1 row2) = 
    Mat2 (Vec2 (_1 row1) (_1 row2)) 
         (Vec2 (_2 row1) (_2 row2)) 
  idmtx = Mat2 (Vec2 1 0) (Vec2 0 1)
  inverse (Mat2 (Vec2 a b) (Vec2 c d)) = 
    Mat2 (Vec2 (d*r) (-b*r)) (Vec2 (-c*r) (a*r)) 
    where r = 1.0 / (a*d - b*c)

instance AbelianGroup Mat2 where
  (&+) (Mat2 r1 r2) (Mat2 s1 s2) = Mat2 (r1 &+ s1) (r2 &+ s2)
  (&-) (Mat2 r1 r2) (Mat2 s1 s2) = Mat2 (r1 &- s1) (r2 &- s2)
  neg  (Mat2 r1 r2)              = Mat2 (neg r1) (neg r2)  
  zero = Mat2 zero zero  
  
instance Vector Mat2 where
  scalarMul s (Mat2 r1 r2) = Mat2 (g r1) (g r2) where g = scalarMul s
  mapVec    f (Mat2 r1 r2) = Mat2 (g r1) (g r2) where g = mapVec f

instance MultSemiGroup Mat2 where
  (.*.) (Mat2 r1 r2) n = 
    let (Mat2 c1 c2) = transpose n
    in Mat2 (Vec2 (r1 &. c1) (r1 &. c2))
            (Vec2 (r2 &. c1) (r2 &. c2))
  one = idmtx 

instance Ring Mat2

instance LeftModule Mat2 Vec2 where
  lmul (Mat2 row1 row2) v = Vec2 (row1 &. v) (row2 &. v) 
  
instance RightModule Vec2 Mat2 where
  rmul v mt = lmul (transpose mt) v

instance Diagonal Vec2 Mat2 where
  diag (Vec2 x y) = Mat2 (Vec2 x 0) (Vec2 0 y)

instance Tensor Mat2 Vec2 where
  outer (Vec2 a b) (Vec2 x y) = Mat2
    (Vec2 (a*x) (a*y))
    (Vec2 (b*x) (b*y))

instance Determinant Mat2 where
  det (Mat2 (Vec2 a b) (Vec2 c d)) = a*d - b*c 

{-
instance Show Mat2 where
  show (Mat2 r1 r2) = show r1 ++ "\n" ++ show r2
-}

instance Storable Mat2 where
  sizeOf    _ = 2 * sizeOf (undefined::Vec2)
  alignment _ = alignment  (undefined::Vec2)
  
  peek q = do
    let p = castPtr q :: Ptr Vec2
        k = sizeOf (undefined::Vec2)
    r1 <- peek        p 
    r2 <- peekByteOff p k
    return (Mat2 r1 r2)
    
  poke q (Mat2 r1 r2) = do
    let p = castPtr q :: Ptr Vec2
        k = sizeOf (undefined::Vec2)
    poke        p   r1
    pokeByteOff p k r2

instance Random Mat2 where
  random = randomR (Mat2 v1 v1 , Mat2 v2 v2) where 
    v1 = Vec2 (-1) (-1) 
    v2 = Vec2   1    1
  randomR (Mat2 a b, Mat2 c d) gen = 
    let (x,gen1) = randomR (a,c) gen
        (y,gen2) = randomR (b,d) gen1
    in (Mat2 x y, gen2)
          
instance Dimension Mat2 where dim _ = 2
     
instance MatrixNorms Mat2 where 
  frobeniusNorm (Mat2 r1 r2) =  
    sqrt $
      normsqr r1 + 
      normsqr r2
     
instance Pointwise Mat2 where
  pointwise (Mat2 x1 y1) (Mat2 x2 y2) = Mat2 (x1 &! x2) (y1 &! y2)
       
--------------------------------------------------------------------------------     
-- Vec3 instances

instance HasCoordinates Vec3 Flt where
  _1 (Vec3 x _ _) = x
  _2 (Vec3 _ y _) = y
  _3 (Vec3 _ _ z) = z
  _4 _ = error "has only 3 coordinates"

instance AbelianGroup Vec3 where
  (&+) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1+x2) (y1+y2) (z1+z2) 
  (&-) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1-x2) (y1-y2) (z1-z2) 
  neg  (Vec3 x y z)                    = Vec3 (-x) (-y) (-z)
  zero = Vec3 0 0 0
  
instance Vector Vec3 where
  scalarMul s (Vec3 x y z) = Vec3 (s*x) (s*y) (s*z)
  mapVec    f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance DotProd Vec3 where
  (&.) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

instance Pointwise Vec3 where
  pointwise (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1*x2) (y1*y2) (z1*z2)

{-
instance Show Vec3 where
  show (Vec3 x y z) = "( " ++ show x ++ " , " ++ show y ++ " , " ++ show z ++ " )"
-}

instance Random Vec3 where
  random = randomR (Vec3 (-1) (-1) (-1),Vec3 1 1 1)
  randomR (Vec3 a b c, Vec3 d e f) gen = 
    let (x,gen1) = randomR (a,d) gen
        (y,gen2) = randomR (b,e) gen1
        (z,gen3) = randomR (c,f) gen2  
    in (Vec3 x y z, gen3)
      
instance CrossProd Vec3 where
  crossprod (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (y1*z2-y2*z1) (z1*x2-z2*x1) (x1*y2-x2*y1) 

instance Determinant (Vec3,Vec3,Vec3) where
  det (u,v,w) = u &. (v &^ w)  
 
instance Storable Vec3 where
  sizeOf    _ = 3 * sizeOf (undefined::Flt)
  alignment _ = sizeOf (undefined::Flt)
  
  peek q = do
    let p = castPtr q :: Ptr Flt
        k = sizeOf (undefined::Flt)
    x <- peek        p 
    y <- peekByteOff p (k  )
    z <- peekByteOff p (k+k)
    return (Vec3 x y z)
    
  poke q (Vec3 x y z) = do
    let p = castPtr q :: Ptr Flt
        k = sizeOf (undefined::Flt)
    poke        p       x
    pokeByteOff p (k  ) y
    pokeByteOff p (k+k) z

instance Dimension Vec3 where dim _ = 3

--------------------------------------------------------------------------------   
-- Mat3 instances

instance HasCoordinates Mat3 Vec3 where
  _1 (Mat3 x _ _) = x
  _2 (Mat3 _ y _) = y
  _3 (Mat3 _ _ z) = z
  _4 _ = error "has only 3 coordinates"  

instance Matrix Mat3 where

  transpose (Mat3 row1 row2 row3) = 
    Mat3 (Vec3 (_1 row1) (_1 row2) (_1 row3)) 
         (Vec3 (_2 row1) (_2 row2) (_2 row3)) 
         (Vec3 (_3 row1) (_3 row2) (_3 row3)) 
         
  idmtx = Mat3 (Vec3 1 0 0) (Vec3 0 1 0) (Vec3 0 0 1)
  
  inverse (Mat3 (Vec3 a b c) (Vec3 e f g) (Vec3 i j k)) = 
    Mat3 (Vec3 (d11*r) (d21*r) (d31*r))  
         (Vec3 (d12*r) (d22*r) (d32*r))  
         (Vec3 (d13*r) (d23*r) (d33*r))  
    where
      r = 1.0 / ( a*d11 + b*d12 + c*d13 )

      d11 = f*k - g*j
      d12 = g*i - e*k
      d13 = e*j - f*i

      d31 = b*g - c*f
      d32 = c*e - a*g
      d33 = a*f - b*e

      d21 = c*j - b*k 
      d22 = a*k - c*i 
      d23 = b*i - a*j 

instance AbelianGroup Mat3 where
  (&+) (Mat3 r1 r2 r3) (Mat3 s1 s2 s3) = Mat3 (r1 &+ s1) (r2 &+ s2) (r3 &+ s3)
  (&-) (Mat3 r1 r2 r3) (Mat3 s1 s2 s3) = Mat3 (r1 &- s1) (r2 &- s2) (r3 &- s3)
  neg  (Mat3 r1 r2 r3)                 = Mat3 (neg r1) (neg r2) (neg r3) 
  zero = Mat3 zero zero zero 

instance Vector Mat3 where
  scalarMul s (Mat3 r1 r2 r3) = Mat3 (g r1) (g r2) (g r3) where g = scalarMul s
  mapVec    f (Mat3 r1 r2 r3) = Mat3 (g r1) (g r2) (g r3) where g = mapVec f

instance MultSemiGroup Mat3 where
  (.*.) (Mat3 r1 r2 r3) n = 
    let (Mat3 c1 c2 c3) = transpose n
    in Mat3 (Vec3 (r1 &. c1) (r1 &. c2) (r1 &. c3))
            (Vec3 (r2 &. c1) (r2 &. c2) (r2 &. c3))
            (Vec3 (r3 &. c1) (r3 &. c2) (r3 &. c3))
  one = idmtx 

instance Ring Mat3

instance LeftModule Mat3 Vec3 where
  lmul (Mat3 row1 row2 row3) v = Vec3 (row1 &. v) (row2 &. v) (row3 &. v)
  
instance RightModule Vec3 Mat3 where
  rmul v mt = lmul (transpose mt) v

instance Diagonal Vec3 Mat3 where
  diag (Vec3 x y z) = Mat3 (Vec3 x 0 0) (Vec3 0 y 0) (Vec3 0 0 z)

instance Tensor Mat3 Vec3 where
  outer (Vec3 a b c) (Vec3 x y z) = Mat3
    (Vec3 (a*x) (a*y) (a*z))
    (Vec3 (b*x) (b*y) (b*z))
    (Vec3 (c*x) (c*y) (c*z))

instance Determinant Mat3 where
  det (Mat3 r1 r2 r3) = det (r1,r2,r3)

{-
instance Show Mat3 where
  show (Mat3 r1 r2 r3) = show r1 ++ "\n" ++ show r2 ++ "\n" ++ show r3
-}

instance Storable Mat3 where
  sizeOf    _ = 3 * sizeOf (undefined::Vec3)
  alignment _ = alignment  (undefined::Vec3)
  
  peek q = do
    let p = castPtr q :: Ptr Vec3
        k = sizeOf (undefined::Vec3)
    r1 <- peek        p 
    r2 <- peekByteOff p (k  )
    r3 <- peekByteOff p (k+k)
    return (Mat3 r1 r2 r3)
    
  poke q (Mat3 r1 r2 r3) = do
    let p = castPtr q :: Ptr Vec3
        k = sizeOf (undefined::Vec3)
    poke        p       r1
    pokeByteOff p (k  ) r2
    pokeByteOff p (k+k) r3

instance Random Mat3 where
  random = randomR (Mat3 v1 v1 v1 , Mat3 v2 v2 v2) where
    v1 = Vec3 (-1) (-1) (-1)
    v2 = Vec3   1    1    1
  randomR (Mat3 a b c, Mat3 d e f) gen = 
    let (x,gen1) = randomR (a,d) gen
        (y,gen2) = randomR (b,e) gen1
        (z,gen3) = randomR (c,f) gen2  
    in (Mat3 x y z, gen3)
   
instance Dimension Mat3 where dim _ = 3
  
instance MatrixNorms Mat3 where 
  frobeniusNorm (Mat3 r1 r2 r3)  = 
    sqrt $
      normsqr r1 + 
      normsqr r2 + 
      normsqr r3 

instance Pointwise Mat3 where
  pointwise (Mat3 x1 y1 z1) (Mat3 x2 y2 z2) = Mat3 (x1 &! x2) (y1 &! y2) (z1 &! z2)
    
--------------------------------------------------------------------------------
-- Vec4 instances

instance HasCoordinates Vec4 Flt where
  _1 (Vec4 x _ _ _) = x
  _2 (Vec4 _ y _ _) = y
  _3 (Vec4 _ _ z _) = z
  _4 (Vec4 _ _ _ w) = w

instance AbelianGroup Vec4 where
  (&+) (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (x1+x2) (y1+y2) (z1+z2) (w1+w2)
  (&-) (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (x1-x2) (y1-y2) (z1-z2) (w1-w2)
  neg  (Vec4 x y z w)                        = Vec4 (-x) (-y) (-z) (-w)
  zero = Vec4 0 0 0 0
  
instance Vector Vec4 where
  scalarMul s (Vec4 x y z w) = Vec4 (s*x) (s*y) (s*z) (s*w)
  mapVec    f (Vec4 x y z w) = Vec4 (f x) (f y) (f z) (f w)

instance DotProd Vec4 where
  (&.) (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = x1*x2 + y1*y2 + z1*z2 + w1*w2

instance Pointwise Vec4 where
  pointwise (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (x1*x2) (y1*y2) (z1*z2) (w1*w2)

{-
instance Show Vec4 where
  show (Vec4 x y z w) = "( " ++ show x ++ " , " ++ show y ++ " , " ++ show z ++ " , " ++ show w ++ " )"
-}

instance Random Vec4 where
  random = randomR (Vec4 (-1) (-1) (-1) (-1),Vec4 1 1 1 1)
  randomR (Vec4 a b c d, Vec4 e f g h) gen = 
    let (x,gen1) = randomR (a,e) gen
        (y,gen2) = randomR (b,f) gen1
        (z,gen3) = randomR (c,g) gen2  
        (w,gen4) = randomR (d,h) gen3  
    in (Vec4 x y z w, gen4)
           
instance Storable Vec4 where
  sizeOf    _ = 4 * sizeOf (undefined::Flt)
  alignment _ = sizeOf (undefined::Flt)
  
  peek q = do
    let p = castPtr q :: Ptr Flt
        k = sizeOf (undefined::Flt)
    x <- peek        p 
    y <- peekByteOff p (k  )
    z <- peekByteOff p (k+k)
    w <- peekByteOff p (3*k)
    return (Vec4 x y z w)
    
  poke q (Vec4 x y z w) = do
    let p = castPtr q :: Ptr Flt
        k = sizeOf (undefined::Flt)
    poke        p       x
    pokeByteOff p (k  ) y
    pokeByteOff p (k+k) z
    pokeByteOff p (3*k) w

instance Dimension Vec4 where dim _ = 4

--------------------------------------------------------------------------------
-- Mat4 instances

instance HasCoordinates Mat4 Vec4 where
  _1 (Mat4 x _ _ _) = x
  _2 (Mat4 _ y _ _) = y
  _3 (Mat4 _ _ z _) = z
  _4 (Mat4 _ _ _ w) = w

instance Matrix Mat4 where
  transpose (Mat4 row1 row2 row3 row4) = 
    Mat4 (Vec4 (_1 row1) (_1 row2) (_1 row3) (_1 row4)) 
         (Vec4 (_2 row1) (_2 row2) (_2 row3) (_2 row4)) 
         (Vec4 (_3 row1) (_3 row2) (_3 row3) (_3 row4)) 
         (Vec4 (_4 row1) (_4 row2) (_4 row3) (_4 row4)) 
  idmtx = Mat4 (Vec4 1 0 0 0) (Vec4 0 1 0 0) (Vec4 0 0 1 0) (Vec4 0 0 0 1)
  inverse = error "inverse/Mat4: not implemented yet"

instance AbelianGroup Mat4 where
  (&+) (Mat4 r1 r2 r3 r4) (Mat4 s1 s2 s3 s4) = Mat4 (r1 &+ s1) (r2 &+ s2) (r3 &+ s3) (r4 &+ s4)
  (&-) (Mat4 r1 r2 r3 r4) (Mat4 s1 s2 s3 s4) = Mat4 (r1 &- s1) (r2 &- s2) (r3 &- s3) (r4 &- s4)
  neg  (Mat4 r1 r2 r3 r4)                    = Mat4 (neg r1) (neg r2) (neg r3) (neg r4) 
  zero = Mat4 zero zero zero zero
  
instance Vector Mat4 where
  scalarMul s (Mat4 r1 r2 r3 r4) = Mat4 (g r1) (g r2) (g r3) (g r4) where g = scalarMul s
  mapVec    f (Mat4 r1 r2 r3 r4) = Mat4 (g r1) (g r2) (g r3) (g r4) where g = mapVec f

instance MultSemiGroup Mat4 where
  (.*.) (Mat4 r1 r2 r3 r4) n = 
    let (Mat4 c1 c2 c3 c4) = transpose n
    in Mat4 (Vec4 (r1 &. c1) (r1 &. c2) (r1 &. c3) (r1 &. c4))
            (Vec4 (r2 &. c1) (r2 &. c2) (r2 &. c3) (r2 &. c4))
            (Vec4 (r3 &. c1) (r3 &. c2) (r3 &. c3) (r3 &. c4))
            (Vec4 (r4 &. c1) (r4 &. c2) (r4 &. c3) (r4 &. c4))
  one = idmtx 

instance Ring Mat4

instance LeftModule Mat4 Vec4 where
  lmul (Mat4 row1 row2 row3 row4) v = Vec4 (row1 &. v) (row2 &. v) (row3 &. v) (row4 &. v)
  
instance RightModule Vec4 Mat4 where
  rmul v mt = lmul (transpose mt) v

instance Diagonal Vec4 Mat4 where
  diag (Vec4 x y z w) = Mat4 (Vec4 x 0 0 0) (Vec4 0 y 0 0) (Vec4 0 0 z 0) (Vec4 0 0 0 w)

instance Tensor Mat4 Vec4 where
  outer (Vec4 a b c d) (Vec4 x y z w) = Mat4
    (Vec4 (a*x) (a*y) (a*z) (a*w))
    (Vec4 (b*x) (b*y) (b*z) (b*w))
    (Vec4 (c*x) (c*y) (c*z) (c*w))
    (Vec4 (d*x) (d*y) (d*z) (d*w))

instance Determinant Mat4 where
  det = error "det/Mat4: not implemented yet" 
  -- det (Mat4 r1 r2 r3 r4) = 

{-
instance Show Mat4 where
  show (Mat4 r1 r2 r3 r4) = show r1 ++ "\n" ++ show r2 ++ "\n" ++ show r3 ++ "\n" ++ show r4
-}

instance Storable Mat4 where
  sizeOf    _ = 4 * sizeOf (undefined::Vec4)
  alignment _ = alignment  (undefined::Vec4)
  
  peek q = do
    let p = castPtr q :: Ptr Vec4
        k = sizeOf (undefined::Vec4)
    r1 <- peek        p 
    r2 <- peekByteOff p (k  )
    r3 <- peekByteOff p (k+k)
    r4 <- peekByteOff p (3*k)
    return (Mat4 r1 r2 r3 r4)
    
  poke q (Mat4 r1 r2 r3 r4) = do
    let p = castPtr q :: Ptr Vec4
        k = sizeOf (undefined::Vec4)
    poke        p       r1
    pokeByteOff p (k  ) r2
    pokeByteOff p (k+k) r3
    pokeByteOff p (3*k) r4

instance Random Mat4 where
  random = randomR (Mat4 v1 v1 v1 v1, Mat4 v2 v2 v2 v2) where
    v1 = Vec4 (-1) (-1) (-1) (-1)
    v2 = Vec4   1    1    1    1
  randomR (Mat4 a b c d, Mat4 e f g h) gen = 
    let (x,gen1) = randomR (a,e) gen
        (y,gen2) = randomR (b,f) gen1
        (z,gen3) = randomR (c,g) gen2  
        (w,gen4) = randomR (d,h) gen3  
    in (Mat4 x y z w, gen4)
    
instance Dimension Mat4 where dim _ = 4
   
instance MatrixNorms Mat4 where 
  frobeniusNorm (Mat4 r1 r2 r3 r4) = 
    sqrt $
      normsqr r1 + 
      normsqr r2 + 
      normsqr r3 + 
      normsqr r4  
    
instance Pointwise Mat4 where
  pointwise (Mat4 x1 y1 z1 w1) (Mat4 x2 y2 z2 w2) = Mat4 (x1 &! x2) (y1 &! y2) (z1 &! z2) (w1 &! w2)
    
--------------------------------------------------------------------------------
-- Extend instances

instance Extend Vec2 Vec3 where
  extendZero   (Vec2 x y) = Vec3 x y 0
  extendWith t (Vec2 x y) = Vec3 x y t
  trim (Vec3 x y _)       = Vec2 x y

instance Extend Vec2 Vec4 where
  extendZero   (Vec2 x y) = Vec4 x y 0 0
  extendWith t (Vec2 x y) = Vec4 x y t t
  trim (Vec4 x y _ _)     = Vec2 x y 

instance Extend Vec3 Vec4 where
  extendZero   (Vec3 x y z) = Vec4 x y z 0
  extendWith t (Vec3 x y z) = Vec4 x y z t
  trim (Vec4 x y z _)       = Vec3 x y z

instance Extend Mat2 Mat3 where
  extendZero   (Mat2 p q) = Mat3 (extendZero p) (extendZero q) zero
  extendWith w (Mat2 p q) = Mat3 (extendZero p) (extendZero q) (Vec3 0 0 w)
  trim (Mat3 p q _) = Mat2 (trim p) (trim q)

instance Extend Mat2 Mat4 where
  extendZero   (Mat2 p q) = Mat4 (extendZero p) (extendZero q) zero zero
  extendWith w (Mat2 p q) = Mat4 (extendZero p) (extendZero q) (Vec4 0 0 w 0) (Vec4 0 0 0 w)
  trim (Mat4 p q _ _) = Mat2 (trim p) (trim q)

instance Extend Mat3 Mat4 where
  extendZero   (Mat3 p q r) = Mat4 (extendZero p) (extendZero q) (extendZero r) zero
  extendWith w (Mat3 p q r) = Mat4 (extendZero p) (extendZero q) (extendZero r) (Vec4 0 0 0 w)
  trim (Mat4 p q r _) = Mat3 (trim p) (trim q) (trim r)
  
--------------------------------------------------------------------------------

  