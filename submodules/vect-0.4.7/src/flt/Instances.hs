
-- | 'Eq', 'Num' and 'Fractional' instances for vectors and matrices.
-- These make writing code much more convenient, but also much more 
-- dangerous; thus you have to import this module explicitely.
--
-- In the case of Vector instances, all operations are pointwise
-- (including multiplication and division), and scalars are implicitly
-- converted to vectors so that all components of the resulting vectors
-- are the equal to the given scalar. This gives a set of consistent
-- instances.
--
-- In the case of Matrices, multiplication is usual matrix multiplication,
-- division is not implemented, and scalars are converted to diagonal 
-- matrices.
--
-- 'abs' and 'signum' are implemented to be 'normalize' and 'norm'
-- (in the case of matrices, Frobenius norm).

module Data.Vect.Flt.Instances where

--------------------------------------------------------------------------------  
  
import Data.Vect.Flt.Base 

--------------------------------------------------------------------------------  
-- * Vectors

instance Eq Vec2 where
  Vec2 x y == Vec2 a b = (x==a && y==b)

instance Num Vec2 where
  (+) = (&+)
  (-) = (&-)
  negate = neg
  (*) = pointwise 
  fromInteger a = let x = fromInteger a in Vec2 x x
  abs = normalize
  signum v = let x = norm v in Vec2 x x
  
instance Fractional Vec2 where
  (Vec2 x y) / (Vec2 a b) = Vec2 (x/a) (y/b) 
  fromRational a = let x = fromRational a in Vec2 x x 

--------------------------------------------------------------------------------  

instance Eq Vec3 where
  Vec3 x y z == Vec3 a b c = (x==a && y==b && z==c)

instance Num Vec3 where
  (+) = (&+)
  (-) = (&-)
  negate = neg
  (*) = pointwise 
  fromInteger a = let x = fromInteger a in Vec3 x x x
  abs = normalize
  signum v = let x = norm v in Vec3 x x x
  
instance Fractional Vec3 where
  (Vec3 x y z) / (Vec3 a b c) = Vec3 (x/a) (y/b) (z/c)
  fromRational a = let x = fromRational a in Vec3 x x x  
  
--------------------------------------------------------------------------------  

instance Eq Vec4 where
  Vec4 x y z w == Vec4 a b c d = (x==a && y==b && z==c && w==d)

instance Num Vec4 where
  (+) = (&+)
  (-) = (&-)
  negate = neg
  (*) = pointwise 
  fromInteger a = let x = fromInteger a in Vec4 x x x x
  abs = normalize
  signum v = let x = norm v in Vec4 x x x x
  
instance Fractional Vec4 where
  (Vec4 x y z w) / (Vec4 a b c d) = Vec4 (x/a) (y/b) (z/c) (w/d)
  fromRational a = let x = fromRational a in Vec4 x x x x  

--------------------------------------------------------------------------------  
-- * Matrices  

instance Eq Mat2 where
  Mat2 x y == Mat2 a b = (x==a && y==b)

instance Num Mat2 where
  (+) = (&+)
  (-) = (&-)
  negate = neg
  
  (*) = (.*.)
  fromInteger = diag . fromInteger
  abs m = m &* (1.0 / frobeniusNorm m)
  signum = diag . (\x -> Vec2 x x) . frobeniusNorm
  
  -- (*) = pointwise 
  -- fromInteger a = let x = fromInteger a in Mat2 x x
  -- abs m = m &* (1.0 / frobeniusNorm m)
  -- signum m = Mat2 v v where
  --   x = frobeniusNorm m 
  --   v = Vec2 x x
    
instance Fractional Mat2 where
  -- (Mat2 x y) / (Mat2 a b) = Mat2 (x/a) (y/b) 
  -- fromRational a = let x = fromRational a in Mat2 x x  
  (/) = error "Mat2/division: not implemented"
  fromRational = diag . fromRational
  
--------------------------------------------------------------------------------  
    
instance Eq Mat3 where
  Mat3 x y z == Mat3 a b c = (x==a && y==b && z==c)

instance Num Mat3 where
  (+) = (&+)
  (-) = (&-)
  negate = neg

  (*) = (.*.)
  fromInteger = diag . fromInteger
  abs m = m &* (1.0 / frobeniusNorm m)
  signum = diag . (\x -> Vec3 x x x) . frobeniusNorm

  -- (*) = pointwise 
  -- fromInteger a = let x = fromInteger a in Mat3 x x x
  -- abs m = m &* (1.0 / frobeniusNorm m)
  -- signum m = Mat3 v v v where
  --   x = frobeniusNorm m 
  --   v = Vec3 x x x
    
instance Fractional Mat3 where
  -- (Mat3 x y z) / (Mat3 a b c) = Mat3 (x/a) (y/b) (z/c)
  -- fromRational a = let x = fromRational a in Mat3 x x x  
  (/) = error "Mat3/division: not implemented"
  fromRational = diag . fromRational
    
--------------------------------------------------------------------------------  
  
instance Eq Mat4 where
  Mat4 x y z w == Mat4 a b c d = (x==a && y==b && z==c && w==d)

instance Num Mat4 where
  (+) = (&+)
  (-) = (&-)
  negate = neg

  (*) = (.*.)
  fromInteger = diag . fromInteger
  abs m = m &* (1.0 / frobeniusNorm m)
  signum = diag . (\x -> Vec4 x x x x) . frobeniusNorm

  -- (*) = pointwise 
  -- fromInteger a = let x = fromInteger a in Mat4 x x x x
  -- abs m = m &* (1.0 / frobeniusNorm m)
  -- signum m = Mat4 v v v v where
  --   x = frobeniusNorm m 
  --   v = Vec4 x x x x
    
instance Fractional Mat4 where
  -- (Mat4 x y z w) / (Mat4 a b c d) = Mat4 (x/a) (y/b) (z/c) (w/d)
  -- fromRational a = let x = fromRational a in Mat4 x x x x
  (/) = error "Mat4/division: not implemented"
  fromRational = diag . fromRational

--------------------------------------------------------------------------------  
    
