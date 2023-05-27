{-# LANGUAGE CPP #-}
#define Flt Float
#define VECT_Float

-- | Gram-Schmidt orthogonalization.
-- This module is not re-exported by "Data.Vect".

module Data.Vect.Flt.GramSchmidt 
  ( GramSchmidt(..)
  )
  where

import Data.Vect.Flt.Base

--------------------------------------------------------------------------------

liftPair :: (a -> b) -> (a,a) -> (b,b)
liftPair f (x,y) = (f x, f y)

liftTriple :: (a -> b) -> (a,a,a) -> (b,b,b)
liftTriple f (x,y,z) = (f x, f y, f z)

liftQuadruple :: (a -> b) -> (a,a,a,a) -> (b,b,b,b)
liftQuadruple f (x,y,z,w) = (f x, f y, f z, f w)

--------------------------------------------------------------------------------
    
-- | produces orthogonal\/orthonormal vectors from a set of vectors    
class GramSchmidt a where
  gramSchmidt          :: a -> a   -- ^ does not normalize the vectors!
  gramSchmidtNormalize :: a -> a   -- ^ normalizes the vectors.

{-# RULES
"gramSchmidt is idempotent"  forall a. gramSchmidt (gramSchmidt a) = gramSchmidt a 
"gramSchmidtNormalize is idempotent"  forall a. gramSchmidtNormalize (gramSchmidtNormalize a) = gramSchmidtNormalize a 
  #-}

--------------------------------------------------------------------------------

instance GramSchmidt (Vec2,Vec2) where
  gramSchmidt = gramSchmidtPair
  gramSchmidtNormalize = gramSchmidtNormalizePair
  
instance GramSchmidt (Vec3,Vec3) where
  gramSchmidt = gramSchmidtPair
  gramSchmidtNormalize = gramSchmidtNormalizePair
  
instance GramSchmidt (Vec4,Vec4) where
  gramSchmidt = gramSchmidtPair
  gramSchmidtNormalize = gramSchmidtNormalizePair

----------

instance GramSchmidt (Normal2,Normal2) where
  gramSchmidt          = error "use 'gramSchmidtNormalize' for Normal2!"
  gramSchmidtNormalize = liftPair toNormalUnsafe . gramSchmidtNormalizePair . liftPair fromNormal

instance GramSchmidt (Normal3,Normal3) where
  gramSchmidt          = error "use 'gramSchmidtNormalize' for Normal3!"
  gramSchmidtNormalize = liftPair toNormalUnsafe . gramSchmidtNormalizePair . liftPair fromNormal

instance GramSchmidt (Normal4,Normal4) where
  gramSchmidt          = error "use 'gramSchmidtNormalize' for Normal4!"
  gramSchmidtNormalize = liftPair toNormalUnsafe . gramSchmidtNormalizePair . liftPair fromNormal

----------
  
gramSchmidtPair :: (Vector v, DotProd v) => (v,v) -> (v,v)
gramSchmidtPair (u,v) = (u',v') where 
  u' = u
  v' = project v u'     
  
gramSchmidtNormalizePair :: (Vector v, DotProd v) => (v,v) -> (v,v)
gramSchmidtNormalizePair (u,v) = (u',v') where
  u' = normalize u 
  v' = normalize $ projectUnsafe v u'     

----------

instance GramSchmidt (Vec3,Vec3,Vec3) where
  gramSchmidt = gramSchmidtTriple
  gramSchmidtNormalize = gramSchmidtNormalizeTriple
     
instance GramSchmidt (Vec4,Vec4,Vec4) where
  gramSchmidt = gramSchmidtTriple
  gramSchmidtNormalize = gramSchmidtNormalizeTriple

instance GramSchmidt (Normal3,Normal3,Normal3) where
  gramSchmidt          = error "use 'gramSchmidtNormalize' for Normal3!"
  gramSchmidtNormalize = liftTriple toNormalUnsafe . gramSchmidtNormalizeTriple . liftTriple fromNormal

instance GramSchmidt (Normal4,Normal4,Normal4) where
  gramSchmidt          = error "use 'gramSchmidtNormalize' for Normal4!"
  gramSchmidtNormalize = liftTriple toNormalUnsafe . gramSchmidtNormalizeTriple . liftTriple fromNormal

----------

gramSchmidtTriple :: (Vector v, DotProd v) => (v,v,v) -> (v,v,v)
gramSchmidtTriple (u,v,w) = (u',v',w') where 
  u' = u
  v' = project v u'     
  w' = project (project w u') v' 
  
gramSchmidtNormalizeTriple :: (Vector v, DotProd v) => (v,v,v) -> (v,v,v)
gramSchmidtNormalizeTriple (u,v,w) = (u',v',w') where
  u' = normalize $ u 
  v' = normalize $ projectUnsafe v u'     
  w' = normalize $ projectUnsafe (projectUnsafe w u') v'     

----------

instance GramSchmidt (Vec4,Vec4,Vec4,Vec4) where
  gramSchmidt          = gramSchmidtQuadruple
  gramSchmidtNormalize = gramSchmidtNormalizeQuadruple 

instance GramSchmidt (Normal4,Normal4,Normal4,Normal4) where
  gramSchmidt          = error "use 'gramSchmidtNormalize' for Normal4!"
  gramSchmidtNormalize = liftQuadruple toNormalUnsafe . gramSchmidtNormalizeQuadruple . liftQuadruple fromNormal

----------
  
gramSchmidtQuadruple :: (Vector v, DotProd v) => (v,v,v,v) -> (v,v,v,v)
gramSchmidtQuadruple (u,v,w,z) = (u',v',w',z') where 
  u' = u
  v' = project v u'     
  w' = project (project w u') v' 
  z' = project (project (project z u') v') w'

gramSchmidtNormalizeQuadruple :: (Vector v, DotProd v) => (v,v,v,v) -> (v,v,v,v)
gramSchmidtNormalizeQuadruple (u,v,w,z) = (u',v',w',z') where
  u' = normalize $ u
  v' = normalize $ projectUnsafe v u'     
  w' = normalize $ projectUnsafe (projectUnsafe w u') v' 
  z' = normalize $ projectUnsafe (projectUnsafe (projectUnsafe z u') v') w'
  
----------
  