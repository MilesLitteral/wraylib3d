{-#LANGUAGE RankNTypes #-}

module HRayLib3d.Utils.Prisms ((##), (#?), (#!), (#.), (#^.), (##?)) where

import Data.Maybe   ()
import Data.Monoid  ( First )
import Control.Lens ( (^?), (^.), re, view, from, Getting, Getter, AReview, AnIso, Iso, Prism ) 

-- | "Review" Operator
-- Use this to create instances of types
-- it can also be used to Turn a Prism or 
-- Iso around to build a Getter(IE: turn an Iso or Prism around and view a value).
-- combining ## and #? will show a satisfaction of Prism's laws 
-- (ex1: ((4, 4, Yellow, 120) ## _Car) #? _Car = Just (4, 4, Yellow, 120))
--  ex2: let porsche    = _Car ## (4, 4, Yellow, 120)
--           components = (wheels . color) ## porsche  -- components = (4, Yellow)
(##)  :: AReview t s -> s -> t
x ## y = y ^. re x
--End

-- |"Preview Value" Operator
-- Use this to assert if an object is of a specified type
-- the assertion is returned wrapped in Maybe where it is
-- either Nothing (assert fails) or it is Just a deconstruction of the object
-- in terms of the specified type (assert succeeds) 
-- (tl;dr example: _Car #? sedan = (4, 4, Silver, 120))
-- tires = wheels #? porsche (tires = Just 4 ^ Nothing)
-- it's equivalent to: y ^. pre x
(#?)  :: Getting (First a) s a -> s -> Maybe a 
x #? y = y ^? x 
--End

-- | View Value Operator 
-- Will get a value when given a specific lens
-- it's a strict version of #? that will throw an 
-- error if it's invalid rather than Nothing
-- ex: data Car = Car {_wheels :: Int, _doors :: Int, _color :: Color, _topSpeed :: Int} 
--     let  porsche = _Car ## (4, 4, Yellow, 120)
--          tires = wheels #! porsche (tires = 4)
(#!) :: Getter s a -> s -> a
x #! y = view x y

-- Invert Iso Operator
-- it's just an operator version of 'from'
-- use it to quickly create isomorphisms for quick access
-- of two types or invert isomorphisms
(#.) :: AnIso s t a b -> Iso b a t s 
x #. y = from x y 

-- | Boolean Version of #?
-- Use this to assert types (IE: _RenderImgSrc ##? object == True ^ False)
(##?) :: Getting (First a) s a -> s -> Bool
x ##? y = case (y ^? x) of
    Nothing        -> False
    Just _         -> True
    
-- |"Preview Lens" Operator
-- query if an object has a lens
-- it gives you an optic in terms of the specific object
-- helpful for holding references
-- ex: data Car = Car {_wheels :: Int, _doors :: Int, _color :: Color, _topSpeed :: Int} 
--     let myWheels = wheels #^. myPorsche ≡ (_wheels myPorsche) || myPorsche ^. wheels
-- aka: (x ^. (y ^? x))
(#^.) :: s -> Getting a s a -> a 
x #^. y = x ^. y --(pre x)
--End