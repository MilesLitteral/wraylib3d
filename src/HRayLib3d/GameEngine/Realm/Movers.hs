{-# LANGUAGE RecordWildCards #-}

module HRayLib3d.GameEngine.Realm.Movers where

import Control.Monad ( when )
import Lens.Micro.Platform ()
import Data.Vect
    ( Vec3(..), normalize, AbelianGroup(zero), Vector((*&)) )
import Data.Vect.Float.Instances ()
import Data.Vect.Float.Base
    ( Vec3(..), normalize, AbelianGroup(zero), Vector((*&)) )
import Data.Vect.Float.Util.Quaternion ()
import qualified Data.Vect.Float.Base as VB (_1, _2, _3)

import Control.Monad.State.Strict ( when, MonadState(put, get) )
import Control.Monad.Writer.Strict ( when )
import Control.Monad.Reader ( when, MonadReader(ask) )

import HRayLib3d.GameEngine.RenderSystem ()
import HRayLib3d.GameEngine.Collision
    ( traceBox, traceRay, TraceHit )
import HRayLib3d.GameEngine.Data.BSP ( BSPLevel )
import HRayLib3d.GameEngine.Data.MD3 ()
import HRayLib3d.GameEngine.RenderSystem

import HRayLib3d.GameEngine.Realm.Entities ()
import HRayLib3d.GameEngine.Realm.Visuals ()
import HRayLib3d.GameEngine.Realm.World
    ( Input(Input, forwardmove, jump, toggleHoldable, changeWeapon,
            mouseV, mouseU, mouseY, mouseX, time, dtime, shoot, sidemove) )
import HRayLib3d.GameEngine.Realm.Collision ()
import HRayLib3d.GameEngine.Realm.Monads
    ( EntityEnvironment(gravity, level, userInput), EntityM )

-- general mover typeclass
-- any type of object can be moved around the map provided that it implements this interface
class Mover object where 
 getPosition :: EntityM object Vec3
 getVelocity :: EntityM object Vec3
 getBounds :: EntityM object (Vec3, Vec3)
 setPosition :: Vec3 -> EntityM object ()
 setVelocity :: Vec3 -> EntityM object ()
 reactToCollision :: Vec3 -> TraceHit -> EntityM object ()
 reactToGroundHit :: Vec3 -> TraceHit -> EntityM object ()
 reactToFalling :: EntityM object ()
  
modifyPosition :: Mover object => Vec3 -> EntityM object ()
modifyPosition vector = do
 position <- getPosition
 setPosition (position + vector)
 
modifyVelocity :: Mover object => (Vec3 -> Vec3) -> EntityM object ()
modifyVelocity f = getVelocity >>= setVelocity . f
 
--returns whether the object was successfully moved or not
--the second argument is a function that handles collisions. it gets the initial state of the mover
tryMovingWith:: Mover object => 
   (BSPLevel -> Vec3 -> Vec3 -> Maybe (Vec3, TraceHit)) ->  --collision detector function
   EntityM object () ->                                     --transformation of the object 
   (Vec3 -> TraceHit -> EntityM object ()) ->     --if a collision is detected, this action is executed
   EntityM object Bool 
tryMovingWith trace transformation corrigation = do
 bsplevel <- level <$> ask 
 object <- get
 oldPosition <- getPosition
 transformation
 newPosition <- getPosition
 case trace bsplevel oldPosition newPosition of
  Nothing -> return True 
  Just (hitPos, traceHit) -> put object >> corrigation hitPos traceHit >> return False


tryMovingWithBoxBounds :: Mover object => (Vec3, Vec3) -> EntityM object () -> (Vec3 -> TraceHit -> EntityM object ()) -> EntityM object Bool
tryMovingWithBoxBounds (frMin, frMax) = tryMovingWith (traceBox frMin frMax)

tryMovingWithRay :: Mover object => EntityM object () -> (Vec3 -> TraceHit -> EntityM object ()) -> EntityM object Bool 
tryMovingWithRay = tryMovingWith traceRay

modifyZ :: (Float -> Float) -> Vec3 -> Vec3
modifyZ f (Vec3 x y z) = Vec3 x y $ f z

setZ :: Float -> Vec3 -> Vec3
setZ z = modifyZ $ const z

clearZ :: Vec3 -> Vec3
clearZ = setZ 0


updateMover :: Mover object => EntityM object ()
updateMover = do
 Input{..} <- userInput <$> ask
 gravity <- gravity <$> ask
 velocity <- getVelocity
 bounds <- getBounds
 moved <- tryMovingWithBoxBounds bounds (modifyPosition $ clearZ velocity) reactToCollision
 falling <- tryMovingWithBoxBounds bounds (modifyPosition (dtime *& gravity - 0.1 * normalize gravity + setZ (VB._3 velocity) zero)) reactToGroundHit
 if falling then reactToFalling else modifyVelocity clearZ
 when falling $ setVelocity (velocity + dtime *& gravity)
 
jumpMover :: Mover object => Float -> EntityM object ()
jumpMover scale = do
 Input{..} <- userInput <$> ask
 velocity <- getVelocity 
 gravity <- gravity <$> ask
 setVelocity (velocity + scale *& normalize (-gravity))