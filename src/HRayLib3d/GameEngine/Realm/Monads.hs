{-# LANGUAGE FlexibleContexts #-}


module HRayLib3d.GameEngine.Realm.Monads where

import Control.Monad.RWS    ( MonadPlus(mzero), MonadWriter(tell), RWST, evalRWST )
import Control.Monad.Random ( RandT, Rand, evalRand, runRandT )
import Control.Monad.Writer.Strict ( WriterT, execWriterT )
import Control.Monad.Trans.Maybe ( MaybeT(runMaybeT) )

import HRayLib3d.GameEngine.Collision ()
import HRayLib3d.GameEngine.Data.BSP ( BSPLevel )
import HRayLib3d.GameEngine.Data.MD3 ()
import HRayLib3d.GameEngine.RenderSystem ( ResourceCache )
import HRayLib3d.GameEngine.Realm.Entities ( Action, Entity )
import HRayLib3d.GameEngine.Realm.Visuals ( Visual )
import HRayLib3d.GameEngine.Realm.World ( Input )

import System.Random.Mersenne.Pure64 ( PureMT )

import Data.Vect ( Vec3 )
import Data.Vect.Float.Instances ()
import Data.Vect.Float.Base ( Vec3 )
import Data.Vector (Vector)
import Data.Functor.Identity ( Identity(runIdentity) )
import qualified Data.Vector as V

-- Entities have access to this data
data EntityEnvironment = EntityEnvironment 
 {
    resources :: ResourceCache
  , level     :: BSPLevel
  , userInput :: Input
  , gravity   :: Vec3
  , entities  :: Vector Entity
 }

type UpdateM w s = MaybeT (RWST EntityEnvironment w s (Rand PureMT))
type Collected_Objects = ([Entity], [Visual], [(Int, Action)])
type EntityM e = UpdateM Collected_Objects e
type VisualM v = UpdateM [Visual] v
type CollectMT m = WriterT Collected_Objects (RandT PureMT m)
type CollectM = CollectMT Identity

runUpdateM :: EntityEnvironment -> s -> UpdateM w s s -> PureMT -> (Maybe s, w)
runUpdateM entityEnvironment state update randGen = evalRand (evalRWST (runMaybeT update) entityEnvironment state) randGen

runEntityM :: EntityEnvironment -> e -> EntityM e e -> PureMT -> (Maybe e, Collected_Objects)
runEntityM = runUpdateM

runVisualM :: EntityEnvironment -> v -> VisualM v v -> PureMT -> (Maybe v, [Visual])
runVisualM = runUpdateM

runCollectMT :: Monad m => PureMT -> CollectMT m a -> m (Collected_Objects, PureMT)
runCollectMT randGen collector = runRandT (execWriterT collector) randGen

runCollectM :: PureMT -> CollectM a -> (Collected_Objects, PureMT)
runCollectM p c = runIdentity $ runCollectMT p c

addEntities :: MonadWriter Collected_Objects m => [Entity] -> m ()
addEntities entities = tell (entities, [], [])

addEntity :: MonadWriter Collected_Objects m => Entity -> m ()
addEntity = addEntities . pure

addVisuals :: MonadWriter Collected_Objects m => [Visual] -> m ()
addVisuals visuals = tell ([], visuals, [])

addVisual :: MonadWriter Collected_Objects m => Visual -> m ()
addVisual = addVisuals . pure

addActions :: MonadWriter Collected_Objects m => [(Int, Action)] -> m ()
addActions actions = tell ([],[],actions)

addAction :: MonadWriter Collected_Objects m => (Int, Action) -> m ()
addAction = addActions . pure

die :: MonadPlus m => m a
die = mzero