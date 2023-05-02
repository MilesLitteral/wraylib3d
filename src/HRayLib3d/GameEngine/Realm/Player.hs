{-# LANGUAGE LambdaCase, RecordWildCards, TupleSections, NoMonomorphismRestriction, FlexibleContexts #-}
module HRayLib3d.GameEngine.Realm.Player where

import HRayLib3d.GameEngine.Realm.Entities
import HRayLib3d.GameEngine.Realm.World
import HRayLib3d.GameEngine.Realm.Monads
import HRayLib3d.GameEngine.Realm.Visuals
import HRayLib3d.GameEngine.Realm.Items
import HRayLib3d.GameEngine.Realm.Movers
import HRayLib3d.GameEngine.Realm.Collision
import HRayLib3d.GameEngine.Realm.LoadResources(itemMap, weaponInfoMap)

import Control.Monad
import Control.Monad.Writer.Strict
import Control.Monad.Reader
import Control.Monad.Random
import Control.Monad.State

import Data.Maybe
import Data.Foldable
import Data.Map.Strict((!))
import Data.Vect hiding (Vector)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Lens.Micro.Platform

import HRayLib3d.GameEngine.Utils
import HRayLib3d.GameEngine.Collision
import Debug.Trace 


---Weapon behaviours
  
shoots :: EntityM Player ()  
shoots = do
  Input{..} <- userInput <$> ask
  shootTime <- use pShootTime
  weapon    <- use pSelectedWeapon
  hasAmmo   <- maybe False (>0) . Map.lookup weapon <$> use pAmmos
  let WeaponInfo{..} = weaponInfoMap ! weapon
  when (shoot && time - shootTime > 60 / fromIntegral wiRPM && hasAmmo) $ do
    pAmmos %= Map.update (\amount -> Just $ if amount < 1 then 0 else (amount - 1)) weapon
    fireWeapon weapon
    pShootTime .= time
    
fireWeapon :: HRayLib3d.GameEngine.Realm.Items.Weapon -> EntityM Player ()
fireWeapon = \case
 WP_MACHINEGUN -> fireMachineGun 
 WP_SHOTGUN    -> fireShotGun
 _ -> do
  pos       <- use pPosition
  direction <- use pDirection
  weapon    <- use pSelectedWeapon
  addEntities [EBullet $ Bullet (pos + 50 *& direction) (500 *& direction) 1 2 weapon]
---


fireMachineGun :: EntityM Player ()
fireMachineGun = do
  pos       <- use pPosition
  direction <- use pDirection
  weapon    <- use pSelectedWeapon
  entities  <- entities <$> ask
  level     <- level <$> ask
  let entityHits = getEntitiesIntersectingRay entities pos direction
  --We can add actions here to damage the entities in entityHits
  maybe (return ()) (\(hitPos, trHit) -> addEntity (EBullet $ Bullet { _bPosition = hitPos, _bDirection = (-100)*&direction, _bDamage = 1, _bLifeTime = 10, _bType = weapon})) (traceRay level pos (pos + 200 *& direction))
 
 

fireShotGun :: EntityM Player ()
fireShotGun = undefined
 
changeWeapon :: EntityM Player () 
changeWeapon = do
  let 
    performChange newWeapon = do
     weapons <- use pWeapons
     when (newWeapon `Set.member` weapons) $ do
      pSelectedWeapon .= newWeapon

  Input{..} <- userInput <$> ask
  maybe (return ()) performChange changeWeapon
    
    
togglesHoldable :: EntityM Player ()
togglesHoldable = do
  let 
    performToggle holdable = do
     holdables <- use pHoldables
     when (holdable `Map.member` holdables) $ do
      pHoldables %= Map.update (\(active, time) -> Just (not active, time)) holdable   
 
  Input{..} <- userInput <$> ask
  maybe (return ()) performToggle toggleHoldable

      
tickHoldables :: EntityM Player ()
tickHoldables = do
 Input{..} <- userInput <$> ask
 pHoldables %= Map.mapMaybe (\(active, rest) ->
    case (active, rest - dtime < 0) of
      (True, True)  -> Nothing
      (True, False) -> Just (active, rest - dtime)
      (False, _)    -> Just (active, rest))
      
 
 
applyUserIntendedAcceleration :: EntityM Player () --changes player velocity according to user input
applyUserIntendedAcceleration = do
 Input{..} <- userInput <$> ask
 gravity <- gravity <$> ask
 pDirection .= (let sinMV = sin mouseV in Vec3 (cos mouseU * sinMV) (sin mouseU * sinMV) (cos  mouseV))
 direction <- use pDirection
 velocity'<- use pVelocity
 isFalling <- not <$> use pCanJump
 let
  friction = 0.91
  applyFriction friction (Vec3 x y z) = Vec3 (friction*x) (friction*y) z 
  up = normalize (-gravity)
  runningSpeed = 8
  acceleration = 3
  strafeDirection = normalize $ direction `crossprod` up
  wishDir = normalize (forwardmove *& (up `crossprod` strafeDirection) + sidemove *& strafeDirection)
  currentSpeed = velocity' &. wishDir
  addSpeed = runningSpeed - currentSpeed
  accelSpeed = min addSpeed (acceleration * dtime * runningSpeed)
  finalVel = if addSpeed <= 0 then velocity' else velocity' + accelSpeed *& wishDir
  idleFriction = if isFalling then 1 else 0.85
 pVelocity .= if forwardmove == 0 && sidemove == 0 then applyFriction idleFriction velocity' else applyFriction friction finalVel

 
playerDie :: EntityM Player ()
playerDie = do
    time      <- time . userInput <$> ask
    pos       <- use pPosition
    ammos     <- Map.toList <$> use pAmmos
    armor     <- use pArmor
    armorType <- use pArmorType
    weapons <- Set.toList <$> use pWeapons
    let randomPos = (pos +) <$> getRandomR (Vec3 (-50) (-50) (-50), Vec3 50 50 50) 
    droppedAmmos <- forM ammos $ \(weapon, amount) -> do
      rpos <- randomPos
      return $ EAmmo $ Ammo rpos amount True weapon time
    droppedWeapos <- forM weapons $ \weapon -> do
      rpos <- randomPos
      return $ EWeapon $ Weapon rpos True weapon time
    droppedArmors <- case armorType of
      Just at | armor > 0 -> do
        rpos <- randomPos
        return [(EArmor $ Armor rpos armor True at time)]
      _ -> return []
    addEntities $ concat [droppedArmors, droppedWeapos, droppedAmmos]
    addVisuals [VParticle $ Particle pos (400 *& (extendZero . unitVectorAtAngle $ pi / 50 * i)) 1 | i <- [0..100]]
    die

    
-- | Spawn a player if there is none.
spawnPlayer :: World -> World
spawnPlayer w = w { _wEntities = entities }
  where
    wTime = w ^. wInput . to time
    entities = (case find hasPlayer (_wEntities w) of
                  Nothing -> ((PSpawn . Spawn wTime $ EPlayer player):)
                  Just _  -> id) $ _wEntities w

    hasPlayer (EPlayer _)                    = True
    hasPlayer (PSpawn (Spawn _ (EPlayer _))) = True
    hasPlayer _                              = False

    isSpawnPoint (ESpawnPoint _) = True
    isSpawnPoint _               = False

    (ESpawnPoint spawnPoint) = fromJust $ find isSpawnPoint (_wEntities w)
    player = Player
      { _pPosition    = _spPosition spawnPoint + Vec3 0 0 80
      , _pDirection   = _spAngles spawnPoint
      , _pVelocity   = Vec3 0 0 0
      , _pHealth      = 100
      , _pArmor       = 0
      , _pArmorType   = Nothing
      , _pShootTime   = 0
      , _pDamageTimer = 0
      , _pName        = "Bones"
      , _pId          = 0
      , _pWeapons     = Set.fromList [HRayLib3d.GameEngine.Realm.Items.WP_GAUNTLET, HRayLib3d.GameEngine.Realm.Items.WP_MACHINEGUN, HRayLib3d.GameEngine.Realm.Items.WP_ROCKET_LAUNCHER]
      , _pSelectedWeapon = HRayLib3d.GameEngine.Realm.Items.WP_MACHINEGUN
      , _pAmmos       = Map.fromList
           [ (HRayLib3d.GameEngine.Realm.Items.WP_GAUNTLET,         1)
           , (HRayLib3d.GameEngine.Realm.Items.WP_MACHINEGUN,      100)
           , (HRayLib3d.GameEngine.Realm.Items.WP_ROCKET_LAUNCHER, 100)
           ]
      , _pHoldables  = Map.empty
      , _pPowerups   = Set.empty
      , _pRotationUV = Vec3 0 0 0
      , _pCanJump    = True
      }
      
      
stepPlayer :: EntityM Player Player
stepPlayer = do
  Input{..} <- userInput <$> ask
 
  pRotationUV .= (Vec3 mouseU 0 mouseV)
  HRayLib3d.GameEngine.Realm.Player.applyUserIntendedAcceleration
  canJump <- use pCanJump
  when (jump && canJump) $ jumpMover 6
  updateMover
 
  HRayLib3d.GameEngine.Realm.Player.changeWeapon 
  HRayLib3d.GameEngine.Realm.Player.shoots 
  HRayLib3d.GameEngine.Realm.Player.togglesHoldable 
  HRayLib3d.GameEngine.Realm.Player.tickHoldables 
 
  -- death
  pHealth %= min 200
  health <- use pHealth
  unless (health > 0) playerDie
  get
  
instance Mover Player where
  getPosition = use pPosition
  getVelocity = use pVelocity
  reactToCollision _ _ = return ()
  reactToGroundHit _ _ = pCanJump .= True 
  reactToFalling = pCanJump .= False
  setPosition newPos = pPosition .= newPos
  setVelocity newVel = pVelocity .= newVel 
  getBounds = return $ (,) (Vec3 (-5) (-5) (-60)) (Vec3 5 5 2)
  
pickUpAmmo :: Int -> HRayLib3d.GameEngine.Realm.Items.Weapon -> EntityM Player ()
pickUpAmmo q w = pAmmos %= Map.insertWith (+) w q

pickUpWeapon :: HRayLib3d.GameEngine.Realm.Items.Weapon -> EntityM Player ()
pickUpWeapon w = pWeapons %= Set.insert w

oncePerSec :: EntityM Player Bool
oncePerSec = do
 time <- time . userInput <$> ask
 t <- use pDamageTimer
 if t > time then return False else do
   pDamageTimer .= time + 1
   return True