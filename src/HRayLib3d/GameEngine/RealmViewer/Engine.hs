{-# LANGUAGE RecordWildCards, OverloadedStrings, RankNTypes, DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module HRayLib3d.GameEngine.RealmViewer.Engine
  ( loadPK3
  , loadAssetBundle
  , loadShaderCache
  , createLoadingScreen
  , drawLoadingScreen
  , engineInit
  , setupStorage
  , updateRenderInput
  -- temp
  , loadQuake3Graphics
  , compileQuake3GraphicsCached
  , getSpawnPoints
  , getBSP
  , getModelIndexFromBrushIndex
  , getTeleportFun
  , getMusicFile
  ) where

-- import Data.Aeson hiding (Object(..))
import Control.Monad
import Data.Char
import Data.List (isPrefixOf,partition,isInfixOf,elemIndex)
import Data.Maybe
import Data.Vect
import Data.Set (Set)
import Data.Map ( Map, fromList, map )
import System.FilePath
import System.Directory

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Binary (encodeFile,decodeFile)
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.List.Split
-- import Text.Format

import Control.Monad.Catch
import Text.Show.Pretty (ppShow)
import Codec.Picture
import LambdaCube.GL as GL
import HRayLib3d.GameEngine.Data.BSP
import HRayLib3d.GameEngine.Data.GameCharacter
import HRayLib3d.GameEngine.Data.Material hiding (Vec3,Entity)
import HRayLib3d.GameEngine.Content
import HRayLib3d.GameEngine.Graphics.Culling
import HRayLib3d.GameEngine.Graphics.Frustum
import HRayLib3d.GameEngine.Graphics.Storage
import HRayLib3d.GameEngine.Graphics.BSP
import HRayLib3d.GameEngine.Graphics.MD3
import HRayLib3d.GameEngine.Loader.BSP
import HRayLib3d.GameEngine.Loader.Zip
import HRayLib3d.GameEngine.Utils
import qualified HRayLib3d.GameEngine.Data.MD3 as MD3
import qualified HRayLib3d.GameEngine.Loader.Entity as E

import HRayLib3d.GameEngine.RealmViewer.Entity
import HRayLib3d.GameEngine.RealmViewer.Content
import Text.XML.Writer

-- import qualified Data.Aeson.Decoding as JSON
import qualified Data.Aeson as JSON
import Data.String (IsString)
import System.Directory()
import System.Process (StdStream(..), proc, shell, createProcess, cwd, std_out)
import HRayLib3d.GameEngine.Loader.Entity (EntityData(EntityData))
import qualified Control.Exception as Data.Aeson

type EngineContent =
  ( BSPLevel
  , Map ByteString MD3.MD3Model -- change to GLB/GLTF
  , [(Proj4, (Map String String, String))]
  , [[(Proj4, (Map String String, String))]]
  , [Character]
  , Map String CommonAttrs
  , [Vec3]
  , V.Vector Int
  , ([Entity], Map ByteString Entity)
  , Maybe String
  )

type EngineGraphics =
  ( GLStorage
  , [(Proj4, MD3Instance)]
  , [Character]
  , [(Proj4, (MD3.MD3Model, MD3Instance), (MD3.MD3Model, MD3Instance),(MD3.MD3Model, MD3Instance))] -- change to GLTF
  , V.Vector [GL.Object]
  , BSPLevel
  , MD3Instance
  , [(Float, SetterFun TextureData, V.Vector TextureData)]
  )

entitiesToJSON :: String -> String -> IO ()
entitiesToJSON inputS outputS = do
  cwrd <- getCurrentDirectory 
  _    <- createProcess (proc "py" [cwrd </> "read.py", inputS, outputS]){ std_out = CreatePipe }   {-(_, Just hout, _, _) <- -}
  return ()

engineInit :: Map String Entry -> FilePath -> IO (PipelineSchema, EngineContent)
engineInit pk3Data fullBSPName = do
  let bspName    = takeBaseName fullBSPName
      bspEntry   = case Map.lookup fullBSPName pk3Data of
                      Nothing   -> error "You need to put pk3 file into your current directory"
                      Just bspd -> bspd
  putStrLn $ "loading map: " ++ show bspName

  -- load bsp data
  bsp        <- readBSP . LB.fromStrict <$> readEntry bspEntry
  cwrd       <- getCurrentDirectory 
  jsonExists <- doesFileExist (lc_q3_cache </> bspName ++ ".json")  
  
  -- parse entities 
  let entitiesPath     = lc_q3_cache </> bspName ++ ".entities"
      jsonEntitiesPath = lc_q3_cache </> bspName ++ ".json" 
  print $ "Entities Path:      " ++ entitiesPath
  print $ "JSON Entities Path: " ++ jsonEntitiesPath
  createDirectoryIfMissing True lc_q3_cache -- create cache 
  if not jsonExists
    then entitiesToJSON (cwrd </> entitiesPath) (cwrd </> jsonEntitiesPath)
    else print $ "File Exists: " ++ bspName ++ ".json" 
  jsonEntities <- BL.readFile(cwrd </> jsonEntitiesPath)  --JSON.decodeFileStrict -- :: Maybe (E.EntityContainer)
  putStrLn $ "Try to load entities at: " ++ (cwrd </> jsonEntitiesPath)
  -- parse entities as json instead of raw parsing
  let entities = case JSON.eitherDecode jsonEntities of 
                  Left  x  -> error ("Error Parsing Container JSON: " ++ x)
                  Right y  -> Just y 
      spawnPoint E.EntityData{..}
        | classname `elem` [ "info_player_deathmatch"
                            , "info_player_start"
                            , "team_CTF_bluespawn"
                            , "team_CTF_redspawn"
                            , "team_CTF_blueplayer"
                            , "team_CTF_redplayer"
                            ] = [fromJust origin]
        | otherwise = []
      spawnPoints   = concatMap spawnPoint (E.ecData $ fromJust entities)
      p0            = head spawnPoints
      teleportData  = loadTeleports $  E.ecData $ fromJust entities
      music         = head . words <$> E.music  (head (E.ecData $ fromJust entities))

  -- MD3 related code
  (characterSkinMaterials,characterObjs,characters) <- readCharacters pk3Data p0
  (md3Materials, md3Map, md3Objs)                   <- readMD3Objects characterObjs (E.ecData $ fromJust entities) pk3Data
  putStrLn "loading level materials"
  --mapM_ SB.putStrLn $ map shName $ V.toList $ blShaders bsp

  print "Current WIP (q3 shader parsing)"
  shMap <- loadShaderMap pk3Data
  -- let maxMaterial = 20 -- TODO: remove if we will have fast reducer
  let shNames   = Set.fromList $ selectedMaterials ++ ignoredMaterials {-Prelude.take maxMaterial $ -} {-Prelude.take maxMaterial $ -} {-Prelude.take maxMaterial $ -} {-Prelude.take maxMaterial $ -} {-Prelude.take maxMaterial $ -} {-Prelude.take maxMaterial $ -} {-Prelude.take maxMaterial $ -} {-Prelude.take maxMaterial $ -}
      allShName = Prelude.map shName $ V.toList $ blShaders bsp
      (selectedMaterials,ignoredMaterials) = partition (\n -> or $ [SB.isInfixOf k n | k <- ["floor","wall","door","trim","block"]]) allShName
      levelMaterials = HashSet.fromList . Set.toList $ Set.map SB.unpack shNames
      modelMaterials = HashSet.fromList . Set.toList $ Set.map SB.unpack (md3Materials `Set.union` characterSkinMaterials)
      (inputSchema,shMapTexSlot) = createRenderInfo shMap levelMaterials modelMaterials
  writeSampleMaterial shMapTexSlot
  --putStrLn $ "all materials:  " ++ show (Map.size shMap)
  --putStrLn $ "used materials: " ++ show (Map.size shMap)
  --putStrLn $ "texture uniforms: \n" ++ ppShow textureUniforms
  --putStrLn $ "used materials: " ++ show (Map.size shMapTexSlot)
  --putStrLn $ "ignored materials: " ++ show (length ignoredMaterials)
  --SB.putStrLn $ SB.unlines ignoredMaterials

  let brushModelMapping = V.replicate (V.length $ blBrushes bsp) (-1) V.//
        concat (V.toList $ V.imap (\i Model{..} -> [(n,i) | n <- [mdFirstBrush..mdFirstBrush+mdNumBrushes-1]]) (blModels bsp))
  putStrLn $ "bsp model count: " ++ show (V.length $ blModels bsp)
  --print brushModelMapping
  --print teleportData
  return (inputSchema,(bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints,brushModelMapping,teleportData,music))

getMusicFile (_,_,_,_,_,_,_,_,_,music) = music

getModelIndexFromBrushIndex (_,_,_,_,_,_,_,brushModelMapping,_,_) brushIndex = brushModelMapping V.! brushIndex

getBSP (bsp,_,_,_,_,_,_,_,_,_) = bsp

getSpawnPoints (_,_,_,_,_,_,spawnPoints,_,_,_) = spawnPoints

getTeleportFun levelData@(bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints,brushModelMapping,(teleport,teleportTarget),music) brushIndex p =
  let models = Prelude.map (getModelIndexFromBrushIndex levelData) brushIndex
      hitModels = [tp | TriggerTeleport target model <- teleport, model `elem` models, TargetPosition _ tp <- maybeToList $ Map.lookup target teleportTarget]
  --in head $ trace (show ("hitModels",hitModels,models)) hitModels ++ [p]
  in head $ hitModels ++ [p]

setupStorage :: Map String Entry -> EngineContent -> GLStorage -> IO EngineGraphics
setupStorage pk3Data (bsp, md3Map, md3Objs, characterObjs, characters, shMapTexSlot,_,_,_,_) storage = do
    let slotU           = uniformSetter storage
        entityRGB       = uniformV3F   "entityRGB"     slotU
        entityAlpha     = uniformFloat "entityAlpha"   slotU
        identityLight   = uniformFloat "identityLight" slotU
        worldMat        = uniformM44F  "worldMat"      slotU
        overbrightBits  = 0
        idmtx = V4 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0) (V4 0 0 0 1)
    worldMat idmtx
    entityRGB $ V3 1 1 1
    entityAlpha 1
    identityLight $ 1 / 2 ^ overbrightBits
    initTableTextures >>= setupTableTextures slotU

    -- default texture
    let redBitmap x y = let v = if even (x+y) then 255 else 0 in PixelRGB8 v v 0
    defaultTexture <- uploadTexture2DToGPU' False False False False $ ImageRGB8 $ generateImage redBitmap 2 2

    putStrLn "loading textures:"
    -- load textures
    animTex <- fmap concat $ forM (Set.toList $ Set.fromList $ concatMap (\(shName,sh) -> [(shName,saTexture sa,saTextureUniform sa,caNoMipMaps sh) | sa <- caStages sh]) $ Map.toList shMapTexSlot) $
      \(shName,stageTex,texSlotName,noMip) -> do
        let texSetter = uniformFTexture2D (SB.pack texSlotName) slotU
            setTex isClamped img = texSetter =<< loadQ3Texture (not noMip) isClamped defaultTexture pk3Data shName img
        case stageTex of
            ST_Map img          -> setTex False img >> return []
            ST_ClampMap img     -> setTex True img >> return []
            ST_AnimMap freq imgs   -> do
                txList <- mapM (loadQ3Texture (not noMip) False defaultTexture pk3Data shName) imgs
                let txVector = V.fromList txList
                return [(fromIntegral (V.length txVector) / freq,texSetter,txVector)]
            _ -> return []

    putStrLn "add bsp to storage"
    surfaceObjs <- bspinstanceSurfaces <$> addBSP (Map.keysSet shMapTexSlot) storage bsp

    -- add entities
    let addMD3Obj (mat,(skin,name)) = case Map.lookup (SB.pack name) md3Map of
          Nothing -> return []
          Just md3 -> do
                    putStrLn ("add model: " ++ name)
                    lcmd3 <- addMD3 storage md3 skin ["worldMat"]
                    return [(mat,lcmd3)]

    lcMD3Objs   <- concat <$> forM md3Objs addMD3Obj
    lcMD3Weapon <- addMD3 storage (fromJust $ Map.lookup (SB.pack handWeapon) md3Map) mempty ["worldMat","viewProj"]
 
    -- add characters
    lcCharacterObjs <- forM characterObjs
      (\[(mat,(hSkin,hName)),(_,(uSkin,uName)),(_,(lSkin,lName))] -> do
        let Just hMD3 = Map.lookup (SB.pack hName) md3Map
            Just uMD3 = Map.lookup (SB.pack uName) md3Map
            Just lMD3 = Map.lookup (SB.pack lName) md3Map
        hLC <- addMD3 storage hMD3 hSkin ["worldMat"]
        uLC <- addMD3 storage uMD3 uSkin ["worldMat"]
        lLC <- addMD3 storage lMD3 lSkin ["worldMat"]
        return (mat,(hMD3,hLC),(uMD3,uLC),(lMD3,lLC))
      )
    return (storage,lcMD3Objs,characters,lcCharacterObjs,surfaceObjs,bsp,lcMD3Weapon,animTex)

-- TODO
updateRenderInput :: EngineGraphics -> (Vec3, Vec3, Vec3) -> Int -> Int -> Float -> Bool -> IO ()
updateRenderInput (storage,lcMD3Objs,characters,lcCharacterObjs,surfaceObjs,bsp,lcMD3Weapon,animTex) (camPos,camTarget,camUp) w h time noBSPCull = do
            let slotU       = uniformSetter storage
                matSetter   = uniformM44F "viewProj" slotU
                viewOrigin  = uniformV3F "viewOrigin" slotU
                --orientation = uniformM44F "orientation" slotU
                viewMat     = uniformM44F "viewMat" slotU
                timeSetter  = uniformFloat "time" slotU

            let cm = fromProjective (lookat camPos camTarget camUp)
                pm = perspective near far (fovDeg / 180 * pi) (fromIntegral w / fromIntegral h)
                sm = fromProjective (scaling $ Vec3 s s s)
                s  = 0.005
                --V4 orientA orientB orientC _ = mat4ToM44F $! cm .*. sm
                Vec3 cx cy cz = camPos
                near = 0.00001/s
                far  = 100/s
                fovDeg = 60
                frust = frustum fovDeg (fromIntegral w / fromIntegral h) near far camPos camTarget camUp
                cullObject obj p = enableObject obj (pointInFrustum p frust)

            -- set uniforms
            timeSetter time
            --putStrLn $ "time: " ++ show time ++ " " ++ show capturing
            viewOrigin $ V3 cx cy cz
            viewMat $ mat4ToM44F cm
            --orientation $ V4 orientA orientB orientC $ V4 0 0 0 1
            matSetter $! mat4ToM44F $! cm .*. sm .*. pm

            let invCM = mat4ToM44F idmtx -- inverse cm .*. (fromProjective $ translation (Vec3 0 (0) (-30)))
                rot = fromProjective $ orthogonal $ toOrthoUnsafe $ rotMatrixX (-pi/2) .*. rotMatrixY (pi/2) .*. rotMatrixX (10/pi*2)
                --rot = fromProjective $ rotationEuler (Vec3 (-pi/2+30/pi*2) (pi/2) (-pi))
            forM_ (md3instanceObject lcMD3Weapon) $ \obj -> do
              uniformM44F "viewProj" (objectUniformSetter obj) $ mat4ToM44F $! rot .*. fromProjective (translation (Vec3 3 (-10) (-5))) .*. sm .*. pm
              uniformM44F "worldMat" (objectUniformSetter obj) invCM
            forM_ lcMD3Objs $ \(mat,lcmd3) -> forM_ (md3instanceObject lcmd3) $ \obj -> do
              let m = mat4ToM44F $ fromProjective (rotationEuler (Vec3 time 0 0) .*. mat)
                  p = trim . _4 $ fromProjective mat
              uniformM44F "worldMat" (objectUniformSetter obj) m
              -- uniformM44F "orientation" (objectUniformSetter obj) orientation
              cullObject obj p

            forM_ (zip characters lcCharacterObjs) $ \(Character{..},(mat,(hMD3,hLC),(uMD3,uLC),(lMD3,lLC))) -> do
{-
typedef struct {
	vec3_t		origin;
	vec3_t		axis[3];
} orientation_t;

void _VectorCopy( const vec3_t in, vec3_t out );
void _VectorMA( const vec3_t veca, float scale, const vec3_t vecb, vec3_t vecc );
  = vecc[i] = veca[i] + scale*vecb[i]; i={0,1,2}
void MatrixMultiply(float in1[3][3], float in2[3][3], float out[3][3]);

                -- entity, parent, parentModel, parent_tag_name
                CG_PositionRotatedEntityOnTag( &torso, &legs, ci->legsModel, "tag_torso");
                CG_PositionRotatedEntityOnTag( &head, &torso, ci->torsoModel, "tag_head");
              -}
              -- minBound, maxBound :: a
              -- fromEnum :: a -> Int
              -- toEnum :: Int -> a

              let bothAnim = [BOTH_DEATH1, BOTH_DEAD1, BOTH_DEATH2, BOTH_DEAD2, BOTH_DEATH3, BOTH_DEAD3]
                  anims = V.fromList $
                    [ (TORSO_GESTURE,LEGS_IDLE)
                    , (TORSO_ATTACK,LEGS_IDLE)
                    , (TORSO_ATTACK2,LEGS_IDLE)
                    , (TORSO_DROP,LEGS_IDLE)
                    , (TORSO_RAISE,LEGS_IDLE)
                    , (TORSO_STAND,LEGS_IDLE)
                    , (TORSO_STAND2,LEGS_IDLE)
                    , (TORSO_GETFLAG,LEGS_IDLE)
                    , (TORSO_GUARDBASE,LEGS_IDLE)
                    , (TORSO_PATROL,LEGS_IDLE)
                    , (TORSO_FOLLOWME,LEGS_IDLE)
                    , (TORSO_AFFIRMATIVE,LEGS_IDLE)
                    , (TORSO_NEGATIVE,LEGS_IDLE)

                    , (TORSO_STAND,LEGS_WALKCR)
                    , (TORSO_STAND,LEGS_WALK)
                    , (TORSO_STAND,LEGS_RUN)
                    , (TORSO_STAND,LEGS_BACK)
                    , (TORSO_STAND,LEGS_SWIM)

                    , (TORSO_STAND,LEGS_JUMP)
                    , (TORSO_STAND,LEGS_LAND)

                    , (TORSO_STAND,LEGS_JUMPB)
                    , (TORSO_STAND,LEGS_LANDB)

                    , (TORSO_STAND,LEGS_IDLE)
                    , (TORSO_STAND,LEGS_IDLECR)

                    , (TORSO_STAND,LEGS_TURN)


                    , (TORSO_STAND,LEGS_BACKCR)
                    , (TORSO_STAND,LEGS_BACKWALK)
                    ] ++ zip bothAnim bothAnim

              let t100 = floor $ time / 4
                  (torsoAnimType,legAnimType) = anims V.! (t100 `mod` V.length anims)

              -- torso = upper
              --  transform torso to legs
              --  transform head to torso (and legs)
              let t = floor $ time * 15
                  legAnim = animationMap HashMap.! legAnimType
                  legFrame = aFirstFrame legAnim + t `mod` aNumFrames legAnim
                  torsoAnim = animationMap HashMap.! torsoAnimType
                  torsoFrame = aFirstFrame torsoAnim + t `mod` aNumFrames torsoAnim

                  tagToMat4 MD3.Tag{..} = translateAfter4 tgOrigin (orthogonal . toOrthoUnsafe $ tgRotationMat)
                  hMat = tagToMat4 ((MD3.mdTags uMD3 V.! torsoFrame) HashMap.! "tag_head") .*. uMat
                  uMat = tagToMat4 $ (MD3.mdTags lMD3 V.! legFrame) HashMap.! "tag_torso"
                  lMat = one :: Proj4
                  lcMat m = mat4ToM44F . fromProjective $ m .*. rotationEuler (Vec3 (time/5) 0 0) .*. mat
                  p = trim . _4 $ fromProjective mat
                  setup m obj = do
                    uniformM44F "worldMat" (objectUniformSetter obj) $ lcMat m
                    cullObject obj p
              forM_ (md3instanceObject hLC) $ setup hMat
              forM_ (md3instanceObject uLC) $ setup uMat
              forM_ (md3instanceObject lLC) $ setup lMat
              --setMD3Frame hLC frame
              setMD3Frame uLC torsoFrame
              setMD3Frame lLC legFrame

            forM_ animTex $ \(animTime,texSetter,v) -> do
              let (_,i) = properFraction (time / animTime)
                  idx = floor $ i * fromIntegral (V.length v)
              texSetter $ v V.! idx
            setScreenSize storage (fromIntegral w) (fromIntegral h)
            -- TODO
            let idmtx = V4 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0) (V4 0 0 0 1)
            V.forM_ surfaceObjs $ \objs -> forM_ objs $ \obj -> uniformM44F "worldMat" (objectUniformSetter obj) idmtx
            (if noBSPCull then V.forM_ surfaceObjs $ \objs -> forM_ objs $ \obj -> enableObject obj True else cullSurfaces bsp camPos frust surfaceObjs)
            return ()
