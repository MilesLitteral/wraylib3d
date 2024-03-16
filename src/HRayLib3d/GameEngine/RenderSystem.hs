{-# LANGUAGE RecordWildCards, OverloadedStrings, LambdaCase, FlexibleContexts, TemplateHaskell, RankNTypes #-}
module HRayLib3d.GameEngine.RenderSystem
  ( RenderSystem
  , initRenderSystem
  , loadResources
  , renderScene
  , Renderable(..)
  , Picture(..)
  , Scene(..)
  , BS8.ByteString

  -- resource cache
  , ResourceCache
  , getResourceCache
  , lookupBSPData
  , lookupMD3Data
  , lookupAnimationData
  ) where

import Data.Vect
    ( Proj4,
      translateAfter4,
      translateBefore4,
      AbelianGroup(neg),
      Extend(trim),
      HasCoordinates(_4),
      Matrix(transpose, idmtx),
      MultSemiGroup((.*.), one),
      Orthogonal(toOrthoUnsafe),
      Projective(fromProjective, orthogonal) )
import Data.Maybe    (fromJust, mapMaybe)
import Data.IORef    ( IORef, newIORef, readIORef, writeIORef )
import Data.List     (foldl')
import Data.Hashable ( Hashable )
import Data.HashSet  (HashSet)

import Codec.Picture ( DynamicImage(ImageRGB8), generateImage, PixelRGB8(PixelRGB8) )
import Data.Vector   ( Vector )
import Data.Map.Strict ( Map )
import Data.Digest.CRC32   ( crc32   )
import Data.HashMap.Strict ( HashMap )

import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified Data.HashSet as HashSet
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashMap.Strict as HashMap

import Text.Printf                ( printf )
import Control.Monad              ( forM, forM_, unless )
import Control.Monad.State.Strict ( MonadIO(liftIO), forM, forM_, unless, StateT, execStateT )
import Lens.Micro.Platform ( Lens', (%=), use, makeLenses )

import LambdaCube.GL
    ( V3(V3),
      disposeRenderer,
      renderFrame,
      uploadTexture2DToGPU',
      allocStorage,
      enableObject,
      objectUniformSetter,
      setScreenSize,
      uniformFTexture2D,
      uniformFloat,
      uniformM44F,
      uniformV3F,
      GLRenderer,
      GLStorage(uniformSetter),
      SetterFun,
      TextureData 
      )
import HRayLib3d.GameEngine.Data.Material
    ( StageTexture(ST_AnimMap, ST_Map, ST_ClampMap),
      StageAttrs(StageAttrs, saBlend, saTextureUniform,
                 saDepthMaskExplicit, saAlphaMap, saAlphaFunc, saDetail,
                 saDepthFunc, saDepthWrite, saTexture, saTCMod, saTCGen, saAlphaGen,
                 saRGBGen),
      CommonAttrs(..) )
import HRayLib3d.GameEngine.Graphics.Storage
    ( compileQuake3GraphicsCached,
      createRenderInfo,
      initTableTextures,
      loadQ3Texture,
      loadQuake3Graphics,
      setupTableTextures,
      writeSampleMaterial,
      TableTextures )
import HRayLib3d.GameEngine.Graphics.Frustum ()
import HRayLib3d.GameEngine.Graphics.Culling ( cullSurfaces )
import HRayLib3d.GameEngine.Graphics.Quad ( addQuad, updateQuad, QuadInstance(..) )
import HRayLib3d.GameEngine.Graphics.MD3
    ( addGPUMD3,
      setMD3Frame,
      uploadMD3,
      GPUMD3(gpumd3Shaders, gpumd3Model),
      MD3Instance(..) )
import HRayLib3d.GameEngine.Graphics.BSP
    ( addGPUBSP,
      uploadBSP,
      BSPInstance(..),
      GPUBSP(gpubspShaders, gpubspBSPLevel) )
import HRayLib3d.GameEngine.Graphics.GameCharacter ( addCharacterInstance, setupGameCharacter, CharacterInstance(..) )
import HRayLib3d.GameEngine.Loader.Zip ( readEntry, Entry )
import HRayLib3d.GameEngine.Loader.BSP (readBSP)
import HRayLib3d.GameEngine.Loader.MD3 (readMD3, readMD3Skin)
import HRayLib3d.GameEngine.Loader.GameCharacter ( parseCharacter )
import HRayLib3d.GameEngine.Content ( loadShaderMap )
import HRayLib3d.GameEngine.Scene
    ( asResource,
      leftOrthoU,
      Camera(..),
      MD3Data(..),
      Picture(..),
      Renderable(..),
      Resource(R_Shader, R_AnimationCfg, R_Skin, R_MD3, R_BSPMap),
      Scene(..),
      Tag(Tag) )
import HRayLib3d.GameEngine.Utils    ( mat4ToM44F, ortho, setNub, toWorldMatrix, vec3ToV3F )
import HRayLib3d.GameEngine.Data.MD3 ( Tag(tgName, tgRotationMat, tgOrigin) )
import qualified HRayLib3d.GameEngine.Data.BSP as BSP
import qualified HRayLib3d.GameEngine.Data.MD3 as MD3
import qualified HRayLib3d.GameEngine.Data.GameCharacter as GCH

type AnimationCache   = HashMap String GCH.Character
type BSPCache         = HashMap String GPUBSP
type BSPInstanceCache = HashMap String [BSPInstance]
type MD3SkinCache     = HashMap String MD3.MD3Skin
type MD3Cache         = HashMap String GPUMD3
type MD3InstanceCache = HashMap String [MD3Instance]
type QuadCache        = HashMap String [QuadInstance]
type CharacterCache   = HashMap (String,String) [CharacterInstance]
type ShaderCache      = HashSet String
type TextureCache     = HashMap (String,Bool,Bool) TextureData
type AnimatedTexture  = (Float, SetterFun TextureData, Vector TextureData)

{-
  remove IORefs
  traverse renderables
    load resources if necessary
    collect scene materials
  load new textures if necessary
  create new pipeline and storage if necessary
  problematic code: use state or writer monad to collect data
    updateModelCache
    updateRenderCache
-}

data ResourceCache
  = ResourceCache
  { rcBSPCache        :: BSPCache
  , rcMD3Cache        :: MD3Cache
  , rcAnimationCache  :: AnimationCache
  }

getResourceCache :: RenderSystem -> IO ResourceCache
getResourceCache RenderSystem{..} = ResourceCache <$> readIORef rsBSPCache <*> readIORef rsMD3Cache <*> readIORef rsAnimationCache

lookupBSPData :: String -> ResourceCache -> Maybe BSP.BSPLevel
lookupBSPData name ResourceCache{..} = gpubspBSPLevel <$> HashMap.lookup name rcBSPCache

lookupMD3Data :: String -> ResourceCache -> Maybe MD3.MD3Model
lookupMD3Data name ResourceCache{..} = gpumd3Model <$> HashMap.lookup name rcMD3Cache

lookupAnimationData :: String -> ResourceCache -> Maybe GCH.Character
lookupAnimationData name ResourceCache{..} = HashMap.lookup name rcAnimationCache

data RenderSystem
  = RenderSystem
  -- static values
  { rsFileSystem        :: Map String Entry
  , rsShaderMap         :: Map String CommonAttrs
  , rsCheckerTexture    :: TextureData
  , rsWhiteTexture      :: TextureData
  , rsTableTextures     :: TableTextures
  -- resource caches
  , rsAnimationCache    :: IORef AnimationCache
  , rsBSPCache          :: IORef BSPCache
  , rsBSPInstanceCache  :: IORef BSPInstanceCache
  , rsBSPShaderCache    :: IORef ShaderCache
  , rsMD3SkinCache      :: IORef MD3SkinCache
  , rsMD3Cache          :: IORef MD3Cache
  , rsMD3InstanceCache  :: IORef MD3InstanceCache
  , rsMD3ShaderCache    :: IORef ShaderCache
  , rsCharacterCache    :: IORef CharacterCache
  , rsQuadCache         :: IORef QuadCache
  , rsTextureCache      :: IORef TextureCache
  -- renderer pipeline
  , rsRenderer          :: IORef GLRenderer
  , rsStorage           :: IORef GLStorage
  , rsAnimatedTextures  :: IORef [AnimatedTexture]
  }
  -- TODO: make a context data structure to make the render pipeline backend agnostic of opengl and viable to multiple contexts
  -- , rsRenderer           :: MVar (RayLibRenderer) -- IORef GLRenderer
  -- , rsStorage            :: MVar (RayLibStorage)  -- IORef GLStorage

initRenderSystem :: Map String Entry -> IO RenderSystem
initRenderSystem pk3 = do
  animationCache <- newIORef mempty
  bspCache <- newIORef mempty
  bspInstanceCache <- newIORef mempty
  bspShaderCache <- newIORef mempty
  md3SkinCache <- newIORef mempty
  md3Cache <- newIORef mempty
  md3InstanceCache <- newIORef mempty
  md3ShaderCache <- newIORef mempty
  characterCache <- newIORef mempty
  quadCache <- newIORef mempty
  textureCache <- newIORef mempty
  shMap <- loadShaderMap pk3
  let (inputSchema,_) = createRenderInfo shMap mempty mempty
  storage <- allocStorage inputSchema
  renderer <- fromJust <$> loadQuake3Graphics storage "SimpleGraphics.json"
  rendererRef <- newIORef renderer
  storageRef <- newIORef storage
  animatedTextures <- newIORef []
  -- default textures
  whiteTexture <- uploadTexture2DToGPU' False False False False $ ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 255 255 255) 1 1
  let redBitmap x y = let v = if (x+y) `mod` 2 == 0 then 255 else 0 in PixelRGB8 v v 0
  checkerTexture <- uploadTexture2DToGPU' False False False False $ ImageRGB8 $ generateImage redBitmap 2 2
  tableTextures <- initTableTextures
  initStorageDefaultValues tableTextures storage
  pure $ RenderSystem
    { rsFileSystem        = pk3
    , rsShaderMap         = shMap
    , rsCheckerTexture    = checkerTexture
    , rsWhiteTexture      = whiteTexture
    , rsTableTextures     = tableTextures
    , rsAnimationCache    = animationCache
    , rsBSPCache          = bspCache
    , rsBSPInstanceCache  = bspInstanceCache
    , rsBSPShaderCache    = bspShaderCache
    , rsMD3SkinCache      = md3SkinCache
    , rsMD3Cache          = md3Cache
    , rsMD3InstanceCache  = md3InstanceCache
    , rsMD3ShaderCache    = md3ShaderCache
    , rsCharacterCache    = characterCache
    , rsQuadCache         = quadCache
    , rsTextureCache      = textureCache
    , rsRenderer          = rendererRef
    , rsStorage           = storageRef
    , rsAnimatedTextures  = animatedTextures
    }

loadResources :: RenderSystem -> [Resource] -> [Picture] -> IO ()
loadResources renderSystem resources pictures = do
  -- load new models
  (newMD3Materials,newBSPMaterials,md3Cache,bspCache,_skinCache) <- updateModelCache renderSystem resources pictures
  -- check new materials
  (storage,renderer,md3InstanceCache,bspInstanceCache,characterCache,quadCache) <- updateRenderCache renderSystem newMD3Materials newBSPMaterials
  return ()

loadAnimation :: Map String Entry -> String -> IO GCH.Character
loadAnimation pk3 name = case Map.lookup name pk3 of
  Nothing -> fail $ "file not found: " ++ name
  Just a -> parseCharacter name . BS8.unpack <$> readEntry a >>= \case
    Left message  -> fail message
    Right a -> pure a

loadMD3Skin :: Map String Entry -> String -> IO MD3.MD3Skin
loadMD3Skin pk3 name = case Map.lookup name pk3 of
  Nothing -> fail $ "file not found: " ++ name
  Just a -> readMD3Skin <$> readEntry a

loadMD3 :: Map String Entry -> String -> IO GPUMD3
loadMD3 pk3 name = case Map.lookup name pk3 of
  Nothing -> fail $ "file not found: " ++ name
  Just a -> readMD3 . LB.fromStrict <$> readEntry a >>= uploadMD3

loadBSP :: Map String a -> Map String Entry -> String -> IO GPUBSP
loadBSP shaderMap pk3 name = case Map.lookup name pk3 of
  Nothing -> fail $ "file not found: " ++ name
  Just a -> readBSP . LB.fromStrict <$> readEntry a >>= uploadBSP (Map.keysSet shaderMap)

initStorageDefaultValues :: TableTextures -> GLStorage -> IO ()
initStorageDefaultValues tableTextures storage = do
  let slotU           = uniformSetter storage
      overbrightBits  = 0
  uniformM44F "worldMat" slotU $ mat4ToM44F idmtx
  uniformV3F "entityRGB" slotU $ V3 1 1 1
  uniformFloat "entityAlpha" slotU 1
  uniformFloat "identityLight" slotU $ 1 / (2 ^ overbrightBits)
  setupTableTextures slotU tableTextures

initStorageTextures :: RenderSystem -> GLStorage -> Map String CommonAttrs -> IO ()
initStorageTextures RenderSystem{..} storage usedMaterials = do
  let usedTextures = setNub [ (name,saTexture,saTextureUniform,caNoMipMaps)
                            | (name,CommonAttrs{..}) <- Map.toList usedMaterials
                            , StageAttrs{..} <- caStages
                            ]
      cachedTexture isMip isClamped shaderName imageName = do
        textureCache <- readIORef rsTextureCache
        let key = (imageName,isMip,isClamped)
        case HashMap.lookup key textureCache of
          Just texture -> return texture
          Nothing -> do
            texture <- loadQ3Texture isMip isClamped rsCheckerTexture rsFileSystem shaderName imageName
            writeIORef rsTextureCache $ HashMap.insert key texture textureCache
            return texture
  putStrLn "loading textures:"
  -- load textures
  animatedTextures <- fmap concat $ forM usedTextures $ \(shName,stageTex,texSlotName,noMip) -> do
      let texSetter = uniformFTexture2D (BS8.pack texSlotName) (uniformSetter storage)
          setTex isClamped img = texSetter =<< cachedTexture (not noMip) isClamped shName img
      case stageTex of
          ST_Map img          -> setTex False img >> return []
          ST_ClampMap img     -> setTex True img >> return []
          ST_AnimMap freq imgs   -> do
              txList <- mapM (cachedTexture (not noMip) False shName) imgs
              let txVector = V.fromList txList
              return [(fromIntegral (V.length txVector) / freq,texSetter,txVector)]
          _ -> return []

  writeIORef rsAnimatedTextures animatedTextures

updateModelCache :: RenderSystem -> [Resource] -> [Picture] -> IO (HashSet String,HashSet String,HashMap String GPUMD3,HashMap String GPUBSP,MD3SkinCache)
updateModelCache RenderSystem{..} resources pictures = do
  -- load new animations
  animationCache <- readIORef rsAnimationCache
  let newAnimationNames = setNub [name | R_AnimationCfg name <- resources, not $ HashMap.member name animationCache]
  newAnimations <- forM newAnimationNames $ loadAnimation rsFileSystem
  let animationCache' = animationCache `HashMap.union` HashMap.fromList (zip newAnimationNames newAnimations)
  unless (null newAnimationNames) $ putStrLn $ unlines $ "new animations:" : newAnimationNames
  writeIORef rsAnimationCache animationCache'

  -- load new md3 skins
  skinCache <- readIORef rsMD3SkinCache
  let newSkinNames = setNub [name | R_Skin name <- resources, not $ HashMap.member name skinCache]
  newSkins <- forM newSkinNames $ loadMD3Skin rsFileSystem
  let skinCache' = skinCache `HashMap.union` HashMap.fromList (zip newSkinNames newSkins)
  unless (null newSkinNames) $ putStrLn $ unlines $ "new skins:" : newSkinNames
  writeIORef rsMD3SkinCache skinCache'

  -- load new md3 models
  md3Cache <- readIORef rsMD3Cache
  let newModelNames = setNub [name | R_MD3 name <- resources, not $ HashMap.member name md3Cache]
  newModels <- forM newModelNames $ loadMD3 rsFileSystem
  let md3Cache' = md3Cache `HashMap.union` HashMap.fromList (zip newModelNames newModels)
  unless (null newModelNames) $ putStrLn $ unlines $ "new models:" : newModelNames
  writeIORef rsMD3Cache md3Cache'

  -- load new bsp maps
  bspCache <- readIORef rsBSPCache
  let newBSPNames = setNub [name | R_BSPMap name <- resources, not $ HashMap.member name bspCache]
  newBSPs <- forM newBSPNames $ loadBSP rsShaderMap rsFileSystem
  let bspCache' = bspCache `HashMap.union` HashMap.fromList (zip newBSPNames newBSPs)
  unless (null newBSPNames) $ putStrLn $ unlines $ "new bsp maps:" : newBSPNames
  writeIORef rsBSPCache bspCache'

  -- collect new materials
  md3ShaderCache <- readIORef rsMD3ShaderCache
  bspShaderCache <- readIORef rsBSPShaderCache
  let pictureMaterials  = HashSet.fromList (map pictureShader pictures ++ [name | R_Shader name <- resources])
      skinMaterials     = HashSet.fromList (concatMap Map.elems newSkins)
      newMD3Materials   = HashSet.unions (pictureMaterials : skinMaterials : map gpumd3Shaders newModels) `HashSet.difference` md3ShaderCache
      newBSPMaterials   = HashSet.unions (map gpubspShaders newBSPs) `HashSet.difference` bspShaderCache
  return (newMD3Materials,newBSPMaterials,md3Cache',bspCache',skinCache')

updateRenderCache :: RenderSystem -> HashSet String -> HashSet String -> IO (GLStorage,GLRenderer,MD3InstanceCache,BSPInstanceCache,CharacterCache,QuadCache)
updateRenderCache renderSystem@RenderSystem{..} newMD3Materials newBSPMaterials
  | HashSet.null newMD3Materials && HashSet.null newBSPMaterials = do
      md3InstanceCache <- readIORef rsMD3InstanceCache
      bspInstanceCache <- readIORef rsBSPInstanceCache
      characterCache <- readIORef rsCharacterCache
      quadCache <- readIORef rsQuadCache
      storage <- readIORef rsStorage
      renderer <- readIORef rsRenderer
      return (storage,renderer,md3InstanceCache,bspInstanceCache,characterCache,quadCache)
  | otherwise = do
      md3ShaderCache <- readIORef rsMD3ShaderCache
      putStrLn $ unlines $ "new md3 materials:" : HashSet.toList newMD3Materials
      let md3ShaderCache' = md3ShaderCache `HashSet.union` newMD3Materials
      writeIORef rsMD3ShaderCache md3ShaderCache'

      bspShaderCache <- readIORef rsBSPShaderCache
      putStrLn $ unlines $ "new bsp materials:" : HashSet.toList newBSPMaterials
      let bspShaderCache' = bspShaderCache `HashSet.union` newBSPMaterials
      writeIORef rsBSPShaderCache bspShaderCache'

      let (inputSchema,usedMaterials) = createRenderInfo rsShaderMap bspShaderCache' md3ShaderCache'
      storage <- allocStorage inputSchema
      -- load new images and set storage texture uniforms
      initStorageTextures renderSystem storage usedMaterials
      initStorageDefaultValues rsTableTextures storage

      writeSampleMaterial usedMaterials
      let filename = show (crc32 . BS8.pack $ show usedMaterials) ++ "_ppl.json"
      compileQuake3GraphicsCached filename >>= \ok -> unless ok $ fail "no renderer"
      renderer <- fromJust <$> loadQuake3Graphics storage filename
      disposeRenderer =<< readIORef rsRenderer
      --renderer <- readIORef rsRenderer
      --setStorage renderer storage
      -- TODO: make sure resources are released
      writeIORef rsStorage storage
      writeIORef rsRenderer renderer
      writeIORef rsMD3InstanceCache mempty
      writeIORef rsBSPInstanceCache mempty
      writeIORef rsCharacterCache mempty
      writeIORef rsQuadCache mempty
      return (storage,renderer,mempty,mempty,mempty,mempty)

data InstanceCache
  = InstanceCache
  { _newMD3        :: !MD3InstanceCache
  , _newBSP        :: !BSPInstanceCache
  , _newCharacter  :: !CharacterCache
  , _newQuad       :: !QuadCache
  , _oldMD3        :: !MD3InstanceCache
  , _oldBSP        :: !BSPInstanceCache
  , _oldCharacter  :: !CharacterCache
  , _oldQuad       :: !QuadCache
  }

makeLenses ''InstanceCache

initCache :: MD3InstanceCache -> BSPInstanceCache -> CharacterCache -> QuadCache -> InstanceCache
initCache = InstanceCache mempty mempty mempty mempty

type RenderM = StateT InstanceCache IO

getInstance :: (Eq k,Hashable k) => Lens' InstanceCache (HashMap k [a]) -> Lens' InstanceCache (HashMap k [a]) -> k -> IO a -> RenderM a
getInstance oldCache newCache name create = HashMap.lookup name <$> use oldCache >>= \case
  Just (model:_) -> do -- use exising instance
    oldCache %= HashMap.adjust tail name
    return model
  _ -> do -- create new instance
    model <- liftIO create
    newCache %= HashMap.insertWith (++) name [model]
    return model

renderScene :: RenderSystem -> Float -> Scene -> IO ()
renderScene a b c = do
  --printTimeDiff "scene process time: " $ do
    renderScene' a b c
  --printTimeDiff "render time: " $ do
    renderFrame =<< readIORef (rsRenderer a)

renderScene' :: RenderSystem -> Float -> Scene -> IO ()
renderScene' renderSystem@RenderSystem{..} effectTime Scene{..} = do
  -- load new models
  let resources = concatMap asResource renderables
  (newMD3Materials,newBSPMaterials,md3Cache,bspCache,skinCache) <- updateModelCache renderSystem resources pictures

  -- check new materials
  (storage,renderer,md3InstanceCache,bspInstanceCache,characterCache,quadCache) <- updateRenderCache renderSystem newMD3Materials newBSPMaterials

  let Camera{..} = camera

  -- create new instances
  let addInstance :: Renderable -> RenderM ()
      addInstance (MD3 position orientation scale rgba name) = do
        MD3Instance{..} <- getInstance oldMD3 newMD3 name $ do
          putStrLn $ "new instance: " ++ name
          addGPUMD3 storage (md3Cache HashMap.! name) mempty ["worldMat","entityRGB","entityAlpha"]
        liftIO $ do
          forM_ md3instanceObject $ \obj -> do
            enableObject obj $ True --((\frame -> boxInFrustum (position + frMins frame + frOrigin frame) (position + frMaxs frame + frOrigin frame) cameraFrustum) . V.head . mdFrames) md3instanceModel  --pointInFrustum position cameraFrustum
            -- set model matrix
            uniformM44F "worldMat" (objectUniformSetter obj) . mat4ToM44F . fromProjective $ toWorldMatrix position orientation scale
            uniformV3F "entityRGB" (objectUniformSetter obj) . vec3ToV3F $ trim rgba
            uniformFloat "entityAlpha" (objectUniformSetter obj) $ _4 rgba

      addInstance (BSPMap name) = do
        BSPInstance{..} <- getInstance oldBSP newBSP name $ do
          -- creates new instance from model cache
          putStrLn $ "new instance: " ++ name
          bspInstance@BSPInstance{..} <- addGPUBSP rsWhiteTexture storage (bspCache HashMap.! name)
          -- set bsp map world matrix
          forM_ bspinstanceSurfaces $ mapM_ (\o -> uniformM44F "worldMat" (objectUniformSetter o) $ mat4ToM44F idmtx)
          return bspInstance
        liftIO $ cullSurfaces bspinstanceBSPLevel cameraPosition cameraFrustum bspinstanceSurfaces

      addInstance (MD3Character position orientation scale rgba name skin) = do
        character <- getInstance oldCharacter newCharacter (name,skin) $ do
          -- creates new instance from model cache
          putStrLn $ printf "new instance: %s %s" name skin
          addCharacterInstance rsFileSystem storage name skin
        liftIO $ setupGameCharacter character effectTime cameraFrustum position orientation scale rgba

      addInstance (MD3New md3Data) = setupMD3Data (one :: Proj4) md3Data

      addInstance _ = pure () -- TODO

      setupMD3Data :: Proj4 -> MD3Data -> RenderM ()
      setupMD3Data baseMat MD3Data{..} = do
        md3Instance@MD3Instance{..} <- getInstance oldMD3 newMD3 md3ModelFile $ do
          putStrLn $ "new instance: " ++ md3ModelFile
          let skin = maybe mempty (skinCache HashMap.!) md3SkinName
          addGPUMD3 storage (md3Cache HashMap.! md3ModelFile) skin ["worldMat","entityRGB","entityAlpha"]
        let localMat = toWorldMatrix md3Position md3Orientation md3Scale .*. baseMat :: Proj4
        -- add md3 to the scene
        liftIO $ do
          case md3Frame of
            Just frameIndex -> setMD3Frame md3Instance frameIndex
            Nothing         -> pure ()
          forM_ md3instanceObject $ \obj -> do
            enableObject obj $ True -- TODO: pointInFrustum md3Position cameraFrustum ; handle md3 collision geometry + local transformations
            -- set model matrix
            uniformM44F  "worldMat"  (objectUniformSetter obj) . mat4ToM44F $ fromProjective localMat
            uniformV3F   "entityRGB" (objectUniformSetter obj) . vec3ToV3F  $ trim md3RGBA
            uniformFloat "entityAlpha" (objectUniformSetter obj) $ _4 md3RGBA

        -- handle attachments
        forM_ md3Attachments $ \(HRayLib3d.GameEngine.Scene.Tag tagName, md3Data) -> do
          let childMat = getTagProj4 md3Instance md3Frame tagName .*. localMat
          setupMD3Data childMat md3Data

      tagToProj4 :: MD3.Tag -> Proj4
      tagToProj4 MD3.Tag{..} = translateAfter4 tgOrigin (orthogonal . toOrthoUnsafe $ tgRotationMat)

      getTagProj4 :: MD3Instance -> Maybe Int -> BS8.ByteString -> Proj4
      getTagProj4 MD3Instance{..} frame name = let tags = MD3.mdTags md3instanceModel in case frame >>= \i -> tags V.!? (i `mod` V.length tags) >>= HashMap.lookup name of
        Nothing   -> idmtx
        Just tag  -> tagToProj4 tag

      addPicture :: Picture -> RenderM ()
      addPicture picture@Picture{..} = do
        quad@QuadInstance{..} <- getInstance oldQuad newQuad pictureShader $ do
          putStrLn $ "new quad instance: " ++ pictureShader
          addQuad storage pictureShader
        liftIO $ do
          let (viewportWidth, viewportHeight) = cameraViewportSize
              viewProj = ortho 0 (fromIntegral viewportWidth) (fromIntegral viewportHeight) 0 0 1
          uniformM44F "viewProj" (objectUniformSetter quadObject) . mat4ToM44F $ transpose viewProj
          updateQuad quad picture
          enableObject quadObject True

  InstanceCache{..} <- execStateT (mapM_ addInstance renderables >> mapM_ addPicture pictures) (initCache md3InstanceCache bspInstanceCache characterCache quadCache)
  writeIORef rsMD3InstanceCache $ HashMap.unionWith (++) md3InstanceCache _newMD3
  writeIORef rsBSPInstanceCache $ HashMap.unionWith (++) bspInstanceCache _newBSP
  writeIORef rsCharacterCache $ HashMap.unionWith (++) characterCache _newCharacter
  writeIORef rsQuadCache $ HashMap.unionWith (++) quadCache _newQuad

  -- hide unused instances
  let hideMD3 :: MD3Instance -> IO ()
      hideMD3 MD3Instance{..} = forM_ md3instanceObject $ flip enableObject False
  forM_ (concat $ HashMap.elems _oldMD3) hideMD3
  forM_ (concat $ HashMap.elems _oldBSP) $ \BSPInstance{..} -> forM_ bspinstanceSurfaces $ mapM_ (flip enableObject False)
  forM_ (concat $ HashMap.elems _oldCharacter) $ \CharacterInstance{..} -> do
    hideMD3 characterinstanceHeadModel
    hideMD3 characterinstanceUpperModel
    hideMD3 characterinstanceLowerModel
  forM_ (concat $ HashMap.elems _oldQuad) $ \QuadInstance{..} -> enableObject quadObject False

  setFrameUniforms effectTime camera storage =<< readIORef rsAnimatedTextures

  --renderFrame renderer

setFrameUniforms :: Float -> Camera -> GLStorage -> [AnimatedTexture] -> IO ()
setFrameUniforms time Camera{..} storage animatedTextures = do
  -- set uniforms
  let uniMap = uniformSetter storage

      rot   = orthogonal $ leftOrthoU cameraOrientation
      view  = fromProjective $ translateBefore4 (neg cameraPosition) rot
      (w,h) = cameraViewportSize

      viewMat     = mat4ToM44F view
      viewProj    = mat4ToM44F $ view .*. cameraProjection
      orientation = mat4ToM44F . fromProjective $ rot

  uniformFloat "time"        uniMap time
  uniformV3F   "viewOrigin"  uniMap $ vec3ToV3F cameraPosition
  uniformM44F  "viewMat"     uniMap viewMat
  uniformM44F  "viewProj"    uniMap viewProj
  uniformM44F  "orientation" uniMap orientation

  setScreenSize storage (fromIntegral w) (fromIntegral h)

  forM_ animatedTextures $ \(animTime,texSetter,v) -> do
    let (_,i) = properFraction (time / animTime)
        idx = floor $ i * fromIntegral (V.length v)
    texSetter $ v V.! idx
