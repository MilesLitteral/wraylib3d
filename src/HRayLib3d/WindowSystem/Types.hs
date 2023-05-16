{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-- {-# LANGUAGE LinearTypes      #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE DeriveAnyClass      #-}

module Manifest.Types where

import Data.List
import Data.Word (Word8)
import Data.Text (Text, pack, unpack, isSuffixOf)
import Data.Default
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import qualified Data.Massiv.Array as A

import Codec.Picture
import Codec.Picture.Png

import Control.Applicative
import Control.Monad
import Control.Lens
import Control.Lens.TH
import Control.Lens.Lens
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.QSem (QSem, newQSem, waitQSem, signalQSem)

import Monomer
import System.Directory
import System.FilePath
import System.Process
import System.IO (Handle)

import Maniga.Type.Base
import Maniga.Type.Record
import Maniga.Control.GAMonad
import qualified Manipipe as MP
import qualified Manipipe.Local.Type as MP
import qualified Manipipe.Local.LocalBlender as MP
-- import Maniga.Type.StateUpdate

import Manifest.Utils.Log
import Manifest.Utils.Json
-- import Manifest.Utils.Communication (FutHandle(..))

import TextShow

data CompModel = CompModel {
  _listA :: [Int],
  _listB :: [Int]
} deriving (Eq, Show)

data BytesImage = BytesImage {
  name    :: Text,
  bytes   :: BS.ByteString,
  size    :: Size
} deriving (Eq, Show)

data RenderType 
  = StandardRender
  | PresenceRender
  | VisualizationRender
  deriving (Eq, Show)

data FutHandle = FutHandle {
  threadID :: ThreadId,
  procSem  :: MVar (Chan GPUCommand) 
} deriving(Eq, Show)

data GPUCommand
  = DiffImages {imgA    :: MP.Image MP.RGBA Float, imgB :: MP.Image MP.RGBA Float}
  | ImageResp  {imgResp :: MP.Image MP.RGBA Float}
  | GPUExit
  deriving (Eq, Show)

data ManieyeImageSource 
  = ManieyeImageFolder {images :: [FilePath]}
  | ManieyeDifferenceImages {inputA :: [FilePath], inputB :: [FilePath]}
  | ManieyeDifferenceFolder {diffImages :: [BytesImage]}
  | ManieyeImageRender {localBlend :: MP.LocalBlender, typeOf :: RenderType, sceneVects :: (A.Array A.S A.Ix2 Float), groupName :: String}
  | ManieyeImageNothing 
  deriving (Eq, Show)

data ManieyeChartSource
  = ManieyeMaskChart {maskData :: (A.Array A.S A.Ix2 Float)}
  | ManieyeSVChart   {
      iSvData   :: (A.Array A.S A.Ix1 Float),
      pSvData   :: Maybe (A.Array A.S A.Ix1 Float)
    } deriving (Eq, Show)

data GAHandle = GAHandle {
  tid    :: ThreadId,
  gaSem  :: Chan (GARecord) 
} deriving(Eq, Show)

-- Ternary Function Helper
data Ternary a = a :? a

infixl 0 ?
infixl 1 :?

(?) :: Bool -> Ternary a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y
-- Ternary Function Helper

data DatasetDir  = Dataset {dirPath :: FilePath} |  None deriving(Ord, Show, Eq)

data ManiexScript 
  = Null
  | Script {scriptPath   :: FilePath}
  deriving(Show, Eq)

data ManiexWeights
  = Nil --lol haskell
  | Weights {weightsPath :: FilePath}
  deriving(Show, Eq)

data ManieyeModel = ManieyeModel {
  _datasetDir             :: FilePath,
  _predictionDirs         :: [String],
  _selectedExperimentC    :: FilePath,
  _selectedPrediction     :: String,
  _selectedCheckpoint     :: FilePath,
  _imageSources           :: [ManieyeImageSource],
  -- _diffSources         :: [BytesImage],
  _memImages              :: [BytesImage],
  _chartSources           :: [ManieyeChartSource],
  _chartImageLabels       :: [(String, Int)],
  _chartImages            :: [BytesImage],
  _currentInferenceImages :: [BytesImage],
  _imageIndex             :: Double,
  _activeCheckpoints      :: [DatasetDir],
  _selectedGraphic        :: DatasetDir,
  _activeGraphics         :: [DatasetDir],
  _selectedGraphicSrc     :: FilePath
  -- _inputAndPredPairs    :: [(FilePath, Maybe FilePath)]
} deriving (Eq, Show)

data ManifestModel = ManifestModel {
  _block              :: Bool,
  _showManiga         :: Bool,
  _showAdaptation     :: Bool,
  _showInspector      :: Bool,
  _showSelection      :: Bool, -- This will be tricky
  _showUserInput      :: Bool,
  _showDiffernence    :: Bool,
  _showPredictorTraining :: Bool,
  _showInferenceEval  :: Bool,
  _showDeployment     :: Bool,
  _showImages         :: Bool,
  _showManiexS        :: Bool,
  _inspectorElements  :: [Text],
  _inspectorElement   :: Text,
  _imagePath          :: Text,  -- The Default Background Image of the Application when Idle
  _experimentPath     :: Text,
  _inputImageMem      :: BytesImage,
  _predImageMem       :: BytesImage,
  _diffImageMem       :: BytesImage,
  _imagePairs         :: [(FilePath, Maybe FilePath)],
  _subprocesses       :: [ProcessHandle],
  _subproc_log        :: [Handle],
  _chartImage         :: Text,
  _dummyPlug          :: Text, --A DEBUG LENS, DON'T DELETE IT

  _selected           :: Int,
  _values             :: Int,

  --Maniga Lens
  _currentGeneration  :: Maniga.Type.Base.Generation,
  _currentLoss        :: Maniga.Type.Base.Loss,
  _currentHistogram   :: Maniga.Type.Base.Generation, 
  _currentSlider      :: Double,
  _smoothSigmaValue   :: Float,
  _mutationBaseline   :: Int,
  _nucleotides        :: Float,
  _bagSizePreset      :: Int,       --Smooth Sigma
  _imageSrc           :: BS.ByteString,
  _differenceImage    :: Text,
  _mutationRateValue  :: Float,
  _gaRecords          :: [GARecord], --Mutation Rate

  _experimentPaths    :: Text,
  _experimentName     :: Text,
  _blendFileName      :: Text,

  --Maniex Lens
  _startManiapi       :: Bool,
  _startManiex        :: Bool,
  _blend_sources   :: Text,
  _dataset_dir     :: Text,
  _output_dir      :: Text,
  _dataset_name    :: Text,
  _classifier_dir  :: Text,
  _weights_dir     :: Text,
  _model_weights   :: Text,
  _ide_command     :: Text,
  _iterationCount  :: Text,
  _render_function :: Text,
  _maskedAnnotationsPath :: Text,
  _availableAnnotations ::  [IntakeSlice],
  _annotationSelections :: [Bool],
  --_modelChoices       :: [ManiexModels] --perhaps make a data ManiexModel?
  --_selectedModel      :: ManiexModels

  -- Experiment Monitoring
  _manigaRecord    :: Text,
  _maniexRecord    :: Text,

  _manifilePath    :: String,
  _initialDir      :: Text,
  _predDir         :: Text,
  _dummyBool       :: Bool,

  -- Manieye Lens
  _datasetDirTextField :: Text,
  _manieyeModel        :: ManieyeModel,
  _selectedExperimentDir :: DatasetDir,
  _activeExperiments   :: [DatasetDir],
  _selectedRun      :: DatasetDir,
  _activeRuns       :: [DatasetDir],
  _gpuContext       :: Maybe FutHandle,
  _subprocListeners :: Handle,
  _gaContext        :: Maybe GAHandle,
  _playback         :: Bool,
  _skipTraining     :: Bool,
  _skipInference    :: Bool,
  _userHome         :: String,
  _resetManiapi     :: Bool,
  _maniex_install_path :: Text,
  _ml_backend        :: String,
  _ml_backend_colorA :: String,
  _ml_backend_colorB :: String,
  _ml_backend_colorC :: String,
  --_viewingMode       :: ViewMode,
  _selectedExperiment  :: Text,
  _selectedChk         :: DatasetDir,
  _selectedInferChk     :: DatasetDir,
  _helpOverlaysEnabled :: Bool
  --_experimentEnv   :: Maybe Text
} deriving (Eq, Show)

data ApplicationFramework 
  = ManifestView ManifestModel
    -- |ManigramView ManigramModel
    -- |ManiexView   ManiexModel
  deriving(Show, Eq)

data ViewMode =
  DATASET
  |EXPERIMENT
  |FOLDER
  deriving(Show, Eq, Enum, TextShow)

data ManigaEvt 
  = ManigaInit 
  deriving(Show, Eq)

data ManifestEvt
  = ManimerInit 
  | ForceUpdateImage   Text
  | CreateAnnotations  ManifestModel
  | UpdateAnnotations  [Text]
  | AnnotationSelected Bool
  | UpdateParameters   (Maybe Float)  Float  
  | ExperimentHook     [GARecord]
  | DoDifference       Double
  | SetExperimentPath  Text Text Double
  | AutoResolve         
  | SetHover           WidgetKey
  | StartManiex        ManifestModel
  | ConfigureManiex    ManifestModel
  | ConfigureManiexPyTorch ManifestModel
  | RebootManiex       ManifestModel
  | StartManiAPI       ManifestModel
  | StartManigram      ManifestModel
  | StartManiga        ManifestModel
  | StartManiBlend     ManifestModel
  -- | UpdateManiga       GARecord
  -- | RecurseManiga      ManifestModel
  | ManifileToChart    FilePath
  | ManifileToHeatmap  FilePath
  | ChartFromPath      FilePath
  | HeatmapFromPath    FilePath
  | SetPath            Text
  | SetHeatPath        Text
  | IDE                Text
  | SetAnnotationText  [IntakeSlice]  
  | UpdateIntake       [IntakeSlice]
  | ManieyeUpdate      [(FilePath, Maybe FilePath)]
  | UpdateImages       BytesImage BytesImage BytesImage
  | PrevImage
  | NextImage
  | GenerateHeatMap    String
  | SetInitalDatasetPath
  | SetPredictedDatasetPath
  | SetDatasetPath BytesImage BytesImage
  | OpenPath String
  | RefreshImagePairs       
  | HandleFile
  | SetLensPath (ASetter ManifestModel ManifestModel Text Text) Text
  | AddToIntake
  | SeekButton Bool
  | PlayBack   
  | PlayBackCycle --Bool
  | CleanupManieye
  | ChangeImages Double
  | GetNewImage Int Int
  | GetNewChart Int Int
  | SetNewImage Int BytesImage
  | SetNewChart Int BytesImage
  | SetDatasetDir --FilePath
  | UpdatePredDirList [String]
  | UpdateImageSources FilePath [FilePath] [ManieyeImageSource] [ManieyeChartSource] (Maybe ReconSpec)
  | UpdateRunSources   FilePath [FilePath] [ManieyeImageSource] [ManieyeChartSource] (Maybe FilePath)
  | SetPredDir String
  | LoadRootDatasetPath FilePath
  | SetActiveDatasets  [FilePath]
  | SetFutCont FutHandle
  | SetBinaryHandles Handle ProcessHandle Bool
  | CaptureManigaRecord GARecord
  | ListenGACont
  | SetGACont GAHandle
  | StartTensorBoard ManifestModel
  | RollOutDrawer    String
  | UpdateRunSource FilePath 
  | UpdateCheckpointSource  FilePath 
  | UpdateCheckpointSources FilePath [FilePath] [ManieyeImageSource] [ManieyeChartSource] (Maybe FilePath)
  | UpdateInferenceCheckpointSources FilePath [FilePath] [ManieyeImageSource] [ManieyeChartSource] (Maybe FilePath)
  | UpdateInferenceRunSource        FilePath 
  | UpdateInferenceCheckpointSource FilePath
  | UpdateGraphicSource     FilePath 
  | UpdateGraphicSources    FilePath [FilePath] 
  | UpdatePredictionSource  FilePath
  | SetHome String
  | DefaultRollOut
  | InstallManiex ManifestModel
  | UnInstallManiex ManifestModel
  | SetMLBackend String
  | MarkVisibleGraphs
  | RefreshVisibleCharts Double 
  | ListRefresh
  | DoNothing
  deriving (Eq, Show)

instance Show (ASetter ManifestModel ManifestModel Text Text) where
    show x = Prelude.show x

instance Show ((Chan GARecord)) where
  show x = "Chan GARecord"
  
instance Eq   (ASetter ManifestModel ManifestModel Text Text) where
    x == y = x == y

instance Eq   (MP.Image MP.RGBA Float) where
  x == y = x == y

instance Default DatasetDir where
  def = None

nullWidget :: WidgetNode ManifestModel ManifestEvt
nullWidget =  button "null" DoNothing

executeDeed :: Floating a => a -> (a -> r) -> r
executeDeed a k = k (sqrt a)

defaultRollout :: Bool -> ManifestEvt
defaultRollout a = DefaultRollOut

genViewModes   :: [ViewMode]
genViewModes = enumFrom (toEnum 0)

maniexWeightsType :: ManiexWeights -> Text
maniexWeightsType mm = showt (show mm)

length :: ManieyeImageSource -> Int
length source = case source of 
  ManieyeImageFolder imageList     -> Data.List.length imageList
  ManieyeDifferenceFolder diffList -> Data.List.length diffList
  ManieyeImageRender _  _ sv _ -> let (A.Sz2 x _)  = A.size sv in x
  ManieyeImageNothing -> 0

emptyBytesImage :: BytesImage
emptyBytesImage = BytesImage "" BS.empty (Size 0 0)

defaultBytesImage :: BytesImage
defaultBytesImage = do
  let v :: [Word8] = take (512 * 512 * 4) [255, 255..]
  BytesImage "Default"    (BS.pack v) (Size 512 512) 

defaultInitialBytesImage :: IO BytesImage
defaultInitialBytesImage    = do
  dynImg  <- readImage ("./assets/images/initial.png")
  let img :: Either String (Image PixelRGBA8) = case dynImg of
            Left msg  -> Left msg
            Right img -> Right $ convertRGBA8 img
  case img of
    Left _    -> return $ emptyBytesImage  
    Right img -> return $ BytesImage (Data.Text.pack ("./assets/images/initial.png"))   (BS.pack $ V.toList  (imageData img)) (Size (fromIntegral $ imageWidth img) (fromIntegral $ imageHeight img))

defaultDifferenceBytesImage :: IO BytesImage
defaultDifferenceBytesImage = do
  dynImg  <- readImage ("./assets/images/difference.png")
  let img :: Either String (Image PixelRGBA8) = case dynImg of
                Left msg  ->  Left msg
                Right img -> Right $ convertRGBA8 img
  case img of
    Left _    -> return $ emptyBytesImage  
    Right img -> return $ BytesImage (Data.Text.pack ("./assets/images/difference.png")) (BS.pack $ V.toList  (imageData img)) (Size (fromIntegral $ imageWidth img) (fromIntegral $ imageHeight img))

defaultPredictedBytesImage ::  IO BytesImage
defaultPredictedBytesImage  =  do
    dynImg  <- readImage ("./assets/images/predicted.png")
    let img :: Either String (Image PixelRGBA8) = case dynImg of
                Left msg -> Left msg
                Right img -> Right $ convertRGBA8 img
    case img of
      Left _    -> return $ emptyBytesImage  
      Right ga  -> return $ BytesImage (Data.Text.pack ("./assets/images/predicted.png")) (BS.pack $ V.toList  (imageData ga)) (Size (fromIntegral $ imageWidth ga) (fromIntegral $ imageHeight ga))

defaultHandle :: Maybe Handle
defaultHandle = Nothing 

findPredDirs :: FilePath -> IO [String]
findPredDirs datasetDir = do 
  let outputsDir = datasetDir </> "Output/" 
  exists <- doesDirectoryExist outputsDir
  if not exists 
    then return []
    else do
      dirContents <- listDirectory outputsDir
      let contentsWithPath = Data.List.map (\f -> outputsDir </> f) dirContents 
      dirsOnly <- filterM doesDirectoryExist contentsWithPath
      return $ Data.List.map (takeFileName . takeDirectory) dirsOnly

-- Needs a more descriptive name to distinguish it from a similar function within PixelDifference
findInputDirectory :: FilePath -> IO (Maybe FilePath)
findInputDirectory basePath = do 
    let inputDirPath = basePath ++ "/Seq/"
    dirExists <- doesDirectoryExist inputDirPath
    let result = if dirExists 
                    then Just inputDirPath
                    else Nothing
    return result

findSceneDir :: FilePath -> IO (Maybe FilePath)
findSceneDir basePath = do 
    let sceneDirPath = basePath ++ "/Seq/" --Manipred doesnt use "/Scene/", this was a path in a test dataset
    print sceneDirPath
    dirExists <- doesDirectoryExist sceneDirPath
    let result = if dirExists 
                    then Just sceneDirPath
                    else Nothing
    return result

findTrainDir :: FilePath -> IO (Maybe FilePath)
findTrainDir basePath = do 
    let sceneDirPath = basePath ++ "/Train/"
    dirExists <- doesDirectoryExist sceneDirPath
    let result = if dirExists 
                    then Just sceneDirPath
                    else Nothing
    return result

mLoadFolderContents :: FilePath -> IO(Maybe [FilePath])
mLoadFolderContents path = do
    dirExists <- doesDirectoryExist path
    if dirExists 
      then Just <$> listDirectory path
      else return Nothing

    
loadFolderContents :: FilePath -> IO([FilePath])
loadFolderContents path = do
    dirs <- listDirectory path
    return dirs
    
loadDirContents :: FilePath -> IO([FilePath])
loadDirContents path = do
    dirs <- listDirectory path
    let filtered = Prelude.filter (Data.Text.isSuffixOf ".png") (Prelude.map (pack) dirs)
    let finalfilter = Prelude.map (unpack) filtered
    return finalfilter

setUVRenderMode :: MP.RenderOptions -> MP.RenderOptions
setUVRenderMode (MP.RenderOptions (MP.RenderSettings transparent colorMode colorDepth eeveeSamples _ renW renH lossRos lossCols) views retOps) = 
  MP.RenderOptions (MP.RenderSettings transparent colorMode colorDepth eeveeSamples MP.UvGradient renW renH lossRos lossCols) views retOps

testEvt :: Bool -> ManifestEvt
testEvt b = DoNothing

slice :: Int -> Int -> [a] -> [a]
slice start stop xs = fst $ splitAt (stop - start) (snd $ splitAt start xs)

sliceM :: Monad m  => Int -> Int -> [a] -> m [a]
sliceM start stop xs  = return $ fst $ splitAt (stop - start) (snd $ splitAt start xs)

makeLenses ''ManifestModel
makeLenses ''ManieyeModel
makePrisms ''ManieyeModel
makeLenses ''Album
makeLenses ''DatasetDir