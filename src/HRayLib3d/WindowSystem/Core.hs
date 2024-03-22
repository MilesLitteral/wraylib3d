{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings, TemplateHaskell, FunctionalDependencies, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Woverlapping-patterns #-}
module HRayLib3d.WindowSystem.Core where

import GHC.Generics    ( Generic )
import System.FilePath ( takeExtension )
import Control.Lens.TH ( abbreviatedFields, makeLenses, makeLensesFor, makeLensesWith )
import Data.Aeson      ( FromJSON(parseJSON), (.!=), (.:), (.:?), withObject )
import Data.Default    ( Default(..) )
import Data.Text       ( Text, pack  )
import Data.Binary     ( Binary )
import Monomer         ( Millisecond, Color )
import Monomer.Hagrid  ( Column (..), ColumnAlign (..), ColumnFooterWidget (..), ColumnSortKey (..), SortDirection (..), hagrid_, initialSort, scrollToRow, showOrdColumn, textColumn, widgetColumn)

import Data.Sequence (Seq ((:|>)))
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Time (Day, addDays, defaultTimeLocale, formatTime, fromGregorian)
import Data.Yaml.Parser (YamlValue(Mapping))

-- SDL2
-- Channel Monomer here, It's going to be necessary xD
-- Maybe even take some inspiration from it?
-- How about forking and modifying it for WRayLib3d's purposes

-- import SDL
-- import SDL.Video
-- import Linear        (V4(..))
-- import Control.Monad (unless)


-- vkWindowConfig :: WindowConfig
-- vkWindowConfig = WindowConfig
--     { windowBorder          = True
--     , windowHighDPI         = False
--     , windowInputGrabbed    = False
--     , windowMode            = Windowed
--     , windowGraphicsContext = VulkanContext
--     , windowPosition        = Wherever
--     , windowResizable       = False
--     , windowInitialSize     = V2 800 600
--     , windowVisible         = True
--     }

-- -- Handle Events
-- appLoop :: Renderer -> IO ()
-- appLoop renderer = do
--     events <- pollEvents
--     let eventIsQPress event =
--             case eventPayload event of
--             KeyboardEvent keyboardEvent -> keyboardEventKeyMotion keyboardEvent == Pressed && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
--             _ -> False
--         qPressed = any eventIsQPress events
--     rendererDrawColor renderer $= V4 100 0 255 255
--     -- renderPrimitive  renderer $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
--     -- SDL.glSwapWindow window
--     clear   renderer
--     present renderer
--     unless qPressed (appLoop renderer)

-- createSDLWindow :: IO ()
-- createSDLWindow = do
--     initializeAll
--     window   <- createWindow   "WRL3D (SDL2)" glWindowConfig -- vkWindowConfig --defaultWindow
--     renderer <- createRenderer window (-1)    defaultRenderer
--     icon     <- SDL.loadBMP "./assets/WRL3D.bmp"
--     setWindowIcon window icon
--     appLoop renderer
--     destroyWindow window
--     -- glSwapWindow window
--     quit

data ProjectSessionPrefs 
  = ProjectSessionPrefs {
    _projectPrefsPlatforms :: [Text],
    _projectPrefsXREnabled :: Bool
  } deriving(Generic, Eq, Show)

data ProjectSessionFile
  = ProjectSessionFile {
    _projectFileName       :: FilePath,
    _projectFileType       :: ProjectSessionFileType
  } deriving(Generic, Eq, Show)

data ProjectSession 
  = ProjectSession {
    _projectName        :: Text,
    _projectFiles       :: [ProjectSessionFile],
    _projectRealms      :: [Text],
    _projectPreferences :: ProjectSessionPrefs
  } deriving(Generic, Eq, Show)

instance Binary ProjectSession
instance Binary ProjectSessionFile
instance Binary ProjectSessionPrefs
instance Binary ProjectSessionFileType

data Spider = Spider
  { index   :: Int,
    species :: Text,
    sName   :: Text,
    dateOfBirth :: Day,
    weightKilos :: Double
  } deriving (Eq, Show)
  
data ProjectSessionFileType
  = Unknown
  | WRLProjectFile 
  | VSProjectFile
  | XCodeProjectFile
  | CMakeProjectFile
  | MakeProjectFile
  | WebGLProjectFile
  | WASMProjectFile
  | DjinniProjectFile
  | IOSProjectFile
  | AndroidProjectFile
  -- System Bundle Types
  | AssetBundleFile
  | ShaderCacheFile
  | GLTFAssetFile
  | GLBAssetFile
  | MD3AssetFile
  | PK3AssetFile
  | BSPAssetFile
  | DBFile
  | RealmFile
  | GemFile
  -- Shader File Types
  | Q3Shader
  | GLSLShaderFile
  | MetalShaderFile
  | HLSLShaderFile
  | MetalLibFile
  | MetalDyLibFile
  | DLLFile
  | LIBFile
  -- Component File Types
  | HaskellFile
  | CPPFile
  | CPPFileHeader
  -- Script File Types
  | RubyScriptFile
  | PythonScriptFile
  | LuaScriptFile
  | JavaScriptFile
  | WASMFile
  | GenericScriptAlt2
  | GenericScriptAlt3
  | GenericScript
  | TextFile
  -- Data Structure Type
  | HTMLFile
  | LCFile
  | XMLFile
  | JSONFile
  | MaterialFile
  | ZIPFile
  -- Audio File Types
  | OGGFile
  | WAVFile
  -- Misc
  | FolderFile
  | CloudbaseFile
  | LocalizationFile
  | MapBoxFile
  | PDFFile
  | UWPFile
  | ImageFile
  | OTFFile
  | TTFFile
  | TFFile
  | ARFile
  | XRFile
  deriving(Generic, Eq, Show, Enum)

matchExtType :: FilePath  -> ProjectSessionFileType
matchExtType pt = case takeExtension  pt of
  ".wrlp"        -> WRLProjectFile
  ".vsproj"      -> VSProjectFile
  ".xcproj"      -> XCodeProjectFile
  ".cmakelists"  -> CMakeProjectFile
  ".makefile"    -> MakeProjectFile
  ".webgl"       -> WebGLProjectFile
  ".wasm"        -> WASMProjectFile
  ".djinni"      -> DjinniProjectFile
  ".ipa"         -> IOSProjectFile
  ".gradle"      -> AndroidProjectFile

  --System Bundle Types
  ".assetBundle" -> AssetBundleFile
  ".shaderCache" -> ShaderCacheFile
  -- ".obj"      -> WavefrontOBJFile -- re-add
  ".glb"         -> GLBAssetFile
  ".gltf"        -> GLTFAssetFile
  ".pk3"         -> PK3AssetFile
  ".md3"         -> MD3AssetFile
  ".bsp"         -> BSPAssetFile
  ".db"          -> RealmFile
  "GemFile"      -> GemFile

  -- Shader File Types
  ".q3s"       -> Q3Shader
  ".glsl"      -> GLSLShaderFile
  ".metal"     -> MetalShaderFile
  ".hlsl"      -> HLSLShaderFile
  ".metallib"  -> MetalLibFile
  ".metaldysm" -> MetalDyLibFile
  ".dll"       -> DLLFile
  ".lib"       -> LIBFile


  -- Component File Types
  ".hs"   -> HaskellFile
  ".cpp"  -> CPPFile
  ".h"    -> CPPFileHeader
  ".hpp"  -> CPPFileHeader

  -- Script File Types
  ".rb"   -> RubyScriptFile
  ".py"   -> PythonScriptFile
  ".lua"  -> LuaScriptFile
  ".js"   -> JavaScriptFile
  ".jsx"  -> JavaScriptFile -- replace with JSX Icon/JSXFile
  ".wasm" -> WASMFile
  -- GenericScript     -> GenericScript
  -- GenericScriptAlt2 -> "./assets/images/script_alt.png"
  -- GenericScriptAlt3 -> "./assets/images/script_alt2.png"
  ".txt"  -> TextFile

  -- Data Structure Type
  ".html" -> HTMLFile
  ".lc"   -> LCFile
  ".xml"  -> XMLFile
  ".json" -> JSONFile
  ".mat"  -> MaterialFile
  ".zip"  -> ZIPFile

  -- Audio File Types
  ".ogg" -> OGGFile
  ".wav" -> WAVFile

  -- Misc
  ""                -> FolderFile
  ".cloudbase"      -> CloudbaseFile
  ".i18n"           -> LocalizationFile
  -- MapBoxFile     -> MapBoxFile
  ".pdf"            -> PDFFile
  ".uwp"            -> UWPFile
  ".otf"            -> OTFFile
  ".ttf"            -> TTFFile
  ".protoc"         -> TFFile
  -- ".glb"         -> ARFile -- technically, this is an alias for GLBFile, as all ARGames will use GLTFs in this capacity
  ".vrsettings"     -> XRFile
  ".png"            -> ImageFile
  ".jpg"            -> ImageFile
  ".jpeg"           -> ImageFile
  ".dds"            -> ImageFile
  ".tiff"           -> ImageFile
  _                 -> Unknown
  --_       -> error "Bad Type included in Project"

matchPFType :: ProjectSessionFileType -> Text
matchPFType pt = case pt of
  Unknown            -> "./assets/images/file-icon/file-unknown.png"
  FolderFile         -> "./assets/images/folder.png"
  WRLProjectFile     -> "./assets/images/file-icon/file-realm-project.png"
  VSProjectFile      -> "./assets/images/file-icon/file-vs.png"
  XCodeProjectFile   -> "./assets/images/file-icon/file-xcode.png"
  CMakeProjectFile   -> "./assets/images/file-icon/file-cmake.png"
  MakeProjectFile    -> "./assets/images/file-icon/automake_file.png"
  WebGLProjectFile   -> "./assets/images/webgl.png"
  WASMProjectFile    -> "./assets/images/file-icon/file-wasm.png"
  DjinniProjectFile  -> "./assets/images/file-icon/file-djinni.png"
  IOSProjectFile     -> "./assets/images/ios.png"
  AndroidProjectFile -> "./assets/images/android.png"

  --System Bundle Types
  AssetBundleFile    -> "./assets/images/file-assetBundle.png"
  ShaderCacheFile    -> "./assets/images/file-shader-cache.png"
  GLBAssetFile       -> "./assets/images/file-icon/file-glb.png"
  GLTFAssetFile      -> "./assets/images/file-icon/file-gltf.png"
  PK3AssetFile       -> "./assets/images/file-icon/file-pk3.png"
  MD3AssetFile       -> "./assets/images/file-icon/file-md3.png"
  BSPAssetFile       -> "./assets/images/file-icon/file-bsp.png"
  DBFile             -> "./assets/images/file-icon/file-db.png"
  RealmFile          -> "./assets/images/file-icon/file-realm-alt.png"
  GemFile            -> "./assets/images/file-icon/ruby-gem.png"

  -- Shader File Types
  Q3Shader         -> "./assets/images/file-icon/file-q3s.png"
  GLSLShaderFile   -> "./assets/images/file-icon/glsl-script.png"
  MetalShaderFile  -> "./assets/images/file-icon/metal-script.png"
  HLSLShaderFile   -> "./assets/images/file-icon/hlsl-script"
  MetalLibFile     -> "./assets/images/metallib.png"
  MetalDyLibFile   -> "./assets/images/metaldylib.png"
  DLLFile          -> "./assets/images/file-dll.png"
  LIBFile          -> "./assets/images/file-lib.png"

  -- Component File Types
  HaskellFile      -> "./assets/images/file-icon/file-haskell.png"
  CPPFile          -> "./assets/images/file-icon/file-cpp.png"
  CPPFileHeader    -> "./assets/images/file-icon/file-hpp.png" -- replace with a h/hpp icon

  -- Script File Types
  RubyScriptFile    -> "./assets/images/file-icon/ruby-script.png"
  PythonScriptFile  -> "./assets/images/file-icon/python-script.png"
  LuaScriptFile     -> "./assets/images/file-icon/lua-script.png"
  JavaScriptFile    -> "./assets/images/file-icon/js-script.png"
  WASMFile          -> "./assets/images/file-icon/file-wasm.png"
  GenericScript     -> "./assets/images/file-icon/generic-script.png"
  GenericScriptAlt2 -> "./assets/images/file-icon/script-file-generic.png"
  GenericScriptAlt3 -> "./assets/images/file-icon/script-file-generic-alt.png"
  TextFile          -> "./assets/images/file-icon/file-txt.png"

  -- Data Structure Type
  HTMLFile          -> "./assets/images/file-icon/file-html.png"
  LCFile            -> "./assets/images/file-icon/file-lc.png"
  XMLFile           -> "./assets/images/file-icon/file-xml.png"
  JSONFile          -> "./assets/images/file-icon/file-json.png"
  MaterialFile      -> "./assets/images/file-icon/file-material.png"
  ZIPFile           -> "./assets/images/file-icon/file-zip.png"

  -- Audio File Types
  OGGFile           -> "./assets/images/file-icon/file-audio.png"
  WAVFile           -> "./assets/images/file-icon/file-wav.png"

  -- Misc
  CloudbaseFile     -> "./assets/images/file-icon/cloudbase-file.png"
  LocalizationFile  -> "./assets/images/file-icon/file-localization.png"
  MapBoxFile        -> "./assets/images/file-icon/file-realm-alt.png"
  PDFFile           -> "./assets/images/file-icon/file-pdf.png"
  UWPFile           -> "./assets/images/file-icon/file-uwp.png"
  OTFFile           -> "./assets/images/file-icon/file-otf.png"
  TTFFile           -> "./assets/images/file-icon/ttf-file.png"
  TFFile            -> "./assets/images/file-icon/tf-file.png"
  ARFile            -> "./assets/images/file-icon/arAsset.png"
  XRFile            -> "./assets/images/file-icon/asset-xr.png"
  ImageFile         -> "./assets/images/image.png"
  --_       -> error "Bad Type included in Project"
  
data ControllerType 
  = XBOX_CONTROLLER
  |PS_CONTROLLER 
  |TP_CONTROLLER
  |GENERIC_CONTROLLER
  deriving (Eq, Show, Enum)

data Realm = 
  Realm {
    _rlmLevelName   :: Text,
    _rlmBsp         :: Text,
    _rlmMultiplayer :: Bool
  } deriving (Eq, Show)

data BooksModel = BooksModel {
  _bkmQuery                   :: Text,
  _bmkSearching               :: Bool,
  _bkmErrorMsg                :: Maybe Text,
  _bkmBooks                   :: [Book],
  _bkmFilePaths               :: [FilePath],
  _bmkSelected                :: Maybe Book,
  _bmkToolTips                :: Bool,
  _bmkMultiplayerEnabled      :: Bool,
  _bmkOptimizeShaders         :: Bool,
  _bmkOptimizeForWeb          :: Bool,
  _bmkRendererEnabled         :: Bool,
  _bmkDebugOnPlay             :: Bool,
  _bmkDedicatedServer         :: Bool,
  _bmkFileMenu                :: Bool,
  _bmkApplicationPreferences  :: Bool,
  _bmkRealmList               :: Bool,
  _bmkControllerPreferences   :: Bool,
  _bmkExportPreferences       :: Bool,
  _bmkRunPreferences          :: Bool,
  _bmkMPPreferences           :: Bool,
  _bmkProjectSession          :: Maybe ProjectSession,
  _bmkSideBarVisible          :: Bool,
  _bmkMaxSections             :: Int,
  _bmkActiveSection           :: Int,
  _bmkControllerBindings      :: [ControllerButton],
  _bmkBuildList               :: [BuildDescription],
  _bmkCloudList               :: [CloudDescription],
  _bmkActiveControllerBinding :: ControllerButton,
  _bmkActiveBuildList  :: BuildDescription,
  _bmkActiveCloudList  :: CloudDescription,
  _bmkCBAction         :: ControllerButtonAction,
  _bmkBUAction         :: BuildAction,
  _bmkCLAction         :: CloudAction,
  _bmkSearchableRealm  :: Bool,
  _bmkPrivateRealmOnly :: Bool,
  _bmkRealmVisibility  :: Text,
  _bmkSelectedCamera   :: Text,
  _bmkDisableScriptEngineExecution  :: Bool,
  _bmkLegacyRenderPipelineOnly      :: Bool,
  _bmkRunCurrentScene  :: Bool,
  _bmkRunNamedScene    :: Text,
  _bmkShowRenderer     :: Bool,
  _bmkShowPlatform     :: Bool,
  _bmkShowCloudSetup   :: Bool,
  _bmkShowRendererOptions   :: Bool,
  _bmkShowControllerSetup   :: Bool,
  _bmkShowAppProjectPrefs   :: Bool,
  _bmkShowAppRealmDB   :: Bool,
  _bmkShowWindowViewer :: Bool,
  _bmkIdeArgument      :: Text,
  _bmkCurrentRenderer  :: Text,
  _bmkSpiders          :: Seq Spider,
  _bmkColumns          :: [AppColumn],
  _bmkRowToScrollTo    :: Int,
  _bmkProjectTitleName :: Text,
  _bmkCurrentRealmName :: Text,
  _bmkControllerType   :: ControllerType,
  _bmkExternalDepsList :: Text,
  _bmkColorPicked      :: Color,
  _bmkWidgetSelected   :: Text,
  _bmkSpirVOnly        :: Bool,
  _bmkGlslOnly         :: Bool,
  _bmkColor1 :: Color,
  _bmkColor2 :: Color,
  _bmkColor3 :: Color,
  _bmkColor4 :: Color,
  _bmkShowFileSystem   :: Bool,
  _bmkShowMainRenderer :: Bool,
  _bmkThreeDimensional :: Bool,
  _bmkTwoDimensional   :: Bool,
  _bmkHandleFalse      :: Bool,
  _bmkProjectLoadedRealms :: [Realm],
  _bmkShowRealmViewer     :: Bool,
  _bmkSelectedRealm :: Realm
} deriving (Eq, Show)
-- darkTheme,
-- rowToScrollTo = 0

instance Default ControllerButton where
  def = ControllerButton {
    _bId          = 0,
    _bType        = HRayLib3d.WindowSystem.Core.Mapping,
    _bstatus      = Active,
    _bDescription = ""
  }
  
instance Default BuildDescription where
  def = BuildDescription{
    _buId          = 0,
    _buType        = Win64,
    _buStatus      = Build,
    _buDescription = ""
  }

instance Default CloudDescription where
  def = CloudDescription{
    _cId          = 0,
    _cType        = Firebase,
    _cStatus      = On,
    _cDescription = ""
  }

newtype AppColumn = AppColumn { enabled :: Bool } deriving (Eq, Show)

instance Default ProjectSession where
  def = ProjectSession "default" [] [] (ProjectSessionPrefs [] False)

data ControllerButtonAction
  = ControllerButtonNone
  | ControllerButtonCreate
  | ControllerButtonAdding
  | ControllerButtonEditing Int
  | ControllerButtonConfirmingDelete Int ControllerButton
  deriving (Eq, Show)

data CloudAction
  = CloudNone
  | CloudCreate
  | CloudAdding
  | CloudEditing Int
  | CloudConfirmingDelete Int CloudDescription
  deriving (Eq, Show)

data BuildAction
  = BuildNone
  | BuildCreate
  | BuildAdding
  | BuildEditing Int
  | BuildConfirmingDelete Int BuildDescription
  deriving (Eq, Show)

data ControllerListType
  = Mapping
  |  START
  |  SELECT
  |  OPTION
  |  SHARE
  |  HOME_BUTTON
  |  TRACKPAD --Playstation Only
  |  L_DPAD
  |  R_DPAD
  |  U_DPAD
  |  D_DPAD
  |  L_STICK
  |  R_STICK
  |  BUTTON_A
  |  BUTTON_B
  |  BUTTON_X
  |  BUTTON_Y
  |  BUTTON_L1
  |  BUTTON_R1
  |  BUTTON_L2
  |  BUTTON_R2
  |  BUTTON_L3
  |  BUTTON_R3
  |  PS_BUTTON_A        
  |  PS_BUTTON_B        
  |  PS_BUTTON_X        
  |  PS_BUTTON_Y        
  |  PS_BUTTON_L1       
  |  PS_BUTTON_R1       
  |  PS_BUTTON_L2       
  |  PS_BUTTON_R2       
  |  PS_BUTTON_L3       
  |  PS_BUTTON_R3 
  |  KEYBOARD_KEY
  |  L_MOUSE_KEY
  |  R_MOUSE_KEY
  |  C_MOUSE_KEY
  |  XR_INTERACTION
  |  XR_GESTURE
  deriving (Eq, Show, Enum)

data CloudListType
  = Firebase
  | GCP
  | Azure
  | Supabase
  deriving (Eq, Show, Enum)

data BuildListType
  = Win64
  | UWP
  | MacOS
  | Ubuntu
  | CMake
  | AutoMake 
  | Djinni
  | Android
  | IOS
  | WebGL_Build
  | WASM_Build
  | XR
  deriving (Eq, Show, Enum)

data TodoStatus
  = Pending
  | Done
  deriving (Eq, Show, Enum)

data ControllerButtonStatus
  = Active
  | Inactive
  deriving (Eq, Show, Enum)

data CloudStatus
  = On
  | Off
  deriving (Eq, Show, Enum)

data BuildStatus
  = Build
  | DontBuild
  deriving (Eq, Show, Enum)

data Book = Book {
  _bkTitle   :: Text,
  _bkAuthors :: [Text],
  _bkYear    :: Maybe Int,
  _bkCover   :: Maybe Int
} deriving (Eq, Show)

instance FromJSON Book where
  parseJSON = withObject "Book" $ \b -> Book
    <$> b .: "title"
    <*> b .:? "author_name" .!= []
    <*> b .:? "first_publish_year"
    <*> b .:? "cover_i"

data BookResp = BookResp {
  _brDocs :: [Book],
  _brFound :: Int
} deriving (Eq, Show)

instance FromJSON BookResp where
  parseJSON = withObject "BookResp" $ \b -> BookResp
    <$> b .: "docs"
    <*> b .: "numFound"

data BooksEvt
  = BooksInit
  | BooksSearch
  | BooksSearchResult BookResp
  | BooksSearchError Text
  | BooksShowDetails Book
  | BooksCloseDetails
  | BooksCloseError
  -- List Events
  | ControllerButtonInit
  | ControllerButtonNew
  | ControllerButtonAdd
  | ControllerButtonEdit Int          ControllerButton
  | ControllerButtonSave Int
  | ControllerButtonConfirmDelete Int ControllerButton
  | ControllerButtonCancelDelete
  | ControllerButtonDeleteBegin Int   ControllerButton
  | ControllerButtonDelete Int        ControllerButton
  | ControllerButtonShowEdit
  | ControllerButtonHideEdit
  | ControllerButtonHideEditDone
  | ControllerButtonCancel
  | BuildListInit
  | BuildListNew
  | BuildListAdd
  | BuildListEdit Int          BuildDescription
  | BuildListSave Int
  | BuildListConfirmDelete Int BuildDescription
  | BuildListCancelDelete
  | BuildListDeleteBegin Int   BuildDescription
  | BuildListDelete Int        BuildDescription
  | BuildListShowEdit
  | BuildListHideEdit
  | BuildListHideEditDone
  | BuildListCancel
  | CloudInit
  | CloudNew
  | CloudAdd
  | CloudEdit Int          CloudDescription
  | CloudSave Int
  | CloudConfirmDelete Int CloudDescription
  | CloudCancelDelete
  | CloudDeleteBegin Int   CloudDescription
  | CloudDelete Int        CloudDescription
  | CloudShowEdit
  | CloudHideEdit
  | CloudHideEditDone
  | CloudCancel
  -- End List Events
  | OpenFS
  | OpenAppPrefs
  | OpenRealmList
  | OpenControllerPrefs
  | OpenExportPrefs
  | OpenRunPrefs
  | OpenMPPrefs
  | RefreshProjectFilesTask
  | RefreshProjectFiles [FilePath]
  | ShowSection   Int
  | SideBarSummon Bool
  | OpenControllerSetup
  | OpenWindowViewing
  | OpenRealmDB 
  | FeedSpider Int
  | AddSpider
  | NameColumnResized Int
  | NameColumnSorted SortDirection
  | ScrollToOriginalIndex
  | XboxController
  | PsController
  | ThirdPartyController
  | FTPSearchResult BookResp 
  | FTPSearchError  Text 
  | AppSearchDir [FilePath]
  | StartIDE
  | AddRealm
  | RemoveRealm
  | OpenRealm Realm
  | UpdateRealmNameLocal  Text
  | UpdateRealmBSPNameLocal  Text
  | UpdateRealmMultiplayerLocal  Bool 
  | NoneB Bool
  | None
  deriving (Eq, Show)

data ControllerButton = ControllerButton {
  _bId          :: Millisecond,
  _bType        :: ControllerListType,
  _bstatus      :: ControllerButtonStatus,
  _bDescription :: Text
} deriving (Eq, Show)

data CloudDescription = CloudDescription {
  _cId          :: Millisecond,
  _cType        :: CloudListType,
  _cStatus      :: CloudStatus,
  _cDescription :: Text
} deriving (Eq, Show)

data BuildDescription = BuildDescription {
  _buId          :: Millisecond,
  _buType        :: BuildListType,
  _buStatus      :: BuildStatus,
  _buDescription :: Text
} deriving (Eq, Show)

controllerButtonTypes :: [ControllerListType]
controllerButtonTypes = enumFrom (toEnum 0)

controllerButtonStatuses :: [ControllerButtonStatus]
controllerButtonStatuses = enumFrom (toEnum 0)

buildTypes :: [BuildListType]
buildTypes = enumFrom (toEnum 0)

buildStatuses :: [BuildStatus]
buildStatuses = enumFrom (toEnum 0)

cloudTypes :: [CloudListType]
cloudTypes = enumFrom (toEnum 0)

cloudStatuses :: [CloudStatus]
cloudStatuses = enumFrom (toEnum 0)

makeLenses     'ControllerButton
makeLenses     'CloudDescription
makeLenses     'BuildDescription
makeLenses     'ProjectSession 
makeLenses     'ProjectSessionFile
makeLenses     'ProjectSessionPrefs 
makeLenses     'Realm
makeLensesWith  abbreviatedFields 'Book
makeLensesWith  abbreviatedFields 'BookResp
makeLensesWith  abbreviatedFields 'BooksModel
makeLensesFor  [("enabled", "_enabled")] ''AppColumn
--makeLensesFor [("columns", "_columns"), ("theme", "_theme"), ("rowToScrollTo", "_rowToScrollTo")] ''AppModel

{-
  --Monomer Style: Alerts, Warnings, Confirmations
  -- Descriptor -> Event
  confirmationDialog :: (Typeable s, Typeable e) => e -> WidgetNode () e -> WidgetNode s e
  confirmationDialog d e = alert d e

  draggableDialog :: (Eq a, Typeable a, Typeable s, Typeable e) => a -> e -> WidgetNode () e -> WidgetNode s e
  draggableDialog msg d e = draggable msg (alert d e)

  customDialog :: (Typeable s, Typeable e) => e  -> [AlertCfg]  -> WidgetNode () e -> WidgetNode s e
  customDialog e c d = alert_ e c d

  --OS specific File Dialogs
  osSaveFileDialog :: Text -> Text	-> [Text]	-> Text	-> IO (Maybe Text)	
  osSaveFileDialog = saveFileDialog

  osOpenFileDialog :: Text -> Text	-> [Text]	-> Text	-> Bool	-> IO (Maybe [Text])	
  osOpenFileDialog = openFileDialog

  osSelectFolderDialog :: Text	-> Text	-> IO (Maybe Text)	
  osSelectFolderDialog = selectFolderDialog

  {- 
    evt:ManimerEvt configs:AlertCfg dialogBody:Text
    --
    -- data AlertCfg = AlertCfg {
    --   _alcTitle :: Maybe Text,
    --   _alcClose :: Maybe Text
    -- } 
  -}
-}
