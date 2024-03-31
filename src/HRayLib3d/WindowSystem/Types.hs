{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# OPTIONS_GHC -Woverlapping-patterns #-}

module HRayLib3d.WindowSystem.Types where

import GHC.Generics     ( Generic )
import Control.Lens.TH  ( abbreviatedFields, makeLenses, makeLensesWith )
import Data.Aeson       ( FromJSON(parseJSON), (.!=), (.:), (.:?), withObject )
import Data.Default     ( Default(..) )
import Data.Text        ( Text   )
import Data.Binary      ( Binary )
import Monomer          ( Millisecond )
import System.FilePath  ( takeExtension )
import System.Directory ( doesDirectoryExist, listDirectory )

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
    _projectName           :: Text,
    _projectFiles          :: [ProjectSessionFile],
    _projectRealms         :: [Text],
    _projectPreferences    :: ProjectSessionPrefs
  } deriving(Generic, Eq, Show)

instance Binary ProjectSession
instance Binary ProjectSessionFile
instance Binary ProjectSessionPrefs
instance Binary ProjectSessionFileType

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
  | MetalLibFile
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
  ".wrlp"         -> WRLProjectFile
  ".vsproj"       -> VSProjectFile
  ".xcproj"       -> XCodeProjectFile
  ".cmakelists"   -> CMakeProjectFile
  ".makefile"     -> MakeProjectFile
  ".webgl"        -> WebGLProjectFile
  ".wasm"         -> WASMProjectFile
  ".djinni"       -> DjinniProjectFile
  ".ipa"          -> IOSProjectFile
  ".gradle"       -> AndroidProjectFile

  --System Bundle Types
  ".assetBundle"  -> AssetBundleFile
  ".shaderCache"  -> ShaderCacheFile
  -- ".obj"       -> WavefrontOBJFile -- re-add
  ".glb"          -> GLBAssetFile
  ".gltf"         -> GLTFAssetFile
  ".pk3"          -> PK3AssetFile
  ".md3"          -> MD3AssetFile
  ".bsp"          -> BSPAssetFile
  ".db"           -> RealmFile
  "GemFile"       -> GemFile

  -- Shader File Types
  ".q3s"          -> Q3Shader
  ".glsl"         -> GLSLShaderFile
  ".metal"        -> MetalShaderFile
  ".metallib"     -> MetalLibFile

  -- Component File Types
  ".hs"           -> HaskellFile
  ".cpp"          -> CPPFile
  ".h"            -> CPPFileHeader
  ".hpp"          -> CPPFileHeader

  -- Script File Types
  ".rb"           -> RubyScriptFile
  ".py"           -> PythonScriptFile
  ".lua"          -> LuaScriptFile
  ".js"           -> JavaScriptFile
  ".jsx"          -> JavaScriptFile -- replace with JSX Icon/JSXFile
  ".wasm"         -> WASMFile
  -- GenericScript     -> GenericScript
  -- GenericScriptAlt2 -> "./assets/images/script_alt.png"
  -- GenericScriptAlt3 -> "./assets/images/script_alt2.png"
  ".txt"           -> TextFile

  -- Data Structure Type
  ".html"           -> HTMLFile
  ".lc"             -> LCFile
  ".xml"            -> XMLFile
  ".json"           -> JSONFile
  ".mat"            -> MaterialFile
  ".zip"            -> ZIPFile

  -- Audio File Types
  ".ogg"            -> OGGFile
  ".wav"            -> WAVFile

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
  Unknown            -> "./assets/images/unknown.png"
  FolderFile         -> "./assets/images/folder.png"
  WRLProjectFile     -> "./assets/images/wrlp.png"
  VSProjectFile      -> "./assets/images/wrlp.png"
  XCodeProjectFile   -> "./assets/images/wrlp.png"
  CMakeProjectFile   -> "./assets/images/cmake.png"
  MakeProjectFile    -> "./assets/images/gnu_make.png"
  WebGLProjectFile   -> "./assets/images/webgl.png"
  WASMProjectFile    -> "./assets/images/wasm.png"
  DjinniProjectFile  -> "./assets/images/djinni.png"
  IOSProjectFile     -> "./assets/images/ios.png"
  AndroidProjectFile -> "./assets/images/android.png"

  --System Bundle Types
  AssetBundleFile    -> "./assets/images/assetBundle.png"
  ShaderCacheFile    -> "./assets/images/shaderCache.png"
  GLBAssetFile       -> "./assets/images/glb.png"
  GLTFAssetFile      -> "./assets/images/gltf.png"
  PK3AssetFile       -> "./assets/images/pk3.png"
  MD3AssetFile       -> "./assets/images/md3.png"
  BSPAssetFile       -> "./assets/images/bsp.png"
  DBFile             -> "./assets/images/database.png"
  RealmFile          -> "./assets/images/realmLevel.png"
  GemFile            -> "./assets/images/ruby_gem.png"

  -- Shader File Types
  Q3Shader         -> "./assets/images/q3shader.png"
  GLSLShaderFile   -> "./assets/images/glsl.png"
  MetalShaderFile  -> "./assets/images/metal.png"
  MetalLibFile     -> "./assets/images/metallib.png"

  -- Component File Types
  HaskellFile      -> "./assets/images/haskell.png"
  CPPFile          -> "./assets/images/c++.png"
  CPPFileHeader    -> "./assets/images/hpp.png" -- replace with a h/hpp icon

  -- Script File Types
  RubyScriptFile    -> "./assets/images/ruby.png"
  PythonScriptFile  -> "./assets/images/python.png"
  LuaScriptFile     -> "./assets/images/lua.png"
  JavaScriptFile    -> "./assets/images/js.png"
  WASMFile          -> "./assets/images/wasm.png"
  GenericScript     -> "./assets/images/c++.png"
  GenericScriptAlt2 -> "./assets/images/script_alt.png"
  GenericScriptAlt3 -> "./assets/images/script_alt2.png"
  TextFile          -> "./assets/images/text.png"

  -- Data Structure Type
  HTMLFile          -> "./assets/images/html.png"
  LCFile            -> "./assets/images/lc_file.png"
  XMLFile           -> "./assets/images/xml-file.png"
  JSONFile          -> "./assets/images/json.png"
  MaterialFile      -> "./assets/images/material.png"
  ZIPFile           -> "./assets/images/zip.png"

  -- Audio File Types
  OGGFile           -> "./assets/images/ogg.png"
  WAVFile           -> "./assets/images/wav.png"

  -- Misc
  CloudbaseFile     -> "./assets/images/cloudbase.png"
  LocalizationFile  -> "./assets/images/localization.png"
  MapBoxFile        -> "./assets/images/realm.png"
  PDFFile           -> "./assets/images/pdf.png"
  UWPFile           -> "./assets/images/uwp.png"
  OTFFile           -> "./assets/images/otf.png"
  TTFFile           -> "./assets/images/ttf.png"
  TFFile            -> "./assets/images/tf.png"
  ARFile            -> "./assets/images/ar_file.png"
  XRFile            -> "./assets/images/xr_object.png"
  ImageFile         -> "./assets/images/wav.png"
  
  --_       -> error "Bad Type included in Project"

  
data BooksModel = BooksModel {
  _bkmQuery     :: Text,
  _bmkSearching :: Bool,
  _bkmErrorMsg  :: Maybe Text,
  _bkmBooks     :: [Book],
  _bmkSelected  :: Maybe Book,
  _bmkToolTips  :: Bool,
  _bmkMultiplayerEnabled :: Bool,
  _bmkOptimizeShaders :: Bool,
  _bmkOptimizeForWeb :: Bool,
  _bmkRendererEnabled  :: Bool,
  _bmkDebugOnPlay  :: Bool,
  _bmkDedicatedServer  :: Bool,
  _bmkFileMenu  :: Bool,
  _bmkApplicationPreferences :: Bool,
  _bmkRealmList              :: Bool,
  _bmkControllerPreferences  :: Bool,
  _bmkExportPreferences      :: Bool,
  _bmkRunPreferences         :: Bool,
  _bmkMPPreferences          :: Bool,
  _bmkProjectSession         :: Maybe ProjectSession
} deriving (Eq, Show)

instance Default ProjectSession where
  def = ProjectSession "default" [] [] (ProjectSessionPrefs [] False)

data TodoModel = TodoModel {
  _todos      :: [Todo],
  _activeTodo :: Todo,
  _action     :: TodoAction
} deriving (Eq, Show)

data TodoAction
  = TodoNone
  | TodoCreate
  | TodoAdding
  | TodoEditing Int
  | TodoConfirmingDelete Int Todo
  deriving (Eq, Show)

data BooksEvt
  = BooksInit
  | BooksSearch
  | BooksSearchResult BookResp
  | BooksSearchError Text
  | BooksShowDetails Book
  | BooksCloseDetails
  | BooksCloseError
  | OpenFS
  | SaveFS
  | OpenAppPrefs
  | OpenRealmList
  | OpenControllerPrefs
  | OpenExportPrefs
  | OpenRunPrefs
  | OpenMPPrefs
  | RefreshProjectFilesTask
  | RefreshProjectFiles [FilePath]
  | NoneB Bool
  | None
  deriving (Eq, Show)

data TodoEvt
  = TodoInit
  | TodoNew
  | TodoAdd
  | TodoEdit Int Todo
  | TodoSave Int
  | TodoConfirmDelete Int Todo
  | TodoCancelDelete
  | TodoDeleteBegin Int Todo
  | TodoDelete Int Todo
  | TodoShowEdit
  | TodoHideEdit
  | TodoHideEditDone
  | TodoCancel
  deriving (Eq, Show)

data TodoType
  = Home
  | Work
  | Sports
  deriving (Eq, Show, Enum)

data TodoStatus
  = Pending
  | Done
  deriving (Eq, Show, Enum)

data Todo = Todo {
  _todoId   :: Millisecond,
  _todoType :: TodoType,
  _status   :: TodoStatus,
  _description :: Text
} deriving (Eq, Show)

instance Default Todo where
  def = Todo {
    _todoId = 0,
    _todoType = Home,
    _status = Pending,
    _description = ""
  }

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
    
mLoadProjectFolderContents :: FilePath -> IO(Maybe [FilePath])
mLoadProjectFolderContents path = do
    dirExists <- doesDirectoryExist path
    if dirExists 
        then Just <$> listDirectory path
        else return Nothing
    
slice :: Int -> Int -> [a] -> [a]
slice start stop xs = fst $ splitAt (stop - start) (snd $ splitAt start xs)

sliceM :: Monad m  => Int -> Int -> [a] -> m [a]
sliceM start stop xs  = return $ fst $ splitAt (stop - start) (snd $ splitAt start xs)

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


makeLenses 'Todo
makeLenses 'TodoModel
makeLenses 'ProjectSessionPrefs 
makeLenses 'ProjectSessionFile
makeLenses 'ProjectSession 
makeLensesWith abbreviatedFields 'Book
makeLensesWith abbreviatedFields 'BookResp
makeLensesWith abbreviatedFields 'BooksModel
