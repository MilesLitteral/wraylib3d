{-|
Module      : Main
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Main module for the 'Books' example.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- Notes on making a Widget for direct backend use
-- Very important for the RendererWidget that is coming up
-- https://github.com/fjvallarino/monomer/issues/24
-- https://github.com/fjvallarino/monomer/issues/261
-- https://github.com/fjvallarino/monomer/issues/10
-- https://github.com/fjvallarino/monomer/issues/52 (Bonus)
module Main where

import Control.Exception
import Control.Lens
import Data.Default
import Data.List
import HRayLib3d.Utils.Unsafe

import Data.Either.Extra
import Data.Maybe
import Data.Binary
import Data.Text (Text, pack)
import TextShow

import System.FilePath  ( takeBaseName )
import System.Directory ( createDirectoryIfMissing, listDirectory )
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as Sess

import BookTypes
import Monomer

import qualified Monomer.Lens as L
import Graphics.UI.TinyFileDialogs
import Monomer (CmbFitWidth(fitWidth))
import Data.Data (Typeable)

type BooksWenv = WidgetEnv BooksModel BooksEvt
type BooksNode = WidgetNode BooksModel BooksEvt

type TodoWenv = WidgetEnv TodoModel TodoEvt
type TodoNode = WidgetNode TodoModel TodoEvt

-- todoRowKey :: Todo -> Text
-- todoRowKey todo = "todoRow" <> showt (todo ^. todoId)

-- todoRow :: TodoWenv -> TodoModel -> Int -> Todo -> TodoNode
-- todoRow wenv model idx t = animRow `nodeKey` todoKey where
--   rowButtonColor = wenv ^. L.theme . L.userColorMap . at "rowButton" . non def
--   rowSepColor = gray & L.a .~ 0.5

--   todoKey = todoRowKey t
--   todoDone = t ^. status == Done
--   isLast = idx == length (model ^. todos) - 1

--   (todoBg, todoFg)
--     | todoDone = (doneBg, doneFg)
--     | otherwise = (pendingBg, pendingFg)

--   todoStatus = labelS (t ^. status)
--     `styleBasic` [textFont "Medium", textSize 12, textAscender, textColor todoFg, padding 6, paddingH 8, radius 12, bgColor todoBg]

--   rowButton caption action = button caption action
--     `styleBasic` [textFont "Remix", textMiddle, textColor rowButtonColor, bgColor transparent, border 0 transparent]
--     `styleHover` [bgColor sectionBg]
--     `styleFocus` [bgColor (sectionBg & L.a .~ 0.5)]
--     `styleFocusHover` [bgColor sectionBg]

--   todoInfo = hstack [
--       vstack [
--         labelS (t ^. todoType) `styleBasic` [textSize 12, textColor darkGray],
--         spacer_ [width 5],
--         label (t ^. description) `styleBasic` [textThroughline_ todoDone]
--       ],
--       filler,
--       box_ [alignRight] todoStatus `styleBasic` [width 80],
--       spacer,
--       rowButton remixEdit2Line (TodoEdit idx t),
--       spacer,
--       rowButton remixDeleteBinLine (TodoDeleteBegin idx t)
--     ] `styleBasic` [ paddingV 15, styleIf (not isLast) $ borderB 1 rowSepColor ]

--   animRow = animFadeOut_ [onFinished (TodoDelete idx t)] todoInfo

-- todoEdit :: TodoWenv -> TodoModel -> TodoNode
-- todoEdit wenv model = editNode where
--   sectionBg = wenv ^. L.theme . L.sectionColor
--   isValidInput = model ^. activeTodo . description /= ""

--   (saveAction, saveLabel) = case model ^. action of
--     TodoEditing idx -> (TodoSave idx, "Save")
--     _ -> (TodoAdd, "Add")

--   saveTodoBtn = mainButton saveLabel saveAction

--   editFields = keystroke [("Enter", saveAction) | isValidInput] $ vstack [
--       hstack [
--         label "Task:",
--         spacer,
--         textField (activeTodo . description) `nodeKey` "todoDesc"
--       ],
--       spacer,
--       hgrid [
--         hstack [
--           label "Type:",
--           spacer,
--           textDropdownS (activeTodo . todoType) todoTypes `nodeKey` "todoType",
--           spacer -- Added here to avoid grid expanding it to 1/3 total width
--         ],
--         hstack [
--           label "Status:",
--           spacer,
--           textDropdownS (activeTodo . status) todoStatuses
--         ]
--       ]
--     ]

--   editNode = keystroke [("Esc", TodoCancel)] $ vstack [
--       editFields,
--       spacer,
--       hstack [
--         filler,
--         saveTodoBtn `nodeEnabled` isValidInput,
--         spacer,
--         button "Cancel" TodoCancel
--         ]
--     ] `styleBasic` [bgColor sectionBg, padding 20]

-- buildUI :: TodoWenv -> TodoModel -> TodoNode
-- buildUI wenv model = widgetTree where
--   sectionBg = wenv ^. L.theme . L.sectionColor
--   isEditing
--     | TodoEditing _ <- model ^. action = True
--     | otherwise = False

--   todoList = vstack (zipWith (todoRow wenv model) [0..] (model ^. todos))

--   newButton = mainButton "New" TodoNew `nodeKey` "todoNew"
--     `nodeVisible` not isEditing

--   editLayer = content where
--     dualSlide content = outer where
--       inner = animSlideIn_ [slideTop, duration 200] content
--         `nodeKey` "animEditIn"
--       outer = animSlideOut_ [slideTop, duration 200, onFinished TodoHideEditDone] inner
--         `nodeKey` "animEditOut"

--     content = vstack [
--         dualSlide (todoEdit wenv model),
--         filler
--       ] `styleBasic` [bgColor (grayDark & L.a .~ 0.5)]

--   confirmDeleteLayer = case model ^.action of
--     TodoConfirmingDelete idx todo -> [popup] where
--       popup = confirmMsg msg (TodoConfirmDelete idx todo) TodoCancelDelete
--       msg = "Are you sure you want to delete '" <> (todo ^. description) <> "' ?"
--     _ -> []

--   mainLayer = vstack [
--       countLabel,
--       scroll_ [] (todoList `styleBasic` [padding 20, paddingT 5]),
--       filler,
--       box_ [alignRight] newButton
--         `styleBasic` [bgColor sectionBg, padding 20]
--     ]

--   widgetTree = zstack ([
--       mainLayer,
--       editLayer `nodeVisible` isEditing
--     ] <> confirmDeleteLayer)

bookImage :: Maybe Int -> Text -> WidgetNode BooksModel BooksEvt
bookImage imgId size = maybe filler coverImg imgId where
  baseUrl = "http://covers.openlibrary.org/b/id/<id>-<size>.jpg"
  imgUrl i = T.replace "<size>" size $ T.replace "<id>" (showt i) baseUrl
  coverImg i = image_ (imgUrl i) [fitHeight, alignRight]

bookRow :: BooksWenv -> Book -> BooksNode
bookRow wenv b = row where
  rowBgColor = wenv ^. L.theme . L.userColorMap . at "rowBgColor" . non def
  publishYear = maybe "" showt (b ^. year)

  rowContent b = hstack [
      vstack [
        label_ (b ^. title) [resizeFactor 1]
          `styleBasic` [textFont "Medium", textSize 16],
        spacer,
        label_ (T.intercalate ", " (b ^. authors)) [resizeFactor 1]
          `styleBasic` [textSize 14]
      ],
      filler,
      vstack [
        label publishYear `styleBasic` [width 50, textSize 14],
        spacer
      ],
      bookImage (b ^. cover) "S" `styleBasic` [width 35]
    ]

  row = box_ cfg content `styleBasic` [padding 10, paddingT 0] where
    cfg = [expandContent, onClick (BooksShowDetails b)]
    content = rowContent b
      `styleBasic` [height 80, padding 20, radius 5]
      `styleHover` [bgColor rowBgColor, cursorIcon CursorHand]

bookDetail :: Book -> WidgetNode BooksModel BooksEvt
bookDetail b = content `styleBasic` [minWidth 500, paddingH 20] where
  hasCover = isJust (b ^. cover)
  publishYear = maybe "" showt (b ^. year)

  shortLabel value = label value `styleBasic` [textFont "Medium", textTop]
  longLabel value = label_ value [multiline, ellipsis, trimSpaces]

  content = hstack [
      vstack_ [childSpacing] [
        longLabel (b ^. title)
          `styleBasic` [textSize 20, textFont "Medium"],
        longLabel (T.intercalate ", " (b ^. authors))
          `styleBasic` [textSize 16],
        label publishYear
          `styleBasic` [textSize 14]
      ],
      widgetIf hasCover $ hstack [
        filler,
        bookImage (b ^. cover) "M"
          `styleBasic` [width 200]
      ]
    ]

readProjectFileIndex :: ProjectSession -> [WidgetNode BooksModel BooksEvt] -- will be changed to a Scene Inspector 
readProjectFileIndex   projSess = map (\x -> hstack [(image_  . matchPFType $ _projectFileType x) [fitWidth] `styleBasic` [border 1 black, width 35, height 35], label $ pack $ _projectFileName x]) $ _projectFiles projSess

generateProjectFileGUI :: ProjectSession -> [WidgetNode BooksModel BooksEvt]
generateProjectFileGUI projSess = map (\x -> hstack [(image_  . matchPFType $ _projectFileType x) [fitWidth] `styleBasic` [border 1 black, width 45, height 45], label $ pack $ _projectFileName x]) $ _projectFiles projSess

buildUI
  :: WidgetEnv BooksModel BooksEvt
  -> BooksModel
  -> WidgetNode BooksModel BooksEvt
buildUI wenv model = widgetTree where
  sectionBg  = wenv ^. L.theme . L.sectionColor

  countLabel = label caption `styleBasic` styles where
    caption  = "Files (" <> showt (length $ model ^. books) <> ")"
    styles   = [textFont "Regular", textSize 16, padding 20, bgColor sectionBg]

  searchOverlay = box content `styleBasic` [bgColor (darkGray & L.a .~ 0.8)] where
    content = label "Searching" `styleBasic` [textSize 20, textColor black]

  errorOverlay = alert BooksCloseError content where
    content    = fromMaybe spacer (Just $ label err :: Maybe (WidgetNode BooksModel BooksEvt))
    err        = fromJust $ model ^. errorMsg

  -- for file selection
  bookOverlay = alert BooksCloseDetails content where
    content   = fromMaybe spacer (Just $ bookDetail book)
    book      = fromJust $ model ^. selected

  searchForm = keystroke [("Enter", BooksSearch)] $ vstack [
      hstack [
        label "Query:",
        spacer,
        textField query `nodeKey` "query",
        spacer,
        mainButton "Search" BooksSearch
      ] `styleBasic` [bgColor (rgbHex "A6A6A6"), padding 25]
    ]

  -- countLabel = label caption `styleBasic` [padding 10] where
  --   caption = "Books (" <> showt (length $ model ^. books) <> ")"

  -- booksChanged wenv old new = old ^. books /= new ^. books

  {-
  This could even be a button, since they support focus and can be navigated with
  the keyboard. The 'ignoreTheme' config could be useful to reset styling (remove
  borders, etc).
  -}
  -- sectionSelector sectionIdx = selector where
  --   item = label ("Section " <> showt sectionIdx)
  --     `styleHover` [textColor lightSkyBlue]

  --   selector = box_ [alignLeft, onClick (ShowSection sectionIdx)] item
  --     `styleBasic` [cursorHand]

  -- sectionBody sectionIdx = item where
  --   cfgs = [alignCenter, alignMiddle]
  --   item = box_ cfgs (label $ "This is section " <> showt sectionIdx)
  --     `styleBasic` [minWidth 100, minHeight 100]
  --     `nodeVisible` (model ^. activeSection == sectionIdx)

  -- sideBarToggle = toggleButton "Toggle side bar" sideBarVisible

  -- sideBar = item where
  --   children = vstack_ [childSpacing] $
  --     sectionSelector <$> sectionIds

  --   layout = vstack [
  --       sideBarToggle,
  --       spacer,
  --       children
  --     ]
  --     `styleBasic` [width 200, minHeight 100, padding 10, bgColor sideBarBg]

  --   item = box_ [alignLeft, ignoreEmptyArea] layout

  -- menuBar = hstack [
  --     hstack_ [childSpacing] (sectionSelector <$> sectionIds),
  --     spacer,
  --     sideBarToggle
  --   ]

  -- mainContent = vstack_ [childSpacing] [
  --     menuBar,
  --     vstack (sectionBody <$> sectionIds)
  --   ]

  --   where
  -- config = mconcat configs
  -- dialogBody wenv = label_ message [multiline]
  --   & L.info . L.style .~ collectTheme wenv L.dialogMsgBodyStyle
  -- createUI = buildUI dialogBody acceptEvt cancelEvt config
  -- compCfg = [compositeMergeReqs mergeReqs]
  -- newNode = compositeD_ "confirm" (WidgetValue ()) createUI handleEvent compCfg
  --popup field content

  -- confirmDeleteLayer = case model ^. action of
  --   TodoConfirmingDelete idx todo -> [popup] where
  --     popup = confirmMsg msg (TodoConfirmDelete idx todo) TodoCancelDelete
  --     msg   = "Are you sure you want to delete '" <> (todo ^. description) <> "' ?"
  --   _                             -> []

  dialogBoxMenu field content              = box_ [alignCenter, onClickEmpty OpenFS] (boxShadow $ popup field content)
  fullscreenDialogMenu field content       = popup field content `styleBasic` [bgColor (rgbHex "#000000")]
  separator = separatorLine

  -- create alternative trees for the other menus and add transitions to all of them 
  -- animFadeIn (widget) `nodeKey` "fadeTimeLabel" -- (see handleEvent)
  widgetTree = zstack [
    vstack [
      spacer,
      separator,
      spacer,
      hstack [
        spacer,
        button "📖"  OpenFS              `styleBasic`  [ textFont  "UI", textMiddle ],
        spacer,
        button "🆔"  OpenAppPrefs        `styleBasic`  [ textFont  "UI", textMiddle ],
        spacer,
        button "🌑"  OpenRealmList       `styleBasic`  [ textFont  "UI", textMiddle ],
        spacer,
        button "🎮"  OpenControllerPrefs `styleBasic` [ textFont  "UI", textMiddle ],
        spacer,
        button "🆑"  OpenExportPrefs     `styleBasic` [ textFont  "UI", textMiddle ],
        spacer,
        button "🎥"  OpenRunPrefs        `styleBasic` [ textFont  "UI", textMiddle ],
        spacer,
        button "⛳"  OpenMPPrefs         `styleBasic` [ textFont  "UI", textMiddle ], -- Multiplayer
        filler,
        button "#"    None               `styleBasic` [ textFont  "UI", textMiddle ],
        spacer,
        button "➿"  None                `styleBasic` [ textFont  "UI", textMiddle ],
        spacer
      ],
      spacer,
      separator,
      vstack [
        label "📷 0" `styleBasic` [textFont  "UI", textSize 10, textMiddle, textRight],
        filler,
        filler
      ] `styleBasic` [bgColor (rgbHex "#00AF54"), sizeReqW $ fixedSize 256, sizeReqH $ fixedSize 256], -- this is mainContent
      separator,
      hstack [
        label "Inspector",
        spacer,
        searchForm,
        countLabel
        --        box_ [mergeRequired booksChanged] $ vscroll (vstack (bookRow wenv <$> model ^. books)) `nodeKey` "mainScroll"
      ],
      separator,
      hstack [
        --scroll $ vstack (label "File System" : readProjectFileIndex (fromJust $ model ^. projectSession)) `styleBasic` [bgColor (rgbHex "#A6A6A6")],
        --separator,
        scroll $ vstack (label "File GUI"    : generateProjectFileGUI (fromJust $ model ^. projectSession))  `styleBasic` [bgColor (rgbHex "#612B8A")]
      ],
      separator,
      spacer,
      hstack [
        spacer,
        button "💥" RefreshProjectFilesTask `styleBasic` [ textFont  "UI", textMiddle, textLeft ],
        filler, 
        label "Run Dedicated Server",
        checkbox dedicatedServer,
        spacer,
        label "Debug On Play",
        checkbox debugOnPlay,
        spacer,
        label "📷" `styleBasic` [ textFont  "UI", textMiddle ],
        checkbox rendererEnabled,
        spacer,
        label "Tooltips",
        checkbox toolTips,
        spacer
      ],
      spacer,
      searchOverlay `nodeVisible` model ^. searching
    ],
    --FileSystem Menu
    menuDialog_ (vstack $ intersperse spacer [
      label "Files",
      spacer,
      button "Save"   (ConfirmParentEvt OpenFS),
      button "Load"   (ConfirmParentEvt OpenFS),
      button "Import" (ConfirmParentEvt OpenFS),
      button "Export" (ConfirmParentEvt OpenFS)
      ]) OpenFS OpenFS `nodeVisible` (model ^. fileMenu),

    -- Application Preferences
    menuDialog_ (vstack $ intersperse spacer [
      label "Application Preferences",
      spacer,
      button "Project"   (ConfirmParentEvt OpenAppPrefs),
      button "Assets"    (ConfirmParentEvt OpenAppPrefs),
      button "Renderer"  (ConfirmParentEvt OpenAppPrefs)
      ]) OpenAppPrefs OpenAppPrefs `nodeVisible` (model ^. applicationPreferences),

    -- Realm List (Number of Stages/Scenes included in Game)
    menuDialog_ (vstack $ intersperse spacer [
        label "Realms",
        spacer,
        scroll $ vstack [
          button "default"  (ConfirmParentEvt OpenRealmList) -- Replace for a ALens' BookEvt [Realm], also add data Realm
        ],
        hstack [
          button "Add"      (ConfirmParentEvt OpenRealmList),
          spacer,
          button "Remove"   (ConfirmParentEvt OpenRealmList)
        ]
      ]) OpenRealmList OpenRealmList `nodeVisible` (model ^. realmList),

    -- Controller Preferences
    menuDialog_ (vstack $ intersperse spacer [
      label "Controller Preferences",
      spacer,
      -- Elaborate Widget for Controllers (Xbox vs Playstation vs 3rd Party) goes here
      button "Controller Setup"   (ConfirmParentEvt OpenControllerPrefs),
      button "Load Button Assignment"   (ConfirmParentEvt OpenControllerPrefs),
      button "Export Profile" (ConfirmParentEvt OpenControllerPrefs),
      button "Export Button Assignment" (ConfirmParentEvt OpenControllerPrefs)
      ]) OpenControllerPrefs OpenControllerPrefs `nodeVisible` (model ^. controllerPreferences),
      -- box_ [alignTop, onClick OpenFS] closeIcon

    -- Export Preferences
    menuDialog_ (vstack $ intersperse spacer [
      label "Export Preferences",
      spacer,
      button "Select Platform"     (ConfirmParentEvt OpenExportPrefs),
      button "Set External Deps"   (ConfirmParentEvt OpenExportPrefs),
      hstack [
        label "Multiplayer",
        checkbox multiplayerEnabled,
        spacer,
        label "Optimize Shaders for Platform",
        checkbox optimizeShaders,
        spacer,
        label "Optimize For Web (WASM, WebGL)",
        checkbox optimizeForWeb,
        spacer
      ],
      button "Revert to Defaults" (ConfirmParentEvt OpenExportPrefs)
      ]) OpenExportPrefs OpenExportPrefs `nodeVisible` (model ^. exportPreferences),

    -- Run Preferences
    menuDialog_ (vstack $ intersperse spacer [
      label "Run Preferences",
      spacer,
      button "Run Current Scene"   (ConfirmParentEvt OpenRunPrefs),
      button "Run Named Scene"   (ConfirmParentEvt OpenRunPrefs),
      button "Disable ScriptEngine Execution" (ConfirmParentEvt OpenRunPrefs),
      button "Legacy Render Pipeline Only (OpenGL 3.2)" (ConfirmParentEvt OpenRunPrefs)
      ]) OpenRunPrefs OpenRunPrefs `nodeVisible` (model ^. runPreferences),

    -- Multiplayer Preferences
    menuDialog_ (vstack $ intersperse spacer [
      label "Multiplayer Preferences",
      spacer,
      button "Push Realm to Cloud"     (ConfirmParentEvt OpenMPPrefs),
      button "Make Realm Searchable"   (ConfirmParentEvt OpenMPPrefs),
      button "Private Realm Only (Singleplayer/Co-Op Only)" (ConfirmParentEvt OpenMPPrefs),
      button "Realm Visibility"        (ConfirmParentEvt OpenMPPrefs)
      ]) OpenMPPrefs OpenMPPrefs `nodeVisible` (model ^. mPPreferences)
    --sideBar
    errorOverlay  `nodeVisible` isJust (model ^. errorMsg),
    bookOverlay   `nodeVisible` isJust (model ^. selected)
    ]

buildProjectSession :: BooksModel -> String -> IO ProjectSession
buildProjectSession model projPath =  do
  projDir <- listDirectory  projPath
  let pSession = fromJust $ model ^. projectSession
  return $ ProjectSession (_projectName pSession) (map (\x -> ProjectSessionFile (takeBaseName x) $ matchExtType x) projDir) (_projectRealms pSession) (_projectPreferences pSession)
  
handleEvent
  :: Sess.Session
  -> WidgetEnv BooksModel BooksEvt
  -> WidgetNode BooksModel BooksEvt
  -> BooksModel
  -> BooksEvt
  -> [EventResponse BooksModel BooksEvt BooksModel BooksEvt]
handleEvent sess wenv node model evt = case evt of
  BooksInit -> [SetFocusOnKey "query",  Task $ do
    createDirectoryIfMissing True "./assets/projects/default"
    BL.writeFile "./assets/projects/default/default.wrlp" (encode . fromJust $ model ^. projectSession)
    return None]
  BooksSearch -> [
    Model $ model & searching .~ True,
    Task $ searchBooks sess (model ^. query)
    ]
  BooksSearchResult resp -> [
    Message "mainScroll" ScrollReset,
    Model $ model
      & searching .~ False
      & errorMsg .~ Nothing
      & books .~ resp ^. docs
    ]
  BooksSearchError msg -> [
    Model $ model
      & searching .~ False
      & errorMsg ?~ msg
      & books .~ []
    ]
  OpenFS                -> [Model $ model & fileMenu .~ not (model ^. fileMenu)]
  OpenAppPrefs          -> [Model $ model & applicationPreferences .~ not (model ^. applicationPreferences)]
  OpenRealmList         -> [Model $ model & realmList .~ not (model ^. realmList)]
  OpenControllerPrefs   -> [Model $ model & controllerPreferences .~ not (model ^. controllerPreferences)]
  OpenExportPrefs       -> [Model $ model & exportPreferences .~ not (model ^. exportPreferences)]
  OpenRunPrefs          -> [Model $ model & runPreferences .~ not (model ^. runPreferences)]
  OpenMPPrefs           -> [Model $ model & mPPreferences  .~ not (model ^. mPPreferences )]
  RefreshProjectFiles      projDir -> [Model $ model & projectSession ?~ ProjectSession (_projectName (fromJust $ model ^. projectSession)) (map (\x -> ProjectSessionFile (takeBaseName x) $ matchExtType x) projDir) (_projectRealms (fromJust $ model ^. projectSession)) (_projectPreferences (fromJust $ model ^. projectSession))]
  RefreshProjectFilesTask   -> [Task $ do
    dirCont <- listDirectory  "./assets/projects/default/" -- this will be formally addressed later
    print dirCont
    return $ RefreshProjectFiles dirCont]
  --do  _projectName        :: Text,

    -- _projectFiles       :: [ProjectSessionFile],
    -- _projectRealms      :: [Text],
    -- _projectPreferences :: ProjectSessionPrefs
  --   projDir <- listDirectory  "./assets/projects/default/"
  --   
  None                  -> []
  NoneB   b             -> []
  --LoadFile fp         -> -- [Task $ do
  -- filePath <- selectFolderDialog "Path to RENDER Source" (pack $ inlinePerformIO getHomeDirectory ++ "/Desktop/GitHub/manifest-v3/assets/images/")
  -- case filePath of
  --   Nothing  -> do
  --     notifyPopup "IOException" "Invalid File Path or Cancelled I/O Operation" Graphics.UI.TinyFileDialogs.Warning
  --     return $ DoNothing]
  BooksShowDetails book -> [Model $ model & selected ?~ book]
  BooksCloseDetails     -> [Model $ model & selected .~ Nothing]
  BooksCloseError       -> [Model $ model & errorMsg .~ Nothing]
  -- AppSetTime time       -> fadeInMsg time ++ [Model $ model & currentTime .~ time]
  --   where
  --     fadeInMsg time
  --       | truncate (todSec time) `mod` 10 /= 0 = []
  --       | otherwise = [Message "fadeTimeLabel" AnimationStart]

searchBooks :: Sess.Session -> Text -> IO BooksEvt
searchBooks sess query = do
  putStrLn . T.unpack $ "Searching: " <> query
  result <- catchAny (fetch url) (return . Left . T.pack . show)

  case result of
    Right resp -> return (BooksSearchResult resp)
    Left err -> return (BooksSearchError err)
  where
    url = "https://openlibrary.org/search.json?q=" <> T.unpack query
    checkEmpty resp
      | null (resp ^. docs) = Nothing
      | otherwise = Just resp
    fetch url = do
      resp <- Sess.get sess url >>= W.asJSON >>= return . preview (W.responseBody . _Just)

      return $ maybeToEither "Empty response" (resp >>= checkEmpty)

main :: IO ()
main = do
  sess <- Sess.newAPISession
  startApp initModel (handleEvent sess) buildUI config
  where
    config = [
      appWindowTitle "Book search",
      appWindowIcon "./assets/images/iconL.png",
      appTheme customDarkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Medium"  "./assets/fonts/Roboto-Medium.ttf",
      appFontDef "UI"      "./assets/fonts/og-dcm-emoji.ttf",
      appInitEvent BooksInit
      ]
    initModel = appModel

appModel :: BooksModel
appModel  = BooksModel "" False Nothing [] Nothing False False False False False False False False False False False False False False (Just $ ProjectSession "default" [] [] (ProjectSessionPrefs [] False))

customLightTheme :: Theme
customLightTheme = lightTheme
  & L.userColorMap . at "rowBgColor" ?~ rgbHex "#ECECEC"

customDarkTheme :: Theme
customDarkTheme = darkTheme
  & L.userColorMap . at "rowBgColor" ?~ rgbHex "#656565"

-- Utility function to avoid the "Ambiguous type variable..." error
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch
