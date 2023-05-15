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

module Main where

import Control.Exception
import Control.Lens
import Data.Default
import Data.List
import Data.Either.Extra
import Data.Maybe
import Data.Text (Text)
import TextShow

import qualified Data.Text as T
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as Sess

import BookTypes
import Monomer

import qualified Monomer.Lens as L
import Graphics.UI.TinyFileDialogs

type BooksWenv = WidgetEnv BooksModel BooksEvt
type BooksNode = WidgetNode BooksModel BooksEvt


type TodoWenv = WidgetEnv TodoModel TodoEvt
type TodoNode = WidgetNode TodoModel TodoEvt

-- todoRowKey :: Todo -> Text
-- todoRowKey todo = "todoRow" <> showt (todo ^. todoId)

-- todoRow :: TodoWenv -> TodoModel -> Int -> Todo -> TodoNode
-- todoRow wenv model idx t = animRow `nodeKey` todoKey where
--   sectionBg = wenv ^. L.theme . L.sectionColor
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

--   countLabel = label caption `styleBasic` styles where
--     caption = "Tasks (" <> showt (length $ model ^. todos) <> ")"
--     styles = [textFont "Regular", textSize 16, padding 20, bgColor sectionBg]

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

bookImage :: Maybe Int -> Text -> WidgetNode s BooksEvt
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

bookDetail :: Book -> WidgetNode s BooksEvt
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

buildUI
  :: WidgetEnv BooksModel BooksEvt
  -> BooksModel
  -> WidgetNode BooksModel BooksEvt
buildUI wenv model = widgetTree where
  -- sectionBgColor = wenv ^. L.theme . L.sectionColor

  -- errorOverlay = alertMsg msg BooksCloseError where
  --   msg = fromMaybe "" (model ^. errorMsg)

  -- bookOverlay = alert BooksCloseDetails content where
  --   content = maybe spacer bookDetail (model ^. selected)

  -- searchOverlay = box content `styleBasic` [bgColor (darkGray & L.a .~ 0.8)] where
  --   content = label "Searching" `styleBasic` [textSize 20, textColor black]

  -- searchForm = keystroke [("Enter", BooksSearch)] $ vstack [
  --     hstack [
  --       label "Query:",
  --       spacer,
  --       textField query `nodeKey` "query",
  --       spacer,
  --       mainButton "Search" BooksSearch
  --     ] `styleBasic` [bgColor sectionBgColor, padding 25]
  --   ]

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

  -- dialogBoxMenu field content = case model ^.action of
  --   TodoCreate  -> [popup field content]
  --   _           -> []

  -- confirmDeleteLayer = case model ^. action of
  --   TodoConfirmingDelete idx todo -> [popup] where
  --     popup = confirmMsg msg (TodoConfirmDelete idx todo) TodoCancelDelete
  --     msg   = "Are you sure you want to delete '" <> (todo ^. description) <> "' ?"
  --   _                             -> []

  separator = separatorLine

  widgetTree = vstack [
      spacer,
      separator,
      spacer,
      hstack [
        spacer,
        button "📖"  None `styleBasic` [ textFont  "UI", textMiddle ],
        spacer,
        button "🆔"  None `styleBasic` [ textFont  "UI", textMiddle ],
        spacer,
        button "🚻"  None `styleBasic` [ textFont  "UI", textMiddle ],
        spacer,
        button "🆑"  None `styleBasic` [ textFont  "UI", textMiddle ],
        spacer,
        button "🌑"  None `styleBasic` [ textFont  "UI", textMiddle ],
        spacer,
        button "🎮"  None `styleBasic` [ textFont  "UI", textMiddle ],
        spacer,
        button "🎥" None `styleBasic` [ textFont  "UI", textMiddle ],
        filler,
        button "#" None `styleBasic` [ textFont  "UI", textMiddle ],
        spacer,
        button "➿" None `styleBasic` [ textFont  "UI", textMiddle ],
        spacer
      ],
      spacer,
      separator,
      filler,
      --mainContent,
      --sideBar
      separator,
      spacer,
      hstack [
        vstack [
          filler
          -- searchForm,
          -- countLabel,
          -- box_ [mergeRequired booksChanged] $
          --   vscroll (vstack (bookRow wenv <$> model ^. books)) `nodeKey` "mainScroll"
        ],
        vstack [
          filler
        ]
      ],
      spacer,
      separator,
      spacer,
      hstack [
        filler,
        label "Run Dedicated Server",
        checkbox toolTips,
        spacer,
        label "Debug On Play",
        checkbox toolTips,
        spacer,
        label "Tooltips",
        checkbox toolTips,
        spacer
      ],
      spacer
      -- errorOverlay `nodeVisible` isJust (model ^. errorMsg),
      -- bookOverlay `nodeVisible` isJust (model ^. selected),
      -- searchOverlay `nodeVisible` model ^. searching
    ] -- <> confirmDeleteLayer

handleEvent
  :: Sess.Session
  -> WidgetEnv BooksModel BooksEvt
  -> WidgetNode BooksModel BooksEvt
  -> BooksModel
  -> BooksEvt
  -> [EventResponse BooksModel BooksEvt BooksModel BooksEvt]
handleEvent sess wenv node model evt = case evt of
  BooksInit -> [SetFocusOnKey "query"]
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
      resp <- Sess.get sess url
        >>= W.asJSON
        >>= return . preview (W.responseBody . _Just)

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
    initModel = BooksModel "" False Nothing [] Nothing False

customLightTheme :: Theme
customLightTheme = lightTheme
  & L.userColorMap . at "rowBgColor" ?~ rgbHex "#ECECEC"

customDarkTheme :: Theme
customDarkTheme = darkTheme
  & L.userColorMap . at "rowBgColor" ?~ rgbHex "#656565"

-- Utility function to avoid the "Ambiguous type variable..." error
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch
