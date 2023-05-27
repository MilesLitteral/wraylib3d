{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Monomer
import TextShow

import qualified Monomer.Lens as L

data AppModel = AppModel {
  _sideBarVisible :: Bool,
  _maxSections :: Int,
  _activeSection :: Int
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | ShowSection Int
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  sideBarBg = wenv ^. L.theme . L.sectionColor
  sectionIds = [1 .. model ^. maxSections]

  {-
  This could even be a button, since they support focus and can be navigated with
  the keyboard. The 'ignoreTheme' config could be useful to reset styling (remove
  borders, etc).
  -}
  sectionSelector sectionIdx = selector where
    item = label ("Section " <> showt sectionIdx)
      `styleHover` [textColor lightSkyBlue]

    selector = box_ [alignLeft, onClick (ShowSection sectionIdx)] item
      `styleBasic` [cursorHand]

  sectionBody sectionIdx = item where
    cfgs = [alignCenter, alignMiddle]
    item = box_ cfgs (label $ "This is section " <> showt sectionIdx)
      `styleBasic` [minWidth 100, minHeight 100]
      `nodeVisible` (model ^. activeSection == sectionIdx)

  sideBarToggle = toggleButton "Toggle side bar" sideBarVisible

  sideBar = item where
    children = vstack_ [childSpacing] $
      sectionSelector <$> sectionIds

    layout = vstack [
        sideBarToggle,
        spacer,
        children
      ]
      `styleBasic` [width 200, minHeight 100, padding 10, bgColor sideBarBg]

    item = box_ [alignLeft, ignoreEmptyArea] layout

  menuBar = hstack [
      hstack_ [childSpacing] (sectionSelector <$> sectionIds),
      spacer,
      sideBarToggle
    ]

  mainContent = vstack_ [childSpacing] [
      menuBar,
      vstack (sectionBody <$> sectionIds)
    ]

  {-
  You can toggle 'onlyTopActive' (or even remove it) to avoid events reaching the
  bottom layer when the side bar is open.
  -}
  widgetTree = zstack_ [onlyTopActive_ False] [
      mainContent,
      sideBar `nodeVisible` (model ^. sideBarVisible)
    ] `styleBasic` [padding 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  ShowSection section -> [
    Model $ model
      & activeSection .~ section
      -- Uncommenting this line causes the side bar to close when a section is selected
      -- & sideBarVisible .~ False
    ]

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Menus and layouts",
      appWindowIcon "./assets/images/icon.bmp",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit
      ]
    model = AppModel {
      _sideBarVisible = False,
      _maxSections = 5,
      _activeSection = 1
    }