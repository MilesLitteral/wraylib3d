{-|
Module      : Main
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Main module for the 'Books' example.
-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE    FlexibleContexts    #-}
{-# LANGUAGE    FlexibleInstances   #-}
{-# LANGUAGE    ScopedTypeVariables #-}
{-# LANGUAGE    OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
-- Notes on making a Widget for direct backend use
-- Very important for the RendererWidget that is coming up
-- TODO:
-- 1) wiring mouse/keyboard/gamepad inputs 
-- 2) uploading a texture to your GPU/render engine 
-- 3) providing a render function that can bind textures and render textured triangles.
-- https://github.com/fjvallarino/monomer/issues/24
--  It is possible to make OpenGL-based widgets without rendering them to a texture. The render function of Widget runs
--  in IO instead of a restricted monad; this allows calling any OpenGL function you need. 
--  If other Renderer instances are added in the future (for Vulkan or Metal), this escape hatch 
--  will allow making low-level rendering with them too. The objective of choosing IO instead of a 
--  restricted monad was to give flexibility to applications that need this kind of functionality without 
--  tying the library to any specific backend.

-- When doing low-level OpenGL rendering, some care needs to be taken:
-- Since NanoVG depends on OpenGL's context, modifying it may cause unexpected results. You will need to call Renderer's saveContext/restoreContext for this.
-- Unless you are using appRenderOnMainThread, doing anything related to OpenGL outside the render function will cause a crash due to OpenGL's threading constraints. Consequently, shader compilation has to happen in the render function; you will need to use an IORef for storing state (that you can create on init) to avoid re-compiling the shader or re-creating vertex buffers every time. I will improve the API to remove this workaround and allow initializing OpenGL resources with a mechanism equivalent to RunTask.
-- I'll add an example with proper documentation; I had this in my backlog, but I never got around to implementing it. I'll keep you posted.

-- https://github.com/fjvallarino/monomer/issues/261
-- Map is currently JS (nightmare), 3D view is openGL, profile view is canvas, bottom right is SVG.

-- Peter
-- peterjamesward commented on Feb 21
-- Ah. I've found your openGL example, that should help!

-- fjvallarino commented on Feb 21
-- @peterjamesward I think creating a custom widget is the best option in this case, yes. 
-- If you only need 2D, maybe you are fine with the drawing API provided by the library (which in turn uses NanoVG). 
-- If one of your maps is tiles based (similar to what some clients of OpenStreetMap do), it will be simpler than going 
-- full into OpenGL.

-- https://github.com/fjvallarino/monomer/issues/10
-- Monomer.Graphics.Types.Renderer (hs)
-- The library has a Renderer abstraction that is used by widgets for all rendering related operations. 
-- Currently, there is only one implementation based on NanoVG, although other implementations could be added.
--  I recently added this section to Design decisions that could be of interest, especially for the alternative 
--  renderer ideas.

-- Regarding GLFW, I think that one is going to be much harder. Originally I wanted to abstract SDL to be able to 
-- replace it with a different library (GLFW in particular), but it turned out to be complex, and I didn't feel it 
-- provided much value for what I needed at that point (plus I'd like to support mobile someday, which GLFW does not
--  handle). At this point, Monomer is pretty much tied to SDL2.

-- In Monomer.Graphics.Types.Renderer 
-- This runs _before_ overlays of any type, and it's useful for the content of widgets created with low level APIs.
--   -}
--   createRawTask :: IO () -> IO (),
--   -- | Renders the added raw tasks and clears its queue.
--   renderRawTasks :: IO (),
--   {-|
--   Creates an overlay which does not rely on the abstractions provided by the
--   Renderer. Well suited for pure OpenGL/Vulkan/Metal.

--   This runs _after_ overlays based on Renderer.
--   -}
--   createRawOverlay :: IO () -> IO (),
--   -- | Renders the added raw overlays and clears its queue.
--   renderRawOverlays :: IO (),

-- Note regarding mixing lower-level rendering code
-- While still using the current NanoVG implementation, in your widget's render function, you can call createRawTask; 
-- the action provided to this function will be run outside the context of NanoVG. Since NanoVG uses OpenGL, this means 
-- you will be able to run any OpenGL command you want (and also means you have to take care of setting everything up, 
-- including shaders). Raw tasks are executed after rendering regular widgets (it's a separate rendering pass).

-- I don't currently have an example at hand, and it's been a while since I last tested this, so weird issues may happen
-- that I'm unaware of.

-- dpwiz commented on Aug 18, 2021
-- Hmm. Building on DearImGui is an interesting venue - opens up many different backends.

-- dpwiz commented on Aug 18, 2021
-- The polymorphic event story is a mess, I agree. I, too, decided to ditch it for now, in my engine and stuck with glfw instead.

-- fjvallarino commented on Aug 18, 2021
-- Just to clarify, the widgets are agnostic to SDL2. They handle events and requests using Monomer's types and functions.

-- What I mean when I say that it is tied to SDL2 is that the internals, which are not visible to library users, are 
-- programmed directly using SDL2 functions instead of using some abstraction. This is contrary to what happens with 
-- Renderer, which can be swapped for a different one.

-- fjvallarino commented on Aug 22, 2021
-- @dpwiz I'll close this issue for the time being. If you have any other questions or issues, don't hesitate to re-open it or create a new one. Thanks!


-- dpwiz commented on Sep 16, 2021
-- BTW, I've added most of the DrawList methods to dear-imgui bindings.
-- There are some event/input related functions I need to investigate, they might provide that cross-framework platform.

-- fjvallarino commented on Sep 17, 2021
-- @dpwiz nice! I'll take a look. My main concern is I'm relying on quite a few NanoVG features:

-- Painting with a texture inside an arbitrary path (can be seen when setting rounded corners on an image widget)
-- General curves (bezier, quadratic, etc), which are rendered pretty nicely.
-- I have to check how hard it is to get the same out of ImGUI's DrawList.

-- https://github.com/fjvallarino/monomer/issues/52 (Bonus)
module Main where

import Data.List
import Data.List.Lens
import Data.Maybe
import Data.Binary
import Data.Default
import Data.Either.Extra
import Data.Text ( Text, pack )
import Data.Time (Day, addDays, defaultTimeLocale, formatTime, fromGregorian)

import Control.Lens hiding (index)
import Control.Exception
import TextShow

import System.FilePath  ( takeBaseName )
import System.Directory ( createDirectoryIfMissing, listDirectory )
import qualified Data.Text as T
import qualified Data.Sequence as S
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as Sess

import Monomer        hiding (Column(..))
import Monomer.Hagrid 
import qualified Data.Map     as M
import qualified Monomer.Lens as L
import Text.Printf
import HRayLib3d.Network.FTP
import HRayLib3d.Core.BuildBundler
import HRayLib3d.WindowSystem.Theme
import HRayLib3d.Utils.Tooltips
import HRayLib3d.WindowSystem.Core           hiding (name)
import HRayLib3d.WindowSystem.RendererWidget hiding (name)

type BooksWenv = WidgetEnv  BooksModel BooksEvt
type BooksNode = WidgetNode BooksModel BooksEvt
    
keyListType c v =
    case c of
      XBOX_CONTROLLER     -> xboxKeyList    v
      PS_CONTROLLER       -> psKeyList      v
      TP_CONTROLLER       -> thirdPartyList v 
      GENERIC_CONTROLLER  -> genericKeyList v

keyTextListType c =
  case c of
    XBOX_CONTROLLER     -> xboxKeyTextList
    PS_CONTROLLER       -> psKeyTextList    
    TP_CONTROLLER       -> thirdKeyTextList 
    GENERIC_CONTROLLER  -> thirdKeyTextList

xboxKeyTextList = [Mapping,        
  START,           
  SELECT,          
  L_DPAD,          
  R_DPAD,          
  U_DPAD,          
  D_DPAD,          
  L_STICK,         
  R_STICK,         
  BUTTON_A, 
  BUTTON_B,   
  BUTTON_X,     
  BUTTON_Y,  
  BUTTON_L1, 
  BUTTON_R1,
  BUTTON_L2, 
  BUTTON_R2,
  BUTTON_L3,
  BUTTON_R3
  ]       

psKeyTextList = [
    Mapping,           
    OPTION,          
    SHARE,           
    HOME_BUTTON,     
    TRACKPAD,         --Playstation Only
    PS_BUTTON_A,     
    PS_BUTTON_B,
    PS_BUTTON_X,     
    PS_BUTTON_Y,     
    PS_BUTTON_L1,    
    PS_BUTTON_R1,    
    PS_BUTTON_L2,    
    PS_BUTTON_R2,    
    PS_BUTTON_L3,    
    PS_BUTTON_R3   
    ] 

thirdKeyTextList =  [
    Mapping,                 
    KEYBOARD_KEY,    
    L_MOUSE_KEY,     
    R_MOUSE_KEY,     
    C_MOUSE_KEY,     
    XR_GESTURE    
    ]    

xboxKeyList c = 
  case c of
    Mapping         -> image "./assets/images/ControllerIcons/mapping_icon.png"                   `styleBasic` [height 15,width 15]
    START           -> image "./assets/images/ControllerIcons/Special/Menu_dark.png"              `styleBasic` [height 15,width 15]
    SELECT          -> image "./assets/images/ControllerIcons/Special/View_dark.png"              `styleBasic` [height 15,width 15]
    L_DPAD          -> image "./assets/images/ControllerIcons/Digipad/Digipad_left_dark.png"      `styleBasic` [height 15,width 15]
    R_DPAD          -> image "./assets/images/ControllerIcons/Digipad/Digipad_right_dark.png"     `styleBasic` [height 15,width 15]
    U_DPAD          -> image "./assets/images/ControllerIcons/Digipad/Digipad_up_dark.png"        `styleBasic` [height 15,width 15]
    D_DPAD          -> image "./assets/images/ControllerIcons/Digipad/Digipad_down_dark.png"      `styleBasic` [height 15,width 15]
    L_STICK         -> image "./assets/images/ControllerIcons/Stick/L_dark.png"      `styleBasic` [height 15,width 15]--, sizeReqW $ fixedSize 25, sizeReqH $ fixedSize 25]
    R_STICK         -> image "./assets/images/ControllerIcons/Stick/R_dark.png"      `styleBasic` [height 15,width 15]--, sizeReqW $ fixedSize 25, sizeReqH $ fixedSize 25]
    BUTTON_A        -> image "./assets/images/ControllerIcons/Button/A_color_dark.png"      `styleBasic` [height 15,width 15]--, sizeReqW $ fixedSize 25, sizeReqH $ fixedSize 25]
    BUTTON_B        -> image "./assets/images/ControllerIcons/Button/B_color_dark.png"      `styleBasic` [height 15,width 15]--, sizeReqW $ fixedSize 25, sizeReqH $ fixedSize 25]
    BUTTON_X        -> image "./assets/images/ControllerIcons/Button/X_color_dark.png"      `styleBasic` [height 15,width 15]--, sizeReqW $ fixedSize 25, sizeReqH $ fixedSize 25]
    BUTTON_Y        -> image "./assets/images/ControllerIcons/Button/Y_color_dark.png"      `styleBasic` [height 15,width 15]
    BUTTON_L1       -> image "./assets/images/ControllerIcons/ShoulderButton/LB_dark.png"      `styleBasic` [height 15,width 15]
    BUTTON_R1       -> image "./assets/images/ControllerIcons/ShoulderButton/RB_dark.png"      `styleBasic` [height 15,width 15]
    BUTTON_L2       -> image "./assets/images/ControllerIcons/ShoulderButton/LT_dark.png"      `styleBasic` [height 15,width 15]
    BUTTON_R2       -> image "./assets/images/ControllerIcons/ShoulderButton/RT_dark.png"      `styleBasic` [height 15,width 15]
    BUTTON_L3       -> image "./assets/images/ControllerIcons/ShoulderButton/windows_sm.png"      `styleBasic` [height 15,width 15]
    BUTTON_R3       -> image "./assets/images/ControllerIcons/ShoulderButton/windows_sm.png"      `styleBasic` [height 15,width 15]

psKeyList c = 
  case c of
    Mapping         -> image "./assets/images/ControllerIcons/mapping_icon.png"                   `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
    OPTION          -> image "./assets/images/ControllerIcons/PlaystationButtons/Share.png"       `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
    SHARE           -> image "./assets/images/ControllerIcons/PlaystationButtons/Share.png"       `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
    HOME_BUTTON     -> image "./assets/images/ControllerIcons/PlaystationButtons/PSButton.png"    `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
    TRACKPAD        -> image "./assets/images/ControllerIcons/PlaystationButtons/Touchbar.png"    `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50] --Playstation Only
    PS_BUTTON_A     -> image "./assets/images/ControllerIcons/PlaystationButtons/X.png"      `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
    PS_BUTTON_B     -> image "./assets/images/ControllerIcons/PlaystationButtons/Circle.png"      `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
    PS_BUTTON_X     -> image "./assets/images/ControllerIcons/PlaystationButtons/Square.png"      `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
    PS_BUTTON_Y     -> image "./assets/images/ControllerIcons/PlaystationButtons/Triangle.png"      `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
    PS_BUTTON_L1    -> image "./assets/images/ControllerIcons/PlaystationButtons/L1.png"      `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
    PS_BUTTON_R1    -> image "./assets/images/ControllerIcons/PlaystationButtons/R1.png"      `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
    PS_BUTTON_L2    -> image "./assets/images/ControllerIcons/PlaystationButtons/L2.png"      `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
    PS_BUTTON_R2    -> image "./assets/images/ControllerIcons/PlaystationButtons/R2.png"      `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
    PS_BUTTON_L3    -> image "./assets/images/ControllerIcons/PlaystationButtons/L3.png"      `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
    PS_BUTTON_R3    -> image "./assets/images/ControllerIcons/PlaystationButtons/R3.png"      `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]

thirdPartyList c =   
  case c of
    Mapping         -> image "./assets/images/ControllerIcons/mapping_icon.png"       `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]        
    KEYBOARD_KEY    -> image "./assets/images/ControllerIcons/keyboard_key.png"       `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
    L_MOUSE_KEY     -> image "./assets/images/ControllerIcons/mouse_left_click.png"   `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
    R_MOUSE_KEY     -> image "./assets/images/ControllerIcons/mouse_right_click.png"  `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
    C_MOUSE_KEY     -> image "./assets/images/ControllerIcons/mouse_middle_click.png" `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
    XR_GESTURE      -> image "./assets/images/ControllerIcons/xr_gesture.png"         `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]  

genericKeyList c =   
  case c of
    Mapping         -> image "./assets/images/ControllerIcons/mapping_icon.png"       `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]        
    KEYBOARD_KEY    -> image "./assets/images/ControllerIcons/keyboard_key.png"       `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
    L_MOUSE_KEY     -> image "./assets/images/ControllerIcons/mouse_left_click.png"   `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
    R_MOUSE_KEY     -> image "./assets/images/ControllerIcons/mouse_right_click.png"  `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
    C_MOUSE_KEY     -> image "./assets/images/ControllerIcons/mouse_middle_click.png" `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
    XR_GESTURE      -> image "./assets/images/ControllerIcons/xr_gesture.png"         `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]  

doneBg :: Color
doneBg     = rgbHex "#CFF6E2"

doneFg :: Color
doneFg     = rgbHex "#459562"

pendingBg :: Color
pendingBg  = rgbHex "#F5F0CC"

pendingFg :: Color
pendingFg  = rgbHex "#827330"

grayLight :: Color
grayLight  = rgbHex "#9E9E9E"

grayDark  :: Color
grayDark   = rgbHex "#393939"

grayDarker :: Color
grayDarker = rgbHex "#2E2E2E"

actionsColumn :: Int -> Spider -> WidgetNode s BooksEvt
actionsColumn idx _spdr =
  button "Feed" (FeedSpider idx)

gridColumns :: [Column BooksEvt Spider]
gridColumns = cols 
  where
    cols =
      [ (showOrdColumn "Level Index" index)
          { initialWidth = 120,
            align = ColumnAlignRight,
            footerWidget = CustomFooterWidget countFooter
          },
        (textColumn "Level Name" sName)
          { initialWidth  = 300,
            resizeHandler = Just NameColumnResized,
            sortHandler   = Just NameColumnSorted
          },
        (textColumn "Single Player Only" species)
          { initialWidth = 200
          },
        (textColumn "Last Date of Update" (T.pack . formatTime defaultTimeLocale "%Y-%m-%d" . dateOfBirth))
          { initialWidth = 200
          }--,
        -- (textColumn "Weight (Kg)"   (T.pack . printf "%.2f" . weightKilos))
        --   { sortKey      = SortWith weightKilos,
        --     initialWidth = 200,
        --     align        = ColumnAlignRight
        --     -- footerWidget = CustomFooterWidget $ sumWeightFooter
        --   },
        -- (widgetColumn "Actions" actionsColumn)
        --   { Monomer.Hagrid.initialWidth = 100,
        --     Monomer.Hagrid.paddingW     = 5,
        --     Monomer.Hagrid.paddingH     = 5
        --   }
      ]
    countFooter     spiders = labelledFooter "Realm Database Viewer" (T.pack . show . length $ spiders) --"Count" (T.pack . show . length $ spiders)
    sumWeightFooter spiders = tree
      where
        tree                = labelledFooter "Sum" (T.pack (printf "%.2f" totalWeightKilos))
        totalWeightKilos    = sum ((.weightKilos) . fst <$> spiders)
    labelledFooter labelText text =
      hstack
        [ 
          label labelText,
          --filler,
          label text
            `styleBasic` [textFont "Bold"]
        ] `styleBasic` [padding 10]

controllerRowKey :: ControllerButton -> Text
controllerRowKey cButton  = "controllerRow" <> showt (cButton ^. bId)

buildRowKey :: BuildDescription -> Text
buildRowKey buildDesc     = "todoRow" <> showt (buildDesc ^. buId)

cloudRowKey :: CloudDescription -> Text
cloudRowKey cloud         = "todoRow" <> showt (cloud ^. cId)

controllerRow :: BooksWenv -> BooksModel -> Int -> ControllerButton -> BooksNode
controllerRow wenv model idx t = animRow `nodeKey` cbKey where
  sectionBg      = wenv ^. L.theme . L.sectionColor
  rowButtonColor = wenv ^. L.theme . L.userColorMap . at "rowButton" . non def
  rowSepColor    = gray & L.a .~ 0.5

  cbKey  = controllerRowKey t
  cbDone = t ^. bstatus == Active
  isLast   = idx == length (model ^. controllerBindings ) - 1

  (cbBg, cbFg)
    | cbDone    = (doneBg,       doneFg)
    | otherwise = (grayLight,    grayDark)

  cbStatus    = labelS (t ^. bstatus)
    `styleBasic` [textFont "Regular", textSize 12, textAscender, textColor cbBg, padding 6, Monomer.paddingH 8, radius 12, bgColor cbFg]

  rowButton caption action = button caption action
    `styleBasic`      [textFont "UI", textMiddle, textColor rowButtonColor, bgColor transparent, border 0 transparent]
    `styleHover`      [bgColor  sectionBg]
    `styleFocus`      [bgColor (sectionBg & L.a .~ 0.5)]
    `styleFocusHover` [bgColor  sectionBg]
    
  todoInfo = hstack [
      vstack [
        keyListType (model ^. controllerType) (t ^. bType),--labelS  (t ^. bType) `styleBasic` [textSize 12, textColor darkGray],
        spacer_ [width 5],
        label   (t ^. bDescription) --`styleBasic` [textThroughline_ cbDone]
      ],
      filler,
      box_ [alignRight] cbStatus `styleBasic` [width 80],
      spacer,
      liftHelpOverlay (model ^. toolTips) "controller_add_button" $ rowButton "📝" (ControllerButtonEdit idx t),
      spacer,
      liftHelpOverlay (model ^. toolTips) "controller_delete_button" $ rowButton "✂️" (ControllerButtonDeleteBegin idx t)
    ] `styleBasic` [ paddingV 15, styleIf (not isLast) $ borderB 1 rowSepColor ]
  animRow  = animFadeOut_ [onFinished (ControllerButtonDelete idx t)] todoInfo

buildRow :: BooksWenv -> BooksModel -> Int -> BuildDescription -> BooksNode
buildRow wenv model idx t = animRow `nodeKey` buildKey where
  sectionBg      = wenv ^. L.theme . L.sectionColor
  rowButtonColor = wenv ^. L.theme . L.userColorMap . at "rowButton" . non def
  rowSepColor    = gray & L.a .~ 0.5

  buildKey  = buildRowKey t
  buildDone = t ^. buStatus == Build
  isLast    = idx == length (model ^. buildList) - 1
  buildListType c =
    case c of
      Win64       -> image "./assets/images/windows_sm.png"      `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
      UWP         -> image "./assets/images/uwp_sm.png"          `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
      MacOS       -> image "./assets/images/osx_sm.png"          `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
      Ubuntu      -> image "./assets/images/ubuntu_sm.png"       `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
      CMake       -> image "./assets/images/cmake_sm.png"        `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
      AutoMake    -> image "./assets/images/gnu_make_sm.png"     `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
      Djinni      -> image "./assets/images/djinni_sm.png"       `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
      Android     -> image "./assets/images/android_sm.png"      `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
      IOS         -> image "./assets/images/ios_sm.png"          `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
      WebGL_Build -> image "./assets/images/webgl_sm.png"        `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
      WASM_Build  -> image "./assets/images/wasm_sm.png"         `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]
      XR          -> image "./assets/images/xrExperience_sm.png" `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50]

  (buildBg, buildFg)
    | buildDone  = (doneBg,    doneFg)
    | otherwise  = (grayLight, grayDark)

  buildStatus    = buildListType (t ^. buType)
    `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50, padding 2,  radius 12, bgColor buildBg]

  rowButton caption action = button caption action
    `styleBasic`      [textFont "UI", textMiddle, textColor rowButtonColor, bgColor transparent, border 0 transparent]
    `styleHover`      [bgColor  sectionBg]
    `styleFocus`      [bgColor (sectionBg & L.a .~ 0.5)]
    `styleFocusHover` [bgColor  sectionBg]

  buildInfo = hstack [
      vstack [
        labelS (t ^. buType) `styleBasic` [textSize 12, textColor darkGray],
        spacer_ [width 5],
        label (t ^. buDescription) --`styleBasic` [textThroughline_ buildDone]
      ],
      filler,
      box_ [alignRight] buildStatus `styleBasic` [width 80],
      spacer,
      liftHelpOverlay (model ^. toolTips) "build_list_edit_button"   $ rowButton "📝" (BuildListEdit idx t),
      spacer,
      liftHelpOverlay (model ^. toolTips) "build_list_delete_button" $ rowButton "✂️" (BuildListDeleteBegin idx t)
    ] `styleBasic` [ paddingV 15, styleIf (not isLast) $ borderB 1 rowSepColor ]
  animRow  = animFadeOut_ [onFinished (BuildListDelete idx t)] buildInfo

cloudRow :: BooksWenv -> BooksModel -> Int -> CloudDescription -> BooksNode
cloudRow wenv model idx t = animRow `nodeKey` cloudKey where
  sectionBg      = wenv ^. L.theme . L.sectionColor
  rowButtonColor = wenv ^. L.theme . L.userColorMap . at "rowButton" . non def
  rowSepColor    = gray & L.a .~ 0.5
  cloudListType c =
    case c of
      Firebase    -> image_ "./assets/images/firebase_sm.png"  [fitNone, alignMiddle]
      GCP         -> image_ "./assets/images/gcp_sm.png"       [fitNone, alignMiddle]    
      Azure       -> image_ "./assets/images/azure_sm.png"     [fitNone, alignMiddle]
      Supabase    -> image_ "./assets/images/supabase_sm.png"  [fitNone, alignMiddle]

  cloudKey  = cloudRowKey t
  cloudDone = t ^. cStatus == On
  isLast   = idx == length (model ^. cloudList ) - 1

  (cloudBg, cloudFg)
    | cloudDone  = (doneBg,    doneFg)
    | otherwise  = (grayLight,  grayDark)

  cloudStatus    = cloudListType (t ^. cType)
    `styleBasic` [sizeReqW $ fixedSize 50, sizeReqH $ fixedSize 50, radius 12, bgColor cloudBg]

  rowButton caption action = button caption action
    `styleBasic`      [textFont "UI", textMiddle, textColor rowButtonColor, bgColor transparent, border 0 transparent]
    `styleHover`      [bgColor  sectionBg]
    `styleFocus`      [bgColor (sectionBg & L.a .~ 0.5)]
    `styleFocusHover` [bgColor  sectionBg]

  cloudInfo = hstack [
      vstack [
        labelS (t ^. cType) `styleBasic` [textSize 12, textColor darkGray],
        spacer_ [width 5],
        label (t ^. cDescription) --`styleBasic` [textThroughline_ cloudDone]
      ],
      filler,
      box_ [alignRight] cloudStatus, -- `styleBasic` [width 80],
      spacer,
      liftHelpOverlay (model ^. toolTips) "cloud_add_button" $ rowButton "📝" (CloudEdit idx t),
      spacer,
      liftHelpOverlay (model ^. toolTips) "cloud_delete_button" $ rowButton "✂️" (CloudDeleteBegin idx t)
    ] `styleBasic` [ paddingV 15, styleIf (not isLast) $ borderB 1 rowSepColor ]
  animRow  = animFadeOut_ [onFinished (CloudDelete idx t)] cloudInfo

-- addNew Functions Begin
controllerButtonEdit :: BooksWenv -> BooksModel -> BooksNode
controllerButtonEdit wenv model = editNode where
  sectionBg    = wenv ^. L.theme . L.sectionColor
  isValidInput = model ^. activeControllerBinding . bDescription /= ""

  (saveAction, saveLabel) = case model ^. cBAction  of
    ControllerButtonEditing idx -> (ControllerButtonSave idx, "Save")
    _ -> (ControllerButtonAdd, "Add")

  saveControllerBtn = mainButton saveLabel saveAction

  editFields  = keystroke [("Enter", saveAction) | isValidInput] $ vstack [
      hstack [
        label "Task:",
        spacer,
        textField (activeControllerBinding . bDescription) `nodeKey` "controllerDesc"
      ],
      spacer,
      hgrid [
        hstack [
          label "Type:",
          spacer,
          textDropdownS (activeControllerBinding . bType) (keyTextListType ((model ^. controllerType))) `nodeKey` "controllerType",
          spacer -- Added here to avoid grid expanding it to 1/3 total width
        ],
        hstack [
          label "Status:",
          spacer,
          textDropdownS (activeControllerBinding . bstatus) controllerButtonStatuses
        ]
      ]
    ]

  editNode = keystroke [("Esc", ControllerButtonCancel)] $ vstack [
      editFields,
      spacer,
      hstack [
        filler,
        saveControllerBtn `nodeEnabled` isValidInput,
        spacer,
        button "Cancel" ControllerButtonCancel
        ]
    ] `styleBasic` [bgColor sectionBg, padding 20]

addNewControllerButton :: WidgetEnv s e -> BooksModel -> BooksModel
addNewControllerButton wenv model = newModel where
  newCB = model ^. activeControllerBinding
    & bId .~ currentTimeMs wenv
  newModel = model
    & controllerBindings .~ (newCB : model ^. controllerBindings)

buildEdit :: BooksWenv -> BooksModel -> BooksNode
buildEdit wenv model = editNode where
  sectionBg    = wenv ^. L.theme . L.sectionColor
  isValidInput = model ^. activeBuildList . buDescription /= ""

  (saveAction, saveLabel) = case model ^. bUAction of
    BuildEditing idx -> (BuildListSave idx, "Save")
    _ -> (BuildListAdd, "Add")

  saveBuildBtn = mainButton saveLabel saveAction

  editFields  = keystroke [("Enter", saveAction) | isValidInput] $ vstack [
      hstack [
        label "Task:",
        spacer,
        textField (activeBuildList . buDescription) `nodeKey` "buildDesc"
      ],
      spacer,
      hgrid [
        hstack [
          label "Type:",
          spacer,
          textDropdownS (activeBuildList . buType) buildTypes `nodeKey` "buildType",
          spacer -- Added here to avoid grid expanding it to 1/3 total width
        ],
        hstack [
          label "Status:",
          spacer,
          textDropdownS (activeBuildList . buStatus) buildStatuses
        ]
      ]
    ]

  editNode = keystroke [("Esc", BuildListCancel)] $ vstack [
      editFields,
      spacer,
      hstack [
        filler,
        saveBuildBtn `nodeEnabled` isValidInput,
        spacer,
        button "Cancel" BuildListCancel
        ]
    ] `styleBasic` [bgColor sectionBg, padding 20]

addNewBuild :: WidgetEnv s e -> BooksModel -> BooksModel
addNewBuild wenv model = newModel where
  newBuild = model ^. activeBuildList
    & buId .~ currentTimeMs wenv
  newModel = model
    & buildList .~ (newBuild : model ^. buildList)

cloudEdit :: BooksWenv -> BooksModel -> BooksNode
cloudEdit wenv model = editNode where
  sectionBg    = wenv ^. L.theme . L.sectionColor
  isValidInput = model ^. activeCloudList . cDescription /= ""

  (saveAction, saveLabel) = case model ^. cLAction of
    CloudEditing idx -> (CloudSave idx, "Save")
    _ -> (CloudAdd, "Add")

  saveCloudBtn = mainButton saveLabel saveAction

  editFields  = keystroke [("Enter", saveAction) | isValidInput] $ vstack [
      hstack [
        label "Task:",
        spacer,
        textField (activeCloudList . cDescription) `nodeKey` "cloudDesc"
      ],
      spacer,
      hgrid [
        hstack [
          label "Type:",
          spacer,
          textDropdownS (activeCloudList . cType) cloudTypes `nodeKey` "cloudType",
          spacer -- Added here to avoid grid expanding it to 1/3 total width
        ],
        hstack [
          label "Status:",
          spacer,
          textDropdownS (activeCloudList . cStatus) cloudStatuses
        ]
      ]
    ]

  editNode = keystroke [("Esc", CloudCancel)] $ vstack [
      editFields,
      spacer,
      hstack [
        filler,
        saveCloudBtn `nodeEnabled` isValidInput,
        spacer,
        button "Cancel" CloudCancel
        ]
    ] `styleBasic` [bgColor sectionBg, padding 20]

addNewCloud :: WidgetEnv s e -> BooksModel -> BooksModel
addNewCloud wenv model = newModel where
  newTodo = model ^. activeCloudList
    & cId .~ currentTimeMs wenv
  newModel = model
    & cloudList .~ (newTodo : model ^. cloudList)
-- addNew Functions End

bookImage :: Maybe Int -> Text -> WidgetNode BooksModel BooksEvt
bookImage imgId size = maybe filler coverImg imgId where
  baseUrl = "http://covers.openlibrary.org/b/id/<id>-<size>.jpg"
  imgUrl i = T.replace "<size>" size $ T.replace "<id>" (showt i) baseUrl
  coverImg i = image_ (imgUrl i) [fitHeight, alignRight]

bookRow :: BooksWenv -> Book -> BooksNode
bookRow wenv b = row where
  rowBgColor   = wenv ^. L.theme . L.userColorMap . at "rowBgColor" . non def
  publishYear  = maybe "" showt (b ^. year)
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
bookDetail b = content `styleBasic` [Monomer.minWidth 500, Monomer.paddingH 20] where
  hasCover = isJust (b ^. cover)
  publishYear = maybe "" showt (b ^. year)

  shortLabel value = label value `styleBasic` [textFont "Medium", textTop]
  longLabel  value = label_ value [multiline, ellipsis, trimSpaces]

  content = hstack [
      vstack_ [childSpacing] [
        shortLabel (b ^. title)
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
readProjectFileIndex   projSess = map (\x -> hstack [(image_  . matchPFType $ _projectFileType x) [fitWidth] `styleBasic` [(textColor $ rgbHex  "#000000"),border 1 black, width 35, height 35], label $ pack $ _projectFileName x]) $ _projectFiles projSess

generateProjectFileGUI :: ProjectSession -> [WidgetNode BooksModel BooksEvt]
generateProjectFileGUI projSess = reverse $ map (\x -> hstack [spacer, (image_  . matchPFType $ _projectFileType x) [fitWidth] `styleBasic` [border 1 black, width 45, height 45], label $ pack $ " " ++ _projectFileName x]) $ _projectFiles projSess

hagridKey :: Text
hagridKey = "SpiderHagrid"



--https://github.com/fjvallarino/monomer/issues/83
buildUI :: WidgetEnv BooksModel BooksEvt -> BooksModel -> WidgetNode BooksModel BooksEvt
buildUI wenv model = widgetTree where
  colorsMap           = M.fromList [(red, "Red"), (green, "Green"), (blue, "Blue"), (orange, "Orange")]
  colors              = M.keys colorsMap
  colorDropdown field = textDropdown_ field colors (colorsMap M.!) []
  sideBarBg     = wenv ^. L.theme . L.sectionColor
  sectionBg     = wenv ^. L.theme . L.sectionColor
  isControllerButtonEditing -- CB
    | ControllerButtonEditing _ <- model ^. cBAction = True
    | otherwise = False

  isBuildEditing -- Build
    | BuildEditing _ <- model ^. bUAction = True
    | otherwise = False

  isCloudEditing -- Cloud
    | CloudEditing _ <- model ^. cLAction = True
    | otherwise = False

  sectionIds    = [1 .. model ^. maxSections]

  -- Grid Configurations
  -- bring this back
  -- themeConfigurer =
  --     hstack_
  --       [childSpacing]
  --       [ labeledRadio_ "Dark Theme" darkTheme   _theme [textRight],
  --         labeledRadio_ "Light Theme" lightTheme _theme [textRight]
  --       ]

  columnConfigurers = zipWith columnConfigurer [0 .. (length (model ^. columns) - 1)] gridColumns

  columnConfigurer :: Int -> Column BooksEvt Spider -> WidgetNode BooksModel BooksEvt
  columnConfigurer idx columnDef = labeledCheckbox_ (name columnDef) (columns   . singular (ix idx) . _enabled) [textRight]

  actionButtons =
    [ hstack_
        [childSpacing]
        [ label "Scroll to index of unsorted list",
          numericField rowToScrollTo,
          button "Go!" ScrollToOriginalIndex
        ],
      hstack_
        [childSpacing]
        [button "Load .db File" None, button "Validate Changes to Queries" None, separator, button "Add Entry" AddSpider, button "Remove Entry" None]
    ]
  -- todoDone = (t ^. status) == Done
  -- (todoBg, todoFg)
  --   | todoDone  = (doneBg, doneFg)
  --   | otherwise = (pendingBg, pendingFg)

  sectionSelector sectionIdx = selector where
    item     = label ("Section " <> showt sectionIdx) `styleHover` [textColor lightSkyBlue]
    selector = box_ [alignLeft, onClick (ShowSection sectionIdx)] item `styleBasic` [cursorHand] `styleBasic` [border 1 black, width 45, height 45]

    --t ^. status
  sideBarToggle = liftHelpOverlay (model ^. toolTips) "toggle_inspector" $ toggleButtonV "Toggle Inspector" (model ^. sideBarVisible) SideBarSummon :: WidgetNode BooksModel BooksEvt

  sideBar    = item where
    children = vstack_ [childSpacing] $ sectionSelector <$> sectionIds
    rowButtonColor = wenv ^. L.theme . L.userColorMap . at "rowButton" . non def
    rowButton caption action = button caption action
      `styleBasic`      [textFont "UI", textMiddle, textColor rowButtonColor, bgColor transparent, border 0 transparent]
      `styleHover`      [bgColor  sectionBg]
      `styleFocus`      [bgColor (sectionBg & L.a .~ 0.5)]
      `styleFocusHover` [bgColor  sectionBg]

    layout = vstack [
        sideBarToggle,
        spacer,
        liftHelpOverlay (model ^. toolTips) "inspector_object" $ hstack [
          label  "♌️" `styleBasic` [textFont "UI"], 
          label  "Null Object", 
          spacer,
          --readd todoFg, rename todo to sceneObjectFg, make it work correctly
          labelS (pack "N/V") `styleBasic` [textFont "Regular", textSize 12, textAscender, textColor grayDark, padding 6, Monomer.paddingH 8, radius 12, bgColor grayLight],
          spacer,
          rowButton "📝" StartIDE `styleBasic` [textFont "UI", textMiddle ],
          rowButton "📷" None
        ],
        spacer,
        children
      ] `styleBasic` [border 1 black, width 45, height 45, width 300, minHeight 100, padding 10, bgColor sideBarBg]

    item = box_ [alignLeft, ignoreEmptyArea] layout

  countLabel    = label caption `styleBasic` styles where
    caption     = "Files (" <> showt (length $ model ^. books) <> ")"
    styles      = [textFont "Regular", textSize 16, padding 20, bgColor sectionBg]

  controllerCountLabel = label caption `styleBasic` styles where
      caption   = "Registered Buttons (" <> showt (length $ model ^. books) <> ")"
      styles    = [textFont "Regular", textSize 16, padding 20, bgColor sectionBg]

  searchOverlay = box content `styleBasic` [bgColor (darkGray & L.a .~ 0.8)] where
    content     = label "Searching" `styleBasic` [textSize 20, textColor black]

  errorOverlay = alert BooksCloseError content where
    content    = fromMaybe spacer (Just $ label err)
    err        = rerr $ model ^. errorMsg

  -- for file selection
  bookOverlay  = alert BooksCloseDetails content where
    content    = fromMaybe spacer (Just $ bookDetail book)
    book       = rbook $ model ^. selected

  searchForm   = keystroke [("Enter", BooksSearch)] $ vstack [
      hstack [
        label "Query:",
        spacer,
        textField query `nodeKey` "query",
        spacer,
        mainButton "Search" BooksSearch
      ] `styleBasic` [bgColor (rgbHex "A6A6A6"), padding 25]
    ]

  confirmDeleteLayerCB = case model ^. cBAction of
    ControllerButtonConfirmingDelete idx cb -> [popup] where
      popup = confirmMsg msg (ControllerButtonDelete idx cb) ControllerButtonCancelDelete :: WidgetNode BooksModel BooksEvt
      msg   = "Are you sure you want to delete '" <> cb ^. bDescription <> "' ?"
    _ -> []
 
  confirmDeleteLayerBU = case model ^. bUAction of
    BuildConfirmingDelete idx cb -> [popup] where
      popup = confirmMsg msg (BuildListDelete idx cb) BuildListCancelDelete :: WidgetNode BooksModel BooksEvt
      msg   = "Are you sure you want to delete '" <> cb ^. buDescription <> "' ?"
    _ -> []

  confirmDeleteLayerCL = case model ^. cLAction of
    CloudConfirmingDelete idx cloud -> [popup] where
      popup = confirmMsg msg (CloudConfirmDelete idx cloud) CloudCancelDelete :: WidgetNode BooksModel BooksEvt
      msg   = "Are you sure you want to delete '" <> cloud ^. cDescription <> "' ?"
    _ -> []

  separator     = separatorLine
  contList      = vstack (zipWith (controllerRow wenv model) [0..] (model ^. controllerBindings))
  buList        = vstack (zipWith (buildRow wenv model) [0..] (model ^. buildList))
  clList        = vstack (zipWith (cloudRow wenv model) [0..] (model ^. cloudList))

  newControllerButton = mainButton "New Button"            ControllerButtonNew `nodeKey` "controllerNew" `nodeVisible` not isControllerButtonEditing
  addPlatform         = mainButton "Add Platform"          BuildListNew        `nodeKey` "buildNew" `nodeVisible` not isBuildEditing
  addCloudConnection  = mainButton "Add Cloud Connection"  CloudNew            `nodeKey` "cloudNew" `nodeVisible` not isCloudEditing

  dialogBoxMenu        field content       = box_ [alignCenter, onClickEmpty OpenFS]   (boxShadow $ popup field content)
  fullscreenDialogMenu field content       = popup field content `styleBasic` [bgColor (rgbHex "#000000")]
  menuBar = hstack [
      hstack_ [childSpacing] (sectionSelector <$> sectionIds),
      spacer,
      sideBarToggle
    ]

  sectionBody sectionIdx = item where
    cfgs = [alignCenter, alignMiddle]
    item = box_ cfgs (label $ "This is section " <> showt sectionIdx) `styleBasic` [Monomer.minWidth 100, Monomer.minHeight 100]
      `nodeVisible` (model ^. activeSection == sectionIdx)

  booksChanged wenv old new = old ^. books /= new ^. books

  editControllerButtonLayer = content where
    dualSlide content = outer where
      inner = animSlideIn_  [slideTop, duration 200] content
        `nodeKey` "animControllerButtonEditIn"
      outer = animSlideOut_ [slideTop, duration 200, onFinished ControllerButtonHideEditDone] inner
        `nodeKey` "animControllerButtonEditOut"

    content = vstack [
        dualSlide (controllerButtonEdit wenv model),
        filler
      ] `styleBasic` [bgColor (grayDark & L.a .~ 0.5)]

  editBuildLayer = content where
        dualSlide content = outer where
          inner = animSlideIn_ [slideTop, duration 200] content
            `nodeKey` "animBuildEditIn"
          outer = animSlideOut_ [slideTop, duration 200, onFinished BuildListHideEditDone] inner
            `nodeKey` "animBuildEditOut"
    
        content = vstack [
            dualSlide (buildEdit wenv model),
            filler
          ] `styleBasic` [bgColor (grayDark & L.a .~ 0.5)]

  editCloudLayer = content where
    dualSlide content = outer where
      inner = animSlideIn_ [slideTop, duration 200] content
        `nodeKey` "animCloudEditIn"
      outer = animSlideOut_ [slideTop, duration 200, onFinished CloudHideEditDone] inner
        `nodeKey` "animCloudEditOut"

    content = vstack [
        dualSlide (cloudEdit wenv model),
        filler
      ] `styleBasic` [bgColor (grayDark & L.a .~ 0.5)]

  mainLayer = vstack [
      label "Controller Setup",
      controllerCountLabel `styleBasic` [textMiddle],
      scroll_ [] (contList `styleBasic` [padding 20, paddingT 5]),
      filler,
      box_ [alignRight] newControllerButton `styleBasic` [bgColor sectionBg, padding 20]
    ]  `styleBasic` [sizeReqW $ fixedSize 200, sizeReqH $ fixedSize 300]

  controllerCase c =
    case c of
      XBOX_CONTROLLER    -> [image "./assets/images/xboxController.png"]
      PS_CONTROLLER      -> [image "./assets/images/psControllerColored.png"]
      TP_CONTROLLER      -> [image "./assets/images/thirdPartyController.png"]
      GENERIC_CONTROLLER -> [image "./assets/images/controller_icon.png"]

  -- Panel 1
  controllerWidgetTree = vstack [
      spacer,
      hstack [
        label "Controller Type", 
        spacer, 
        button "Xbox"        XboxController, 
        spacer, 
        button "PlayStation" PsController, 
        spacer, 
        button "Third Party" ThirdPartyController
      ],
      spacer,
      hstack $ controllerCase (model ^. controllerType),   
      spacer,
      mainLayer
    ]

  -- Panel 2
  setPlatformWidgetTree = vstack [
    spacer, 
    label "Platform Setup",
    scroll_ [] (buList `styleBasic` [padding 20, paddingT 5]),
    filler,
    box_ [alignRight] addPlatform `styleBasic` [bgColor sectionBg, padding 20]
    ]

  -- Panel 3
  cloudSetupTree = vstack [
    spacer, 
    label "Cloud Setup",
    scroll_ [] (clList `styleBasic` [padding 20, paddingT 5]),
    filler,
    box_ [alignRight] addCloudConnection
      `styleBasic` [bgColor sectionBg, padding 20]
    ]

  -- Panel 4
  appProjectPrefsTree = scroll $ vstack [
    spacer,
    label "Project & Realm Preferences",
    spacer,
    vstack[ 
      hstack [label "Project Name", spacer,  textField projectTitleName],
      spacer,
      hstack [label "Current Level Name", spacer,  textField currentRealmName],
      spacer, 
      hstack [label "Invoke IDE Cmd: ", spacer,  textField ideArgument :: WidgetNode BooksModel BooksEvt]
    ],
    spacer,
    separator,
    spacer,
    label "Editor Options",
    hstack [label "Selected Theme Color", colorPicker_ colorPicked [showAlpha]], 
    spacer,
    textDropdownS widgetSelected ["Button Color", "Text Color", "BgColor"],
    spacer,
    label "Saved Themes",
    spacer,
    textDropdownS widgetSelected ["N/A"],
    spacer,
    button "Save Assigned Color" None,
    spacer,
    separator,
    spacer,
    label "Linking Options",
    spacer,
    hstack [label "External Deps To Link (ex l.lib;lib.dll)", spacer, textField externalDepsList],
    filler
    ]

  -- put Connected IDE
  -- create alternative trees for the other menus and add transitions to all of them 
  -- animFadeIn (widget) `nodeKey` "fadeTimeLabel" -- (see handleEvent)
  -- SideBarSummon  bl  -> [Model $ model & sideBarVisible .~ bl, Message "fadeTimeLabel" AnimationStart]
  -- Panel 5
  appRendererTree = vstack [
    spacer,
    label "Renderer Settings",
    spacer,
    hstack [ label "Current* Renderer**: ", textDropdownS currentRenderer ["OpenGL", "Vulkan", "Metal"] ],
    spacer,
    hstack[label "SPIR-V Only", spacer, checkbox spirVOnly, spacer, label "GLSL Only", spacer, checkbox glslOnly],
    spacer,
    label "*  = The backend will fallback to OpenGL in all instances of failure",
    spacer,
    label "** = Vulkan runs on all computer platforms, OpenGL(GLES) is defaulted to for Android, native Metal is optional for Mac/iOS",
    spacer,
    label "Note: by Default the OpenGL backend is SPIR-V enabled, a SPIR-V/GLSL toggle will be available soon(TBA)",
    -- spacer,
    -- hstack [ label "Current Renderer", textDropdownS currentRenderer ["OpenGL(Default)", "Vulkan", "Metal(OSX only)"] ],
    filler
    ]
    -- ShowRealmViewer 

  appRealmViewerList  = intersperse spacer $ map (\x -> button (x ^. rlmLevelName) (ConfirmParentEvt $ OpenRealm x)) $ model ^. projectLoadedRealms
  appRealmViewerTree  = vstack [
      spacer,
      label "Realm Viewer",
      spacer,
      label "Level Name",
      textFieldV (model ^. selectedRealm .  rlmLevelName)   (\x -> UpdateRealmNameLocal        x),
      spacer,
      label "Realm Set BSP",
      textFieldV (model ^. selectedRealm .  rlmBsp)         (\x -> UpdateRealmBSPNameLocal     x),
      spacer,
      label "Multiplayer?",
      checkboxV  (model ^. selectedRealm .  rlmMultiplayer) (\x -> UpdateRealmMultiplayerLocal x),
      spacer
      --TODO: add realm viewer to "views" list
      --button "Close View" OpenRealmViewer
    ]

  -- Panel 6
  appRealmDBTree = vstack [ 
    spacer,
    separatorLine,
    spacer, 
    grid `nodeKey` hagridKey,
    spacer,
    separator,
    vstack_ [childSpacing] (separator : spacer : {-themeConfigurer :-} columnConfigurers <> actionButtons),
      spacer
      `styleBasic` [padding 8]
    ]

  grid =
    hagrid_
      [initialSort 1 SortDescending]
      (mconcat (zipWith column (model ^. columns) gridColumns))
      (model ^. spiders)

  column (AppColumn enabled) columnDef = [columnDef | enabled]
  -- create alternative trees for the other menus and add transitions to all of them 
  -- animFadeIn (widget) `nodeKey` "fadeTimeLabel" -- (see handleEvent)
  -- SideBarSummon  bl  -> [Model $ model & sideBarVisible .~ bl, Message "fadeTimeLabel" AnimationStart]
  
   --2d renderer pipeline
  twoDimensionalRendererWidget :: BooksModel -> ALens' BooksModel Color -> ALens' BooksModel Color -> ALens' BooksModel Color -> ALens' BooksModel Color -> WidgetNode BooksModel BooksEvt
  twoDimensionalRendererWidget model col1 col2 col3 col4 = vstack [
            hgrid [
            openGLWidget (model ^. color1) `styleBasic` [padding 20],
            openGLWidget (model ^. color2) `styleBasic` [padding 20]
            --{- scroll () `styleBasic` [width 800, height 800] -}
          ],
          hgrid [
            openGLWidget (model ^. color3) `styleBasic` [padding 20],
            openGLWidget (model ^. color4) `styleBasic` [padding 20]
          ],
          -- 2d Renderer Pipeline
          -- big kahuna goes here
          -- Directly Control The Rendering Viewports with a widget
          hstack [
            label "Color 1:",
            spacer,
            colorDropdown col1,
            spacer,
            label "Color 2:",
            spacer,
            colorDropdown col2,
            spacer,
            label "Color 3:",
            spacer,
            colorDropdown col3,
            spacer,
            label "Color 4:",
            spacer,
            colorDropdown col4
          ]
        ]

  threeDimensionalRendererWidget :: WidgetNode BooksModel BooksEvt
  threeDimensionalRendererWidget = hgrid [ codenameBigKahuna ]

  rendererContext b = case b of
                        True  -> threeDimensionalRendererWidget `nodeVisible`  (model ^. threeDimensional)
                        False -> twoDimensionalRendererWidget model color1 color2 color3 color4  `nodeVisible` (model ^. twoDimensional)

  widgetTree = zstack ((([
    vstack [
      spacer,
      separator,
      spacer,
      hstack_ [childSpacing] (sectionSelector <$> sectionIds),
      spacer,
      hstack [
        spacer,
        liftHelpOverlay (model ^. toolTips) "file_window" $ button "📖"  OpenFS              `styleBasic`  [ textFont  "UI", textMiddle ],
        spacer,
        liftHelpOverlay (model ^. toolTips) "app_preferences" $ button "🆔"  OpenAppPrefs        `styleBasic`  [ textFont  "UI", textMiddle ],
        spacer,
        liftHelpOverlay (model ^. toolTips) "view_window" $ button "🔍"  OpenWindowViewing   `styleBasic`  [ textFont  "UI", textMiddle ],
        spacer,
        liftHelpOverlay (model ^. toolTips) "realm_preferences" $ button "🌑"  OpenRealmList       `styleBasic`  [ textFont  "UI", textMiddle ],
        spacer,
        liftHelpOverlay (model ^. toolTips) "controller_preferences" $ button "🎮"  OpenControllerPrefs `styleBasic` [ textFont   "UI", textMiddle ],
        spacer,
        liftHelpOverlay (model ^. toolTips) "export_preferences" $ button "🆑"  OpenExportPrefs     `styleBasic` [ textFont  "UI", textMiddle ],
        spacer,
        liftHelpOverlay (model ^. toolTips) "run_preferences" $ button "🎥"  OpenRunPrefs        `styleBasic` [ textFont  "UI", textMiddle ],
        spacer,
        liftHelpOverlay (model ^. toolTips) "multiplayer_preferences" $ button "⛳"  OpenMPPrefs         `styleBasic` [ textFont  "UI", textMiddle ], -- Multiplayer
        spacer,
        liftHelpOverlay (model ^. toolTips) "cameras" $ textDropdownS selectedCamera ["Camera0", "Edit/Add Cameras"],
        spacer,
        separator,
        spacer,
        liftHelpOverlay (model ^. toolTips) "renderer_2d" $ toggleButton "2D"   (if  (not $ model ^. threeDimensional) then twoDimensional   else handleFalse ) `nodeEnabled` (model ^. showMainRenderer) `styleBasic` [textMiddle ],
        spacer,
        liftHelpOverlay (model ^. toolTips) "renderer_3d" $ toggleButton "3D"   (if  (not $ model ^. twoDimensional)   then threeDimensional else handleFalse ) `nodeEnabled` (model ^. showMainRenderer) `styleBasic` [textMiddle ],
        spacer,
        liftHelpOverlay (model ^. toolTips) "ruby_scripting" $ button "#"     None               `styleBasic` [ textFont  "UI", textMiddle ],
        spacer,
        liftHelpOverlay (model ^. toolTips) "record_preferences" $ button "➿"   None                `styleBasic` [ textFont  "UI", textMiddle ],
        spacer
      ],
      vstack [
        -- put everything below this line in a Widget/Function
        vstack [
        spacer,
        separator,
        -- THE RENDERER WIDGET
        liftHelpOverlay (model ^. toolTips) "renderer_widget" $ scroll $ vstack [
            label  "📷0" `styleBasic` [textFont  "UI", textSize 10, textMiddle, textRight],
            rendererContext (model ^. threeDimensional),
            spacer --filler
          ] `styleBasic` [padding 10]  `nodeVisible` (model ^. showRenderer)
        ] `styleBasic` [{-bgColor (rgbHex "#00AF54"),-} sizeReqW $ fixedSize 640, sizeReqH $ fixedSize 300], -- this is mainContent
        -- RENDERER WIDGET END
        filler,
        separator,
        spacer,
        vstack [
          hstack [
            sideBarToggle,
            liftHelpOverlay (model ^. toolTips) "fs_search" $ searchForm,
            countLabel
            -- box_ [mergeRequired booksChanged] $ vscroll (vstack (bookRow wenv <$> model ^. books)) `nodeKey` "mainScroll"
          ],
          separator,
          hstack [
            -- scroll $ vstack (label "File System" : readProjectFileIndex (fromJust $ model ^. projectSession)) `styleBasic` [bgColor (rgbHex "#A6A6A6")],
            -- separator,
            liftHelpOverlay (model ^. toolTips) "file_gui" $ scroll $ vstack (spacer : (label "File GUI" `styleBasic` [textColor $ rgbHex "#000000"]) : hstack [(button "./" None `styleBasic` [textColor $ rgbHex "#000000"]){- goToRoot -}, spacer, (button "../" {- goToDir --takeDirectory -} None `styleBasic` [textColor $ rgbHex "#000000"]), filler] : vstack [spacer, separator, spacer] : generateProjectFileGUI (fromJust $ model ^. projectSession))  `styleBasic` [bgColor (rgbHex "#EFC88B")],--"#612B8A")],
            hstack [
              liftHelpOverlay (model ^. toolTips) "fs_mk_directory" $  button "+" None `styleBasic` [textRight, bgColor $ rgbHex "#006600"], -- createDirectory 
              liftHelpOverlay (model ^. toolTips) "fs_rm_directory" $  button "-" None `styleBasic` [textRight, bgColor $ rgbHex "#660000"]  -- removeDirectory 
            ]
          ],
          separator,
          spacer,
          hstack [
            spacer,
            liftHelpOverlay (model ^. toolTips) "refresh_fs" $ button "💥" RefreshProjectFilesTask `styleBasic` [ textFont  "UI", textMiddle, textLeft ],
            filler,
            liftHelpOverlay (model ^. toolTips) "run_dedicated_server" $ label "Run Dedicated Server",
            checkbox dedicatedServer,
            spacer,
            liftHelpOverlay (model ^. toolTips) "debug_on_play" $ label "Debug On Play",
            checkbox debugOnPlay,
            spacer,
            liftHelpOverlay (model ^. toolTips) "camera_view" $ label "📷" `styleBasic` [ textFont  "UI", textMiddle ],
            checkbox rendererEnabled,
            spacer,
            liftHelpOverlay (model ^. toolTips) "tooltips" $  label "Tooltips",
            checkbox toolTips,
            spacer
          ],
          spacer,
          searchOverlay `nodeVisible` model ^. searching
        ]  `nodeVisible` (model ^. showFileSystem)
      ] `nodeVisible` (model ^. showMainRenderer),
      controllerWidgetTree  `nodeVisible` (model ^. showControllerSetup),
      setPlatformWidgetTree `nodeVisible` (model ^. showPlatform),
      cloudSetupTree        `nodeVisible` (model ^. showCloudSetup),
      appProjectPrefsTree   `nodeVisible` (model ^. showAppProjectPrefs),
      appRendererTree       `nodeVisible` (model ^. showRendererOptions),
      appRealmDBTree        `nodeVisible` (model ^. showAppRealmDB),
      appRealmViewerTree    `nodeVisible` (model ^. showRealmViewer) 
    ],

    --FileSystem Menu
    menuDialog_ (vstack $ intersperse spacer [
    label "Files",
    button "Rename Project"   (ConfirmParentEvt OpenFS),
    button "Save"   (ConfirmParentEvt OpenFS),
    button "Load"   (ConfirmParentEvt OpenFS),
    button "Import" (ConfirmParentEvt OpenFS),
    button "Export" (ConfirmParentEvt OpenFS)
    ]) OpenFS OpenFS `nodeVisible` (model ^. fileMenu),

    -- Application Preferences
    menuDialog_ (vstack $ intersperse spacer [
    label "Application Preferences",
    button "Project"       (ConfirmParentEvt OpenAppPrefs),
    --button "Assets"        (ConfirmParentEvt OpenAppPrefs),
    --button "Shaders"       (ConfirmParentEvt OpenAppPrefs),
    button "Renderer"      (ConfirmParentEvt OpenAppPrefs),
    button "Script Engine" (ConfirmParentEvt OpenAppPrefs),
    button "Network"       (ConfirmParentEvt OpenAppPrefs),
    button "Connected IDE" (ConfirmParentEvt OpenAppPrefs)
    ]) OpenAppPrefs OpenAppPrefs `nodeVisible` (model ^. applicationPreferences),

    -- Realm List (Number of Stages/Scenes included in Game)
    menuDialog_ (vstack $ intersperse spacer [
      label "Realms",
      scroll $ vstack appRealmViewerList,
      -- [
      --   button "default"  (ConfirmParentEvt OpenRealmList) -- Replace for a ALens' BookEvt [Realm], also add data Realm
      -- ],
      hstack [
        button "Add"      (ConfirmParentEvt AddRealm),--OpenRealmList),
        spacer,
        button "Remove"   (ConfirmParentEvt RemoveRealm),--OpenRealmList),
        spacer,
        button "Edit Project Realm Database"   (ConfirmParentEvt OpenRealmDB)
      ]
    ]) OpenRealmList OpenRealmList `nodeVisible` (model ^. realmList),

    -- Controller Preferences
    menuDialog_ (vstack $ intersperse spacer [
      label  "Controller Preferences",
      -- Elaborate Widget for Controllers (Xbox vs Playstation vs 3rd Party) goes here
      button "Controller Setup"           (ConfirmParentEvt OpenControllerSetup),
      button "Import Button Assignment"   (ConfirmParentEvt OpenControllerPrefs),
      button "Export Profile"             (ConfirmParentEvt OpenControllerPrefs),
      button "Export Button Assignment"   (ConfirmParentEvt OpenControllerPrefs)
    ]) OpenControllerPrefs OpenControllerPrefs `nodeVisible` (model ^. controllerPreferences),
    -- box_ [alignTop, onClick OpenFS] closeIcon

    -- Export Preferences
    menuDialog_ (vstack $ intersperse spacer [
    label "Export Preferences",
    button "Select Platform and/or Set External Deps" (ConfirmParentEvt OpenExportPrefs),
    hstack [
      label "Multiplayer",
      checkbox multiplayerEnabled,
      spacer,
      label "Optimize Shaders for Platform",
      checkbox optimizeShaders,
      spacer,
      label "Optimize For Web (WASM, WebGL)",
      checkbox optimizeForWeb
    ],
    button  "Revert to Defaults" (ConfirmParentEvt OpenExportPrefs)
    ]) OpenExportPrefs OpenExportPrefs `nodeVisible` (model ^. exportPreferences),

    -- Run Preferences
    menuDialog_ (vstack $ intersperse spacer [
    label         "Run Preferences",
    toggleButton  "Run Current Scene" runCurrentScene,
    hstack [
      label "Run Selectend Scene",
      spacer,
      textDropdownS runNamedScene ["default"]
    ],
    toggleButton  "Disable ScriptEngine Execution"     disableScriptEngineExecution,
    toggleButton  "Legacy Render Pipeline Only (OpenGL 3.2)" legacyRenderPipelineOnly
    ]) OpenRunPrefs OpenRunPrefs `nodeVisible` (model ^. runPreferences),

  -- Window Manager
    menuDialog_ (vstack $ intersperse spacer [
      label         "View Window",
      toggleButton  "MainWindow (Both File System and Renderer)"            showRenderer,
      toggleButton  "MainWindow - showFileSystem"                           showFileSystem, 
      toggleButton  "MainWindow - showRendererWidget"                       showMainRenderer,
      toggleButton  "ControllerSetupWindow" showControllerSetup,
      toggleButton  "BuildSettingsWindow"   showPlatform,--showRenderer,
      toggleButton  "ProjectSetupWindow"    showAppProjectPrefs, --showControllerSetup
      toggleButton  "RendererSetupWindow"   showRendererOptions, --showControllerSetup
      toggleButton  "RealmDBWindow"         showAppRealmDB, --showControllerSetup
      toggleButton  "CloudSetupWindow"      showCloudSetup --showControllerSetup
    ]) OpenWindowViewing OpenWindowViewing `nodeVisible` (model ^. showWindowViewer),

    -- Multiplayer Preferences
    menuDialog_ (vstack $ intersperse spacer [
      label         "Multiplayer Preferences",
      button        "Push Realm to Cloud"                          (ConfirmParentEvt OpenMPPrefs),
      toggleButton  "Make Realm Searchable"                        searchableRealm,
      toggleButton  "Private Realm Only (Singleplayer/Co-Op Only)" privateRealmOnly,
      hstack [label "Realm Visibility", textDropdownS realmVisibility  ["Public", "Private", "Protected(Password)", "Protected (Whitelist)"] ]
      ]) OpenMPPrefs OpenMPPrefs `nodeVisible` (model ^. mPPreferences),
      animFadeIn sideBar  `nodeVisible` (model ^. sideBarVisible) `nodeKey` "fadeTimeLabel",
      errorOverlay        `nodeVisible` isJust (model ^. errorMsg),
      bookOverlay         `nodeVisible` isJust (model ^. selected),
      editControllerButtonLayer `nodeVisible` isControllerButtonEditing,
      editBuildLayer            `nodeVisible` isBuildEditing,
      editCloudLayer            `nodeVisible` isCloudEditing
    ] <> confirmDeleteLayerCB) <> confirmDeleteLayerBU) <> confirmDeleteLayerCL)

buildProjectSession :: BooksModel -> String -> IO ProjectSession
buildProjectSession model projPath =  do
  projDir <- listDirectory  projPath
  let pSession = fromJust $ model ^. projectSession
  return $ ProjectSession (_projectName pSession) (map (\x -> ProjectSessionFile (takeBaseName x) $ matchExtType x) projDir) (_projectRealms pSession) (_projectPreferences pSession)

rowScrollIndex :: BooksModel -> S.Seq (Spider, Int) -> Maybe Int
rowScrollIndex model items = snd <$> S.lookup (model ^. rowToScrollTo) items

handleEvent :: Sess.Session
  -> WidgetEnv  BooksModel BooksEvt
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
    Task $ searchDirectory (T.unpack $ model ^. query) --searchBooks sess (model ^. query)
    ]
  AppSearchDir result -> [
    Message "mainScroll" ScrollReset,
    Model $ model
      & searching .~ False
      & errorMsg .~ Nothing
      & filePaths .~ result
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
  OpenFS                      -> [Model $ model & fileMenu .~ not (model ^. fileMenu)]
  OpenAppPrefs                -> [Model $ model & applicationPreferences .~ not (model ^. applicationPreferences)]
  OpenRealmList               -> [Model $ model & realmList .~ not (model ^. realmList)]
  OpenControllerPrefs         -> [Model $ model & controllerPreferences .~ not (model ^. controllerPreferences)]
  OpenExportPrefs             -> [Model $ model & exportPreferences .~ not (model ^. exportPreferences)]
  OpenRunPrefs                -> [Model $ model & runPreferences .~ not (model ^. runPreferences)]
  OpenMPPrefs                 -> [Model $ model & mPPreferences  .~ not (model ^. mPPreferences )]
  RefreshProjectFiles projDir -> [Model $ model & projectSession ?~ ProjectSession (_projectName (fromJust $ model ^. projectSession)) (map (\x -> ProjectSessionFile (takeBaseName x) $ matchExtType x) projDir) (_projectRealms (fromJust $ model ^. projectSession)) (_projectPreferences (fromJust $ model ^. projectSession))]
  RefreshProjectFilesTask     -> [Task  $ do
      dirCont <- listDirectory  "./assets/projects/default/" -- this will be formally addressed later
      print dirCont
      return $ RefreshProjectFiles dirCont
    ]
  OpenControllerSetup         -> [ Model $ model & showControllerSetup .~ not (model ^. showControllerSetup) ]
  OpenWindowViewing           -> [ Model $ model & showWindowViewer    .~ not (model ^. showWindowViewer)    ]
  None                        -> []
  NoneB   b                   -> []
  --LoadFile fp         -> [Task $ do
  -- filePath <- selectFolderDialog "Path to RENDER Source" (pack $ inlinePerformIO getHomeDirectory ++ "/Desktop/GitHub/manifest-v3/assets/images/")
  -- case filePath of
  --   Nothing  -> do
  --     notifyPopup "IOException" "Invalid File Path or Cancelled I/O Operation" Graphics.UI.TinyFileDialogs.Warning
  --     return $ DoNothing
  --] 
  BooksShowDetails book       -> [Model $ model & selected ?~ book   ]
  BooksCloseDetails           -> [Model $ model & selected .~ Nothing]
  BooksCloseError             -> [Model $ model & errorMsg .~ Nothing]
  SideBarSummon  bl           -> [Model $ model & sideBarVisible .~ bl, Message "fadeTimeLabel" AnimationStart]
  OpenRealmDB                 -> [Model $ model & showAppRealmDB .~ not (model ^. showAppRealmDB) & showRenderer .~ not (model ^. showRenderer)]
  ShowSection section -> [
    Model $ model
      & activeSection .~ section
      -- Uncommenting this line causes the side bar to close when a section is selected
      -- & sideBarVisible .~ False
    ]
  -- sa
  ControllerButtonInit -> [SetFocusOnKey "controllerNew"]
  
  ControllerButtonNew  -> [
    Event ControllerButtonShowEdit,
    Model $ model
      & activeControllerBinding  .~ def
      & controllerBindings       .~ (model ^. controllerBindings  ++ [model ^. activeControllerBinding])
      & cBAction                 .~ ControllerButtonEditing (length (model ^. controllerBindings)),
    SetFocusOnKey "controllerDesc"
    ]

  ControllerButtonEdit idx td -> [
    Event ControllerButtonShowEdit,
    Model $ model
      & cBAction .~ ControllerButtonEditing idx
      & activeControllerBinding .~ td,
    SetFocusOnKey "controllerDesc"
    ]

  ControllerButtonAdd -> [
    Event ControllerButtonHideEdit,
    Model $ addNewControllerButton wenv model,
    SetFocusOnKey "controllerNew"
    ]

  ControllerButtonSave idx -> [
    Event ControllerButtonHideEdit,
    Model $ model & controllerBindings . ix idx .~ model ^. activeControllerBinding,
    SetFocusOnKey "controllerNew"
    ]

  ControllerButtonDeleteBegin idx todo -> [
    Model (model & cBAction .~ ControllerButtonConfirmingDelete idx todo)]

  ControllerButtonConfirmDelete idx todo -> [
    Model (model & cBAction .~ ControllerButtonNone),
    Message (WidgetKey (controllerRowKey todo)) AnimationStart]

  ControllerButtonCancelDelete -> [
    Model (model & cBAction .~ ControllerButtonNone)
    ]

  ControllerButtonDelete idx controllerBinding -> [
    Model $ model & controllerBindings .~ remove idx (model ^. controllerBindings),
    SetFocusOnKey "controllerNew"]

  ControllerButtonCancel -> [
    Event ControllerButtonHideEdit,
    Model $ model
      & activeControllerBinding .~ def,
    SetFocusOnKey "controllerNew"]

  ControllerButtonShowEdit -> [
    Message "animControllerButtonEditIn"  AnimationStart,
    Message "animControllerButtonEditOut" AnimationStop
    ]
  ControllerButtonHideEdit -> [
    Message "animControllerButtonEditIn"  AnimationStop,
    Message "animControllerButtonEditOut" AnimationStart
    ]
  ControllerButtonHideEditDone -> [
    Model $ model & cBAction .~ ControllerButtonNone
    ]

  --sb
  BuildListInit -> [SetFocusOnKey "buildNew"]

  BuildListNew  -> [
    Event BuildListShowEdit,
    Model $ model
      & activeBuildList .~ def
      & buildList       .~ (model ^. buildList  ++ [model ^. activeBuildList])
      & bUAction        .~ BuildEditing (length (model ^. buildList)),
    SetFocusOnKey "buildDesc"
    ]

  BuildListEdit idx td -> [
    Event BuildListShowEdit,
    Model $ model
      & bUAction     .~ BuildEditing idx
      & activeBuildList .~ td,
    SetFocusOnKey "buildDesc"
    ]

  BuildListAdd -> [
    Event BuildListHideEdit,
    Model $ addNewBuild wenv model,
    SetFocusOnKey "buildNew"
    ]

  BuildListSave idx -> [
    Event BuildListHideEdit,
    Model $ model
      & buildList . ix idx .~ model ^. activeBuildList,
    SetFocusOnKey "buildNew"
    ]

  BuildListDeleteBegin idx todo -> [
    Model (model & bUAction .~ BuildConfirmingDelete idx todo)]

  BuildListConfirmDelete idx build -> [
    Model (model & bUAction .~ BuildNone),
    Message (WidgetKey (buildRowKey build)) AnimationStart]
  BuildListCancelDelete -> [
    Model (model & bUAction .~ BuildNone)
    ]

  BuildListDelete idx build -> [
    Model $ model & buildList .~ remove idx (model ^. buildList),
    SetFocusOnKey "buildNew"]

  BuildListCancel -> [
    Event BuildListHideEdit,
    Model $ model
      & activeBuildList .~ def,
    SetFocusOnKey "buildNew"]

  BuildListShowEdit -> [
    Message "animBuildEditIn"  AnimationStart,
    Message "animBuildEditOut" AnimationStop
    ]

  BuildListHideEdit -> [
    Message "animBuildEditIn"  AnimationStop,
    Message "animBuildEditOut" AnimationStart
    ]

  BuildListHideEditDone -> [
    Model $ model & bUAction .~ BuildNone
    ]

  --sc
  CloudInit -> [SetFocusOnKey "cloudNew"]
  
  CloudNew  -> [
    Event CloudShowEdit,
    Model $ model
      & activeCloudList .~ def
      & cloudList       .~ (model ^. cloudList  ++ [model ^. activeCloudList])
      & cLAction        .~ CloudEditing (length (model ^. cloudList)),
    SetFocusOnKey "cloudDesc"
    ]

  CloudEdit idx td -> [
    Event CloudShowEdit,
    Model $ model
      & cLAction .~ CloudEditing idx
      & activeCloudList .~ td,
    SetFocusOnKey "cloudDesc"
    ]

  CloudAdd -> [
    Event CloudHideEdit,
    Model $ addNewCloud wenv model,
    SetFocusOnKey "cloudNew"
    ]

  CloudSave idx -> [
    Event CloudHideEdit,
    Model $ model
      & cloudList . ix idx .~ model ^. activeCloudList,
    SetFocusOnKey "cloudNew"
    ]

  CloudDeleteBegin idx todo -> [
    Model (model & cLAction .~ CloudConfirmingDelete idx todo)
    ]

  CloudConfirmDelete idx todo -> [
    Model (model & cLAction .~ CloudNone),
    Message (WidgetKey (cloudRowKey todo)) AnimationStart
    ]

  CloudCancelDelete -> [
    Model (model & cLAction .~ CloudNone)
    ]

  CloudDelete idx todo -> [
    Model $ model & cloudList .~ remove idx (model ^. cloudList),
    SetFocusOnKey "cloudNew"]

  CloudCancel -> [
    Event CloudHideEdit,
    Model $ model
      & activeCloudList .~ def,
    SetFocusOnKey "cloudNew"]

  CloudShowEdit -> [
    Message "animCloudEditIn"  AnimationStart,
    Message "animCloudEditOut" AnimationStop
    ]

  CloudHideEdit -> [
    Message "animCloudEditIn" AnimationStop,
    Message "animCloudEditOut" AnimationStart
    ]

  CloudHideEditDone -> [
    Model $ model & cLAction .~ CloudNone
    ]
  --end
  XboxController       -> [Model $ model & controllerType .~ XBOX_CONTROLLER & controllerBindings .~ []]
  PsController         -> [Model $ model & controllerType .~ PS_CONTROLLER   & controllerBindings .~ []]
  ThirdPartyController -> [Model $ model & controllerType .~ TP_CONTROLLER   & controllerBindings .~ []]
  -- Grid Events
  FeedSpider idx
    | Just spdr <- S.lookup idx (model ^. spiders) ->
        [ Producer (const (putStrLn ("Feeding spider " <> T.unpack (sName spdr)))),
          Model $ model & spiders .~ S.update idx (spdr {weightKilos = (weightKilos spdr) + 1}) (model ^. spiders)
        ]
  FeedSpider _ -> []
  AddSpider    -> [Model $ model & spiders .~ ((model ^. spiders) S.:|> newSpider model)]
  NameColumnResized colWidth ->
    [Producer (const (putStrLn ("Name column was resized: " <> show colWidth)))]
  NameColumnSorted direction ->
    [Producer (const (putStrLn ("Name column was sorted: " <> show direction)))]
  ScrollToOriginalIndex -> [scrollToRow (WidgetKey hagridKey) (rowScrollIndex model)]
  StartIDE -> [Task $ do
    startIDE (T.unpack $ model ^. ideArgument) 
    return None
    ]
  AddRealm        -> [Model $ model &   projectLoadedRealms .~ (model ^. projectLoadedRealms) ++ [Realm "new" "new.bsp" False]]
  RemoveRealm     -> [Model $ model &   projectLoadedRealms .~ (filter (\x -> x /= last (model ^. projectLoadedRealms)) $ model ^. projectLoadedRealms)]
  OpenRealm rlm   -> [Model $ model &   selectedRealm .~ rlm & showRealmViewer .~ True & showMainRenderer .~ False]
  UpdateRealmNameLocal        tx    -> [Model $ model & projectLoadedRealms . ix (getItemIndex (model ^. selectedRealm) (model ^. projectLoadedRealms)) . rlmLevelName   .~ tx]  
  UpdateRealmBSPNameLocal     tx    -> [Model $ model & projectLoadedRealms . ix (getItemIndex (model ^. selectedRealm) (model ^. projectLoadedRealms)) . rlmBsp         .~ tx]
  UpdateRealmMultiplayerLocal bl    -> [Model $ model & projectLoadedRealms . ix (getItemIndex (model ^. selectedRealm) (model ^. projectLoadedRealms)) . rlmMultiplayer .~ bl] 

initialModel :: BooksModel
initialModel = BooksModel {
  _bkmQuery     = "",
  _bmkSearching = False,
  _bkmErrorMsg  = Nothing,
  _bkmBooks     = [],
  _bkmFilePaths = [],
  _bmkSelected  = Nothing,
  _bmkToolTips  = False,
  _bmkMultiplayerEnabled     = False,
  _bmkOptimizeShaders        = False,
  _bmkOptimizeForWeb         = False,
  _bmkRendererEnabled        = False,
  _bmkDebugOnPlay            = False,
  _bmkDedicatedServer        = False,
  _bmkFileMenu               = False,
  _bmkApplicationPreferences = False,
  _bmkRealmList              = False,
  _bmkControllerPreferences  = False,
  _bmkExportPreferences      = False,
  _bmkRunPreferences         = False,
  _bmkMPPreferences          = False,
  _bmkProjectSession         = (Just $ ProjectSession "default" [] [] (ProjectSessionPrefs [] False)),
  _bmkSideBarVisible         = False,
  _bmkMaxSections            = 0,
  _bmkActiveSection          = 0,
  _bmkControllerBindings     = [],
  _bmkBuildList              = [],
  _bmkCloudList              = [],
  _bmkActiveControllerBinding  = def,
  _bmkActiveBuildList           = def,
  _bmkActiveCloudList           = def,
  _bmkCBAction                  = ControllerButtonNone,
  _bmkBUAction                  = BuildNone,
  _bmkCLAction                  = CloudNone,
  _bmkSearchableRealm  = False,
  _bmkPrivateRealmOnly = False,
  _bmkRealmVisibility  = "Public",
  _bmkSelectedCamera   = "Camera0",
  _bmkDisableScriptEngineExecution  = False,
  _bmkLegacyRenderPipelineOnly      = False,
  _bmkRunCurrentScene  = False,
  _bmkRunNamedScene    = "default",
  _bmkShowRenderer     = True,
  _bmkShowPlatform           = False,
  _bmkShowCloudSetup         = False,
  _bmkShowRendererOptions    = False,
  _bmkShowControllerSetup    = False,
  _bmkShowAppProjectPrefs    = False,
  _bmkShowAppRealmDB         = False,
  _bmkShowWindowViewer       = False,
  _bmkIdeArgument            = "",
  _bmkCurrentRenderer        = "OpenGL", 
  _bmkSpiders                = spiders,
  _bmkColumns                = (AppColumn True <$ gridColumns),
  _bmkRowToScrollTo          = 0,
  _bmkProjectTitleName       = "",
  _bmkCurrentRealmName       = "",
  _bmkControllerType         =  GENERIC_CONTROLLER,
  _bmkExternalDepsList       =  "",
  _bmkColorPicked            =  Color 0 0 0 1,
  _bmkWidgetSelected         =  "N/A",
  _bmkSpirVOnly              =  False,
  _bmkGlslOnly               =  False,
  _bmkColor1                 =  red,
  _bmkColor2                 =  green,
  _bmkColor3                 =  blue,
  _bmkColor4                 =  orange,
  _bmkShowFileSystem         =  True,
  _bmkShowMainRenderer       =  True,
  _bmkThreeDimensional       =  False,
  _bmkTwoDimensional         =  False,
  _bmkHandleFalse            =  False,
  _bmkProjectLoadedRealms    =  [Realm "default" "default.bsp" False],
  _bmkShowRealmViewer        =  False,
  _bmkSelectedRealm          =  Realm "" "" False
} where 
  spiders  = S.fromFunction numSpiders spider
  spider i =
    Spider
      { index       = i,
        species     = "Level",
        sName       = T.pack  (printf "default realm %d" (i + 1)),
        dateOfBirth = addDays (fromIntegral i) (fromGregorian 1942 3 1),
        weightKilos = fromIntegral (numSpiders + 2 - i) * 2.3
      }
  numSpiders = 5

main :: IO ()
main = do
  sess <- Sess.newAPISession
  startApp initModel (handleEvent sess) buildUI config
  where
    config = [
      appWindowTitle "WRayLib3d",
      appWindowIcon "./assets/images/iconL.png",
      appTheme mainTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Medium"  "./assets/fonts/Roboto-Medium.ttf",
      appFontDef "UI"      "./assets/fonts/og-dcm-emoji.ttf",
      appInitEvent BooksInit
      ]
    initModel = appModel

appModel :: BooksModel
appModel   = initialModel

customLightTheme :: Theme
customLightTheme = lightTheme
  & L.userColorMap . at "rowBgColor" ?~ rgbHex "#ECECEC"

customDarkTheme :: Theme
customDarkTheme = darkTheme
  & L.userColorMap . at "rowBgColor" ?~ rgbHex "#656565"

rerr  :: Maybe Text -> Text
rerr r  = fromMaybe "" r

rbook :: Maybe Book -> Book
rbook b = case b of
            Nothing -> Book "NULL" [] (Just 0) (Just 0)
            Just a  -> a

indexList :: [a] -> [(Int, a)]
indexList l = zip [1..] l

getItemIndex :: Realm -> [Realm] -> Int
getItemIndex rlm lst = fst $ head $ filter (\x -> rlm == snd x) $ indexList lst
-- dataAt :: Int -> [a] -> a
-- dataAt _ [] = error "Empty List!"
-- dataAt y (x:xs)  | y <= 0 = x
--                  | otherwise = dataAt (y-1) xs

newSpider :: BooksModel -> Spider
newSpider model =
  Spider { 
      index       = fromIntegral (length $ model ^. spiders),
      sName       = "Extra Spider " <> pack (show (length $ model ^. spiders)),
      species     = "Spider plant",
      dateOfBirth = fromGregorian 2022 6 26,
      weightKilos = 0.01
    }
    
-- Utility function to avoid the "Ambiguous type variable..." error
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch

remove :: Int -> [a] -> [a]
remove idx ls = take idx ls ++ drop (idx + 1) ls