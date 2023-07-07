module HRayLib3d.WindowSystem.Theme where 

import Monomer      ( Color, Theme, black, rgbHex, transparent )
import Control.Lens ((&), (.~))
import Monomer.Core.Themes.BaseTheme ( BaseThemeColors(..), baseTheme )
import qualified Monomer.Lens as L

-- | Dark theme provided by the Manifest library.
mainTheme :: Theme
mainTheme = baseTheme mainThemeColors

-- | Colors for the dark theme.
mainThemeColors :: BaseThemeColors
mainThemeColors = BaseThemeColors {
  clearColor           = rgbHex "#414141", --"#DEE0E2",
  sectionColor         = rgbHex "#7C7C7C", --"#C8CBC6",

  btnFocusBorder       = rgbHex "#FDCC80",
  btnBgBasic           = rgbHex "#CF5C36",
  btnBgHover           = rgbHex "#EEE5E9",
  btnBgFocus           = rgbHex "#8DACCE",
  btnBgActive          = rgbHex "#EEE5E9",
  btnBgDisabled        = gray05,
  shadow               = gray05,
  btnText              = white02,
  btnTextDisabled      = gray01,
  btnMainFocusBorder   = rgbHex "#FDCC80",
  btnMainBgBasic       = rgbHex "#CF5C36",--blue05b,
  btnMainBgHover       = blue05, --blue06,
  btnMainBgFocus       = blue05c,
  btnMainBgActive      = rgbHex "#EEE5E9",  
  btnMainBgDisabled    = blue04,
  btnMainText          = white02,
  btnMainTextDisabled  = gray08,

  dialogBg             = rgbHex  "#000000",
  dialogBorder         = gray01,
  dialogText           = white02,
  dialogTitleText      = white02,
  emptyOverlay         = gray05 & L.a .~ 0.8,

  externalLinkBasic    = rgbHex "#CF5C36",
  externalLinkHover    = blue08,
  externalLinkFocus    = blue07,
  externalLinkActive   = rgbHex "#EEE5E9",--blue06,
  externalLinkDisabled = gray06,

  iconBg = gray08,
  iconFg = gray01,

  inputIconFg       = black,
  inputBorder       = gray02,
  inputFocusBorder  = blue08,

  inputBgBasic      = rgbHex "CF5C36",
  inputBgHover      = rgbHex "#7B96FF",
  inputBgFocus      = rgbHex "#7B96FF",
  inputBgActive     = blue06,
  inputBgDisabled   = gray07, 

  inputFgBasic      = rgbHex "#CF5C36",
  inputFgHover      = blue08,
  inputFgFocus      = blue08,
  inputFgActive     = blue06,
  inputFgDisabled   = gray07,

  inputSndBasic     = rgbHex "#CF5C36",
  inputSndHover     = gray06,
  inputSndFocus     = gray05,
  inputSndActive    = blue06,
  inputSndDisabled  = gray03,

  inputHlBasic      = rgbHex "#CF5C36",
  inputHlHover      = blue08,
  inputHlFocus      = blue08,
  inputHlActive     = blue06,
  inputHlDisabled   = gray08,

  inputSelBasic     = gray06,
  inputSelFocus     = blue06,

  inputText         = rgbHex  "#000000",
  inputTextDisabled = gray02,
  labelText         = white02,

  scrollBarBasic    = gray01 & L.a .~ 0.2,
  scrollThumbBasic  = gray07 & L.a .~ 0.6,
  scrollBarHover    = gray01 & L.a .~ 0.4,
  scrollThumbHover  = gray07 & L.a .~ 0.8,

  slMainBg            = rgbHex  "#000000",
  slNormalBgBasic     = transparent,
  slNormalBgHover     = gray05,
  slNormalText        = white02,
  slNormalFocusBorder = blue08,

  slSelectedBgBasic =  rgbHex "#CF5C36",
  slSelectedBgHover = gray05,
  slSelectedText    = white02,
  slSelectedFocusBorder = blue08,

  tooltipBorder = gray05,
  tooltipBg     = rgbHex "#1D212B",
  tooltipText   = rgbHex  "#000000"
}

white02 :: Color
white02 = rgbHex "#EEE5E9"

blue01 :: Color
blue01  = rgbHex "#002159"

blue02 :: Color
blue02  = rgbHex "#01337D"

blue03 :: Color
blue03  = rgbHex "#03449E"

blue04 :: Color
blue04  = rgbHex "#0552B5"

blue05 :: Color
blue05  = rgbHex "#0967D2"

blue05b :: Color
blue05b = rgbHex "#0F6BD7"

blue05c :: Color
blue05c = rgbHex "#1673DE"

blue06 :: Color
blue06  = rgbHex "#2186EB"

blue06b :: Color
blue06b = rgbHex "#2489EE"

blue06c :: Color
blue06c = rgbHex "#2B8FF6"

blue07 :: Color
blue07  = rgbHex "#47A3F3"

blue07b :: Color
blue07b = rgbHex "#50A6F6"

blue07c :: Color
blue07c = rgbHex "#57ACFC"

blue08 :: Color
blue08  = rgbHex "#7CC4FA"

blue09 :: Color
blue09  = rgbHex "#BAE3FF"

blue10 :: Color
blue10  = rgbHex "#E6F6FF"

gray00 :: Color
gray00  = rgbHex "#222222"

gray01 :: Color
gray01  = rgbHex "#2E2E2E"

gray02 :: Color
gray02  = rgbHex "#393939"

gray03 :: Color
gray03  = rgbHex "#515151"

gray04 :: Color
gray04  = rgbHex "#626262"

gray05 :: Color
gray05  = rgbHex "#7E7E7E"

gray06 :: Color
gray06  = rgbHex "#9E9E9E"

gray07 :: Color
gray07  = rgbHex "#B1B1B1"

gray07b :: Color
gray07b = rgbHex "#B4B4B4"

gray07c :: Color
gray07c = rgbHex "#BBBBBB"

gray08 :: Color
gray08  = rgbHex "#CFCFCF"

gray09 :: Color
gray09  = rgbHex "#E1E1E1"

gray10 :: Color
gray10  = rgbHex "#F7F7F7"
