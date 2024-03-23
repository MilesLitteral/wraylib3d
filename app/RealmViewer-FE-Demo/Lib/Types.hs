module Lib.Types where

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

appModel :: BooksModel
appModel   = initialModel

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

customLightTheme :: Theme
customLightTheme = lightTheme
  & L.userColorMap . at "rowBgColor" ?~ rgbHex "#ECECEC"

customDarkTheme :: Theme
customDarkTheme = darkTheme
  & L.userColorMap . at "rowBgColor" ?~ rgbHex "#656565"
  