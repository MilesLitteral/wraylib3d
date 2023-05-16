{-# LANGUAGE     RankNTypes #-}
{-# LANGUAGE     FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Manifest.Utils.Templating where

import Data.Text hiding (transpose)
import Data.Typeable
import Data.Maybe(fromMaybe, fromJust)
import Data.List (elemIndex, transpose)
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Vector.Storable hiding ((++), mapM_, elemIndex)

import System.Directory
import System.FilePath
import System.Process

import Control.Monad
import Control.Applicative
import Control.Lens.Combinators 
import Control.Monad.IO.Class

import Monomer
import Monomer.Widgets
import Monomer.Widgets.Single
import qualified Monomer.Lens as L
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as JSON
import qualified Data.Massiv.Array as A

import Manifest.Utils.Log 
import Manifest.Utils.Json
import Manifest.Utils.Conversion
import Manipipe.File.Manifile
import Manipipe.Futhark.Type

import Control.Lens
import Manifest.Types

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.UI.TinyFileDialogs

import Plots
import Linear.V2 
import Control.Lens
import Control.Monad.IO.Class

import Codec.Picture
import Codec.Picture.Types
import Codec.Picture (DynamicImage, PixelRGB8, writePng, savePngImage, convertRGBA8, convertRGB8, imageData, generateImage, readImage, decodeImage, writeTiff)

import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Backend.Rasterific
import Diagrams.Size 

-- Generics
foreach :: [t] -> (t -> a) -> [a]
foreach [] f = []
foreach (x:[]) f = (f x):[]

merge :: [[a]] -> [a]
merge = Prelude.concat . transpose

forN :: Int -> (IO()) -> IO()
forN 0 _ = return ()
forN n f = do
  f
  forN (n - 1) f

map2 :: (a1 -> a2) -> (a1 -> b) -> [a1] -> [(a2, b)]
map2 f1 f2  = Prelude.map (liftA2 (,) f1 f2)

nullIO :: IO ()
nullIO =  putStrLn "n/a"

--RGB8 -> RGBF
ez8toF :: Image PixelRGB8 -> Image PixelRGBF
ez8toF = pixelMap $ \(PixelRGB8 r g b ) ->  promotePixel $ PixelRGBF (promotePixel  r) (promotePixel g) (promotePixel  b)

getImage :: Either a DynamicImage -> Image PixelRGB8
getImage ids = do
    case ids of
      Left err    -> error "\x1b[31m╚(🈲)Not An Image"
      Right image -> convertRGB8 image

packDiff :: ManifestModel -> Text
packDiff mdl = pack $ (unpack $ (mdl ^. experimentPath)) ++ ("test")

chartPath :: ManifestModel -> Text
chartPath model = pack $ (unpack (model ^. experimentPath)) ++ "chart.png"

allImages :: FilePath -> IO [Text]
allImages imgPath = do
  all <- getDirectoryContents imgPath --"./assets/datasets/OldHead/Seq"
  let filtered = Prelude.filter (isSuffixOf ".png") (Prelude.map pack all)
  return filtered

elemIndex' :: Eq a => a -> [a] -> Int
elemIndex' x = fromMaybe (-1) . elemIndex x

createElement :: Typeable e => Text -> e -> Bool -> Bool -> WidgetNode s e
createElement title evt main visible = if main == False then button title evt `nodeVisible` visible else mainButton title evt `nodeVisible` visible

-- This Function needs to be Upgraded 
createHGrid :: Text -> ManifestEvt -> Text -> Text -> Text -> WidgetNode s ManifestEvt
createHGrid mainTitle mainEvt toggleATitle toggleBTitle toggleCTitle = hgrid [
        Monomer.Widgets.spacer,
        Monomer.Widgets.mainButton mainTitle mainEvt,
        Monomer.Widgets.spacer,
        Monomer.Widgets.button toggleATitle   DoNothing,
        Monomer.Widgets.spacer,
        Monomer.Widgets.button toggleBTitle   DoNothing,
        Monomer.Widgets.button toggleCTitle   DoNothing,
        Monomer.Widgets.spacer
      ]

prepareImage :: ManifestModel -> [Char] -> Text
prepareImage mdl file = do
  let xr = unpack (mdl ^. experimentPath)
  let b = xr ++ "Seq/" ++ file ++ ".png"
  pack b

loadIntake mdl = do
  input <- BL.readFile ((unpack $ mdl ^. experimentPath) ++ "Intake.json")
  let mm = JSON.decode input 
  orderedMessage $ ManiLogMessage MANI_LOG_HEAD MANI_LOG_ZONE "Enter Intake Zone"
  orderedMessage $ ManiLogMessage MANI_LOG_BODY MANI_LOG_DEBUG ("Intake Contents (Raw JSON): " ++ (show input)) 

  case mm of
    Nothing ->  do 
      exceptionMessage $ ManiLogMessage MANI_LOG_BODY MANI_LOG_ERROR ("Error Parsing Intake File")  
      return $ (SetAnnotationText [])

    Just m  ->  do  
      return $ (SetAnnotationText (intakeSlices m)) --Model (mdl & availableAnnotations .~ (intakeSlices m)) --

-- defaultImage :: BytesImage
-- defaultImage = do
--   let v :: [Word8] = take (512 * 512 * 4) [255, 255..]
--   BytesImage "Default"    (BS.pack v) (Size 512 512) 

templateAnnotation :: Text -> [WidgetNode ManifestModel ManifestEvt]
templateAnnotation img = [
    image img
    -- checkboxV False AnnotationSelected
    ]

pushed :: Text
pushed = "pressed" -- Debug function for Testing Buttons and Element Interaction

boolToString :: Bool -> String
boolToString True  = "TRUE"
boolToString False = "FALSE"

seekImage :: RealFrac a => ManifestModel -> a -> String
seekImage mdl value = (unpack (mdl ^. experimentPath)) ++ "Seq/" ++ (\x -> Prelude.replicate (4 - Prelude.length x) '0' ++ x) (show $ floor $ value) ++ ".png"

seekDifference :: RealFrac a => Text -> Text -> [Char] -> a -> IO ()
seekDifference ta tb path value = do
  let rx = path ++ "Diffs/" ++ (\x -> Prelude.replicate (4 - Prelude.length x) '0' ++ x) (show $ floor $ value) ++ ".png"
  dynamicDiff (unpack ta) (unpack tb) (rx)

--Monomer Style: Alerts, Warnings, Confirmations
-- Descriptor -> Event
confirmationDialog :: (Typeable s, Typeable e) => e -> WidgetNode () e -> WidgetNode s e
confirmationDialog d e = alert d e

draggableDialog :: (Eq a, Typeable a, Typeable s, Typeable e) => a -> e -> WidgetNode () e -> WidgetNode s e
draggableDialog msg d e = draggable msg (alert d e)

ddWidget :: (Eq a, Typeable a, Typeable s, Typeable e) => WidgetNode s e -> (a -> e) -> a -> WidgetNode s e
ddWidget widget dropEvt dragMsg = dropTarget (dropEvt) $ draggable (dragMsg) $ widget

customDialog :: (Typeable s, Typeable e) => e -> [AlertCfg] -> WidgetNode () e -> WidgetNode s e
customDialog e c d = alert_ e c d

addTooltip :: (Typeable s, Typeable e) => Text -> Millisecond -> WidgetNode s e -> WidgetNode s e 
addTooltip body delay widget = tooltip_ body [tooltipDelay delay, tooltipFollow] $ widget

imageButton :: (Typeable s, Typeable e) => e -> Text -> String -> Color -> WidgetNode s e
imageButton evt imgPath bgCol brCol = box_ [onClick $ evt ]  $ image_ imgPath [alignCenter, fitFill] `styleBasic` [width 25, height 25,  (bgColor (rgbHex $ bgCol)), (border 2 (brCol))]

imageIcon :: (Typeable s, Typeable e) => Text -> Color -> Color -> WidgetNode s e
imageIcon imgPath bgCol brCol = image_ imgPath [alignCenter, fitFill] `styleBasic` [width 25, height 25,  (bgColor bgCol), (border 2 (brCol))]

{-

TBA: AppEnv Variable that holds the important stuff independent of $PATH
You could also simplify handle initialization with AppEnv's associated Pattern

newtype AppEnv = AppEnv {
  _dbServerCtx :: DBServerCtx,
}

handleEvent appEnv wenv node model evt = case evt of
   -- I can access dbServerCtx where needed
   ...

buildUI appEnv wenv model = widgetTree where
   -- A child widget may need to receive it as a parameter; otherwise, it can be omitted
   ...

main :: IO ()
main = do
    -- Create any computationally expensive that requires one-time initialization
    dbCtx <- makeDbCtx
    let appEnv = AppEnv dbCtx
    startApp model (handleEvent appEnv) (buildUI appEnv) config
  where
    config = []
    model = ...

-}

--SetMLBackend "PYT"

--make a generalize version, perhaps ALens a => a -> String -> WidgetNode

-- tooltipKeys :: [(String, Text)]
-- tooltipKeys =  [
--   ("ansible",  pack "start ansible"), 
--   ("refresh",  pack "Pull Preview images from connected Camera(s)"), 
--   ("download", pack "Download Preview Images"),
--   ("reel",     pack "Images And Previews from Cameras"), 
--   ("test",     pack "test")
--   ]

-- getTooltipBody :: String -> [(String, Text)] -> Text
-- getTooltipBody key mp = snd $ (Prelude.filter (\x -> (fst x == key)) mp) !! 0

-- liftHelpOverlay :: TodoModel -> String -> WidgetNode s e -> WidgetNode s e 
-- liftHelpOverlay mdl key widget = do
--   case (mdl ^. helpOverlaysEnabled) of
--     True  -> tooltip_ (getTooltipBody key tooltipKeys) [tooltipDelay 2, tooltipFollow] $ widget
--     False -> widget

-- createTootipWithStyle :: Text -> [TooltipCfg] -> WidgetNode s e -> WidgetNode s e 
-- createTooTipWithStyle = tooltip_

-- --OS specific File Dialogs
-- osSaveFileDialog :: Text -> Text	-> [Text]	-> Text	-> IO (Maybe Text)	
-- osSaveFileDialog = saveFileDialog

-- osSelectFileDialog :: Text -> Text	-> [Text]	-> Text	-> Bool	-> IO (Maybe [Text])	
-- osSelectFileDialog = openFileDialog

-- osSelectFolderDialog :: Text	-> Text	-> IO (Maybe Text)	
-- osSelectFolderDialog = selectFolderDialog

--Revisit when you want to expand significantly on Intake Annotation Exporting
--createDatasetFolder =
--appendDatasetFolderContent



{- 
  evt:ManimerEvt configs:AlertCfg dialogBody:Text
  --
  -- data AlertCfg = AlertCfg {
  --   _alcTitle :: Maybe Text,
  --   _alcClose :: Maybe Text
  -- } 
-}

-- heatMapCustomRender sizeX sizeY =
--   let f (V2 x y) = fromIntegral x + fromIntegral y
--       myHM       = mkHeatMatrix (V2 5 5) f
--   in  pixelHeatRender myHM Plots.magma

-- Dont delete, will be useful for dropdown menu creation in the future
-- seedList :: [Maybe Float]
-- seedList = Nothing : (Just <$> [0.1,0.2..1.0])

-- seedDesc :: Maybe Float -> Text
-- seedDesc Nothing  = "Random"
-- seedDesc (Just v) = pack (show v)

-- codeList :: [ManimerCodeBackend]
-- codeList   = [Haskell, Python]

-- codeDesc :: ManimerCodeBackend -> Text
-- codeDesc v = pack $ show v

-- genTypeDesc :: ManimerType -> Text
-- genTypeDesc Default   = "Maniga"
-- Past this point is errata 