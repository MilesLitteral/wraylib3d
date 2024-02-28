module HRayLib3d.Utils.Tooltips where

import Monomer 
import Data.Text
import Control.Lens 
import Data.Typeable

tooltipKeys :: [(String, Text)]
tooltipKeys =  [
  ("renderer",                pack "start renderer" ),
  ("test",                    pack "test")
  ]

getTooltipBody :: String -> [(String, Text)] -> Text
getTooltipBody key mp = snd $ (Prelude.filter (\x -> (fst x == key)) mp) !! 0

liftHelpOverlay :: (Typeable s, Typeable e) => Bool -> String -> WidgetNode s e -> WidgetNode s e
liftHelpOverlay mdl key widget = do
  case mdl of
    True  -> tooltip_ (getTooltipBody key tooltipKeys) [tooltipDelay 2,  tooltipFollow] (widget) `styleBasic` [textColor (Color 2 2 2 1), bgColor (Color 253 204 128 1)]
    False -> widget