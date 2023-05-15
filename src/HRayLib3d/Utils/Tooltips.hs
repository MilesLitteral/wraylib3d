module HRayLib3d.Utils.Tooltips where

import Monomer 
import Data.Text
import Control.Lens 
import Data.Typeable

tooltipKeys :: [(String, Text)]
tooltipKeys =  [
  ("manieye_sv_graph",                pack "Takes a ManiFile Source (a folder that contains a test_sv.mani and train_sv.mani),\n or a Generator (TBA)" ),
  ("manieye_image_selector",          pack "View Images from Scene Vectors via On The Fly rendering (Point to the GroundTruths folder of the project in question)\n or point to a Image (Folder) source (such as /Seq/)\n *WARNING: SV Source is Disabled until maniapi/local_batch is merged into maniapi/manifest as these branches are incompatible currently"),
  ("manieye_playback",                pack "Begin playback of loaded images and or graphs,\n It is suggested you run through the loaded images and graph atleast once with playback to allow for caching of the dataset\n then run it back to the beginning and begin screen capture"),
  ("manieye_step_forward",            pack "Step forward  in index by one"),
  ("manieye_step_backward",           pack "Step backward in index by one"),
  ("manieye_timeline",                pack "Scrum through loaded images and or graphs\n also denotes index within loaded datasets"),
  ("manieye_add_widget_dropdown",     pack "Select a Widget of defined Type to create, widget types consist of:\n Image Selectors\n SVGraphs\n new widgets will appear over time here"),
  ("manieye_add_widget",              pack "Add a Widget to the viewing area a dialogue will appear when this button is clicked,\n click again to cancel the dialogue"),
  ("test",                            pack "test")
  ]

getTooltipBody :: String -> [(String, Text)] -> Text
getTooltipBody key mp = snd $ (Prelude.filter (\x -> (fst x == key)) mp) !! 0

liftHelpOverlay :: (Typeable s, Typeable e) => Bool -> String -> WidgetNode s e -> WidgetNode s e
liftHelpOverlay mdl key widget = do
  case mdl of
    True  -> tooltip_ (getTooltipBody key tooltipKeys) [tooltipDelay 2,  tooltipFollow] (widget) `styleBasic` [textColor (Color 2 2 2 1), bgColor (Color 253 204 128 1)]
    False -> widget