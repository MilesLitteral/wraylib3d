module HRayLib3d.Utils.Tooltips where

import Monomer 
import Data.Text
import Control.Lens 
import Data.Typeable

tooltipKeys :: [(String, Text)]
tooltipKeys =  [
  ("file_window",                pack "Access various I/O options like `Open`,\n `Save, as well as asset import and export options."),
  ("view_window",                pack "Toggle different Editor interfaces; such as controller and export preferences."),
  ("ruby_scripting",             pack "Opens an associated IDE for Ruby Component scripting (by default)\n You can toggle Scripting Engine environments in the Editor Preferences."),
  ("renderer_2d",                pack "start renderer (2D)\n. the scene will open in Editor."),
  ("renderer_3d",                pack "start renderer (3D)\n Be sure to check your ide to input the desired level/scene to run \n Game will then open a separate window"),
  ("refresh_fs",                 pack "reload file system interface"),
  ("fs_search",                  pack "Search for a file based on it's file name or suffix,\n Full filename is also acceptable."),
  ("fs_mk_directory",            pack "Add file"),
  ("fs_rm_directory",            pack "Remove one file"),
  ("fs_up_directory",            pack "Go up one directory"),
  ("fs_up_two_directory",        pack "Go back to two directories (usually back to root)"),
  ("app_preferences",            pack "test"),
  ("realm_preferences",          pack "test"),
  ("controller_preferences",     pack "test"),
  ("export_preferences",         pack "test"),
  ("multiplayer_preferences",    pack "test"),
  ("run_preferences",            pack "test"),
  ("record_preferences",         pack "test"),
  ("controller_add_button",      pack "Add/Edit Button Def to/in Controller Scheme"),
  ("controller_delete_button",   pack "Remove Button Def to Controller Scheme"),
  ("cloud_add_button",           pack "Add/Edit Button Def to/in Controller Scheme"),
  ("cloud_delete_button",        pack "Remove Button Def to Controller Scheme"),

  ("build_list_edit_button",     pack "Add/Edit Button Def to/in Controller Scheme"),
  ("build_list_delete_button",   pack "Remove Button Def to Controller Scheme"),
  ("renderer_widget",            pack "test"),
  ("file_gui",                   pack "test"),
  ("run_dedicated_server",       pack "Will initialize a joinable server instance\n this allows for co-coding, but can also be used to run a live testable game session\n or multiplayer."),
  ("camera_view",                pack "Hide all editor widgets (including the FileSystem)\n except for the rendering widget"),
  ("debug_on_play",              pack "Run Debugging and Logging window instance alongside play.\n Game will run in Debug mode."),
  ("tooltips",                   pack "Show tooltips."),
  ("cameras",                    pack "Add camera(s) to the scene,\n allows you to switch between views also"),
  ("toggle_inspector",           pack "View objects in scene and toggle their visibility."),
  ("inspector_object",           pack "A Scene Object, optional actions are available\n such as visibility and copy & paste behavior."),
  ("test",                       pack "test")
  ]

getTooltipBody :: String -> [(String, Text)] -> Text
getTooltipBody key mp = snd $ (Prelude.filter (\x -> (fst x == key)) mp) !! 0

liftHelpOverlay' :: (Typeable s, Typeable e) => String -> WidgetNode s e -> WidgetNode s e
liftHelpOverlay' key widget = tooltip_ (getTooltipBody key tooltipKeys) [tooltipDelay 2,  tooltipFollow] (widget) `styleBasic` [textColor (Color 2 2 2 1), bgColor (Color 253 204 128 1)]


liftHelpOverlay :: (Typeable s, Typeable e) => Bool -> String -> WidgetNode s e -> WidgetNode s e
liftHelpOverlay mdl key widget = do
  case mdl of
    True  -> tooltip_ (getTooltipBody key tooltipKeys) [tooltipDelay 2,  tooltipFollow] (widget) `styleBasic` [textColor (Color 2 2 2 1), bgColor (Color 253 204 128 1)]
    False -> widget