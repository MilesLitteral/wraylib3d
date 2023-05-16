module Manifest.Utils.Tooltips where

import Monomer 
import Data.Text
import Control.Lens 
import Data.Typeable
import Manifest.Utils.Prisms
import Manifest.Types
import qualified Data.Text as T

tooltipKeys :: [(String, Text)]
tooltipKeys =  [
  ("maniex",                            pack "All related Maniex tools and mechanics are here"),
  ("maniex_pyt",                        pack "Set M/L Backend to Pytorch"),
  ("maniex_tf",                         pack "Set M/L Backend to Tensorflow"),
  ("maniex_jax",                        pack "Set M/L Backend to JAX"),
  ("maniex_ds_name_override",           pack "Override the set (in YAML) Dataset Name,      this does not modify the value in the YAML"),
  ("maniex_ds_dir_override",            pack "Override the set (in YAML) Dataset Directory, this does not modify the value in the YAML"),
  ("maniex_output_dir_override",        pack "Override the set (in YAML) Output  Directory, this does not modify the value in the YAML"),
  ("maniex_configure_maniex",           pack "Modify the YAML File directly in VS Code"),
  ("maniex_reboot_maniapi",             pack "Restart ManiAPI systemctl daemon"),
  ("maniex_run",                        pack "Run Maniex with current settings"),
  ("maniex_run_tensorboard",            pack "Run Tensorboard against the current Experiment"), -- Becareful the Tensorboard feature may have broke 
  ("maniex_install_path",               pack "The literal path to the desired version of Maniex you wish to install to ManiAPI (eg: /home/usr/maniex) "),
  ("maniex_execute_install",            pack "Install   Maniex (by Path) to ManiAPI"),
  ("maniex_clear_install",              pack "Uninstall Maniex from ManiAPI"),

  ("manieye",                           pack "All related Experiment Analysis tools (Manieye)"),
  ("manieye_selected_experiment",       pack "Selected Experiment In Dataset (contents of eg: /home/usr/<dataset_path>/Output/)"),
  ("manieye_selected_run",              pack "Selected Run In Experiment     (contents of eg: /home/usr/<dataset_path>/Output/<experiment>/)"),
  ("manieye_selected_checkpoint",       pack "Selected Checkpoint In Run     (contents of eg: /home/usr/<dataset_path>/Output/<experiment>/<run>/)"),
  ("manieye_set_dataset",               pack "Selected Dataset Root Folder   (eg: /home/usr/<dataset_path>/Output/<experiment>/<run>/Predictions/)"),
  ("manieye_slider",                    pack "Point in Timeline (x out of maximum image count terminus"),
  ("manieye_playback_start",            pack "Begin Experiment Playback"),
  ("manieye_playback_forward",          pack "Step Forward  on Timeline"),
  ("manieye_playback_backward",         pack "Step Backward on Timeline"),
  ("manieye_image_viewer",              pack "Initial, Difference, Predicted Image"),
  ("manieye_chart_viewer",              pack "SV Chart(s)"),

  ("manifest_inference_analysis",       pack "Inference Analysis"),
  ("manifest_inference_graphic",        pack "Plot to be Loaded from the selected checkpoint"),
  ("manifest_deployment",               pack "Deployment"),
  ("manifest_deployment_blender",       pack "Deploy to Blender"),
  ("manifest_deployment_unreal",        pack "Deploy to Unreal"),
  ("manifest_inference_graphic_actual", pack "Plots of the Model's Training and Test Progress"),

  ("maniga",                            pack "All related Genetic Algorithm Tools (Maniga)"),
  -- put maniga tooltips here
  ("test",                              pack "test")
  ]

getTooltipBody :: String -> [(String, Text)] -> Text
getTooltipBody key mp = snd $ (Prelude.filter (\x -> (fst x == key)) mp) !! 0

liftHelpOverlay :: (Typeable s, Typeable e) => Bool -> String -> WidgetNode s e -> WidgetNode s e
liftHelpOverlay mdl key widget = do
  case mdl of
    True  -> tooltip_ (getTooltipBody key tooltipKeys) [tooltipDelay 2, tooltipFollow] $ widget
    False -> widget
