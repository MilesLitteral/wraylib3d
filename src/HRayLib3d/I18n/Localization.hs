{-# LANGUAGE DeriveGeneric #-}
module HRayLib3d.I18n.Localization where

    import Monomer 
    import System.IO
    import System.Process 
    import GHC.Generics(Generic)

    import Data.Text
    import Data.Maybe
    import Data.Aeson
    import Data.Typeable 

    data    LJSON                   = LJSON                { lKey :: String, lValue :: String }                      deriving (Generic, Eq, Show)
    data    Internationalization    = Internationalization { translations  :: [(String, Text)], fallbacks :: Bool}   deriving (Generic, Eq, Show)
    newtype I18nCollection          = I18nCollection       { cTranslations :: [Internationalization]             }   deriving (Generic, Eq, Show)

    instance FromJSON LJSON 
    instance FromJSON Internationalization
    instance FromJSON I18nCollection

    getSystemLocalWin32 :: String -> IO Handle
    getSystemLocalWin32 metal = do
        (_, Just hout, _, _) <- createProcess (proc "powershell" ["Get-WinSystemLocale []"]){ cwd = Just "./", std_out = CreatePipe }
        return hout
        
    getSystemLocalOSX   :: String -> IO Handle
    getSystemLocalOSX metal = do
        (_, Just hout, _, _) <- createProcess (proc "defaults" ["read", ".GlobalPreferences", "AppleLanguages", "| tr -d [:space:] ", "| cut -c2-3"]){ cwd = Just "./", std_out = CreatePipe }
        return hout
        
    getSystemLocalLinux :: String -> IO Handle
    getSystemLocalLinux metal = do
        (_, Just hout, _, _) <- createProcess (proc "locale" ["-k", "LANG"]){ cwd = Just "./", std_out = CreatePipe }
        return hout

    setTranslation path fallbacks = do
        fp  <- decodeFileStrict path
        return $ Internationalization (fromJust fp) fallbacks

    setTranslationsFromFile path fallbacks = do
        paths <- decodeFileStrict path
        return $ I18nCollection (fromJust paths)

    defaultI18nKeysEN :: Bool -> Internationalization --[(String, Text)]
    defaultI18nKeysEN fallbacksAvailable = Internationalization [
            ("manieye_sv_graph",                pack "Takes a ManiFile Source (a folder that contains a test_sv.mani and train_sv.mani),\n or a Generator (TBA)" ),
            ("manieye_image_selector",          pack "View Images from Scene Vectors via On The Fly rendering (Point to the GroundTruths folder of the project in question)\n or point to a Image (Folder) source (such as /Seq/)\n *WARNING: SV Source is Disabled until maniapi/local_batch is merged into maniapi/manifest as these branches are incompatible currently"),
            ("manieye_playback",                pack "Begin playback of loaded images and or graphs,\n It is suggested you run through the loaded images and graph atleast once with playback to allow for caching of the dataset\n then run it back to the beginning and begin screen capture"),
            ("manieye_step_forward",            pack "Step forward  in index by one"),
            ("manieye_step_backward",           pack "Step backward in index by one"),
            ("manieye_timeline",                pack "Scrum through loaded images and or graphs\n also denotes index within loaded datasets"),
            ("manieye_add_widget_dropdown",     pack "Select a Widget of defined Type to create, widget types consist of:\n Image Selectors\n SVGraphs\n new widgets will appear over time here"),
            ("manieye_add_widget",              pack "Add a Widget to the viewing area a dialogue will appear when this button is clicked,\n click again to cancel the dialogue"),
            ("test",                            pack "test")
        ] fallbacksAvailable

    defaultI18nKeysHY :: Bool -> Internationalization --[(String, Text)]
    defaultI18nKeysHY fallbacksAvailable = Internationalization [
            ("manieye_sv_graph",                pack "Takes a ManiFile Source (a folder that contains a test_sv.mani and train_sv.mani),\n or a Generator (TBA)" ),
            ("manieye_image_selector",          pack "View Images from Scene Vectors via On The Fly rendering (Point to the GroundTruths folder of the project in question)\n or point to a Image (Folder) source (such as /Seq/)\n *WARNING: SV Source is Disabled until maniapi/local_batch is merged into maniapi/manifest as these branches are incompatible currently"),
            ("manieye_playback",                pack "Begin playback of loaded images and or graphs,\n It is suggested you run through the loaded images and graph atleast once with playback to allow for caching of the dataset\n then run it back to the beginning and begin screen capture"),
            ("manieye_step_forward",            pack "Step forward  in index by one"),
            ("manieye_step_backward",           pack "Step backward in index by one"),
            ("manieye_timeline",                pack "Scrum through loaded images and or graphs\n also denotes index within loaded datasets"),
            ("manieye_add_widget_dropdown",     pack "Select a Widget of defined Type to create, widget types consist of:\n Image Selectors\n SVGraphs\n new widgets will appear over time here"),
            ("manieye_add_widget",              pack "Add a Widget to the viewing area a dialogue will appear when this button is clicked,\n click again to cancel the dialogue"),
            ("test",                            pack "test")
        ] fallbacksAvailable

    getI18nBody :: String -> Internationalization -> Text
    getI18nBody key mp = snd $ (Prelude.filter (\x -> (fst x == key)) $ translations mp) !! 0

    liftInternationalizationOverlay :: (Typeable s, Typeable e) => Bool -> String -> WidgetNode s e -> WidgetNode s e
    liftInternationalizationOverlay mdl key widget = do
        case mdl of
            True  -> tooltip_ (getI18nBody key $ defaultI18nKeysEN False) [tooltipDelay 2,  tooltipFollow] (widget) `styleBasic` [textColor (Color 2 2 2 1), bgColor (Color 253 204 128 1)]
            False -> widget