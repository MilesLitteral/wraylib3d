{-# LANGUAGE DeriveGeneric #-}
module HRayLib3d.I18n.Localization where

    import Monomer       ( Color(Color), tooltipDelay, tooltipFollow, tooltip_, CmbBgColor(bgColor), CmbStyleBasic(styleBasic), CmbTextColor(textColor), WidgetNode ) 
    import System.IO      ( Handle )
    import System.Process ( createProcess, proc, CreateProcess(std_out, cwd), StdStream(CreatePipe) ) 
    import GHC.Generics   (Generic)

    import Data.Text  ( Text, pack )
    import Data.Maybe ( fromJust   )
    import Data.Aeson ( FromJSON, decodeFileStrict )

    data    LJSON                   = LJSON                { lKey          :: String,           lValue    :: String }                      deriving (Generic, Eq, Show)
    data    Internationalization    = Internationalization { translations  :: [(String, Text)], fallbacks :: Bool   }   deriving (Generic, Eq, Show)
    newtype I18nCollection          = I18nCollection       { cTranslations :: [Internationalization]                }   deriving (Generic, Eq, Show)

    instance FromJSON LJSON 
    instance FromJSON Internationalization
    instance FromJSON I18nCollection

    getSystemLocalWin32 :: IO Handle
    getSystemLocalWin32 = do
        (_, Just hout, _, _) <- createProcess (proc "powershell" ["Get-WinSystemLocale []"]){ cwd = Just "./", std_out = CreatePipe }
        return hout
        
    getSystemLocalOSX   :: IO Handle
    getSystemLocalOSX = do
        (_, Just hout, _, _) <- createProcess (proc "defaults" ["read", ".GlobalPreferences", "AppleLanguages", "| tr -d [:space:] ", "| cut -c2-3"]){ cwd = Just "./", std_out = CreatePipe }
        return hout
        
    getSystemLocalLinux :: IO Handle
    getSystemLocalLinux = do
        (_, Just hout, _, _) <- createProcess (proc "locale" ["-k", "LANG"]){ cwd = Just "./", std_out = CreatePipe }
        return hout

    setTranslation :: FilePath -> Bool -> IO Internationalization
    setTranslation path fallbacks = do
        fp  <- decodeFileStrict path
        return $ Internationalization (fromJust fp) fallbacks

    setTranslationsFromFile :: FilePath -> IO I18nCollection
    setTranslationsFromFile path  = do
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

    liftInternationalizationOverlay :: Bool -> String -> WidgetNode s e -> WidgetNode s e
    liftInternationalizationOverlay mdl key widget = do
        case mdl of
            True  -> tooltip_ (getI18nBody key $ defaultI18nKeysEN False) [tooltipDelay 2,  tooltipFollow] (widget) `styleBasic` [textColor (Color 2 2 2 1), bgColor (Color 253 204 128 1)]
            False -> widget
