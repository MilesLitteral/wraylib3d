{-# LANGUAGE DeriveGeneric #-}
module HRayLib3d.I18n.Localization where

    import Monomer        ( Color(Color), tooltipDelay, tooltipFollow, tooltip_, CmbBgColor(bgColor), CmbStyleBasic(styleBasic), CmbTextColor(textColor), WidgetNode ) 
    import System.IO      ( Handle )
    import System.Process ( createProcess, proc, CreateProcess(std_out, cwd), StdStream(CreatePipe) ) 
    import GHC.Generics   (Generic)

    import Data.Text  ( Text, pack )
    import Data.Maybe ( fromJust   )
    import Data.Aeson ( FromJSON, decodeFileStrict )

    -- Localization (I18N)
    -- All Localization datatypes are used here
    -- these functions are also related to tooltips
    -- and getting the system locale.

    -- I18NCollections are Arrays of Localizations
    -- represented as JSON objects. A collection would
    -- hold all available languages, eg:
    -- [English, Spanish, Chinese]
    -- The Localizations then share keys but have
    -- localized entries for the same key. 
    -- eg: {
    --       "open_app_welcome" : "Hello"
    --     }
    --     then
    --     {
    --        "open_app_welcome" : "你好"
    --     }
    -- The Locale variable from system is therefore
    -- mapped to an specific index in the I18NCollection.
    -- Perhaps in the future the variable could be mapped to some sort of 
    -- Key in the data structure itself??

    data LJSON = 
        LJSON  { 
            lKey      :: String,           
            lValue    :: String 
        }   deriving (Generic, Eq, Show)

    data Internationalization = 
        Internationalization { 
            translations  :: [(String, Text)], 
            fallbacks     :: Bool   
        }   deriving (Generic, Eq, Show)

    newtype I18nCollection    = 
        I18nCollection       { 
            cTranslations :: [Internationalization]
        }   deriving (Generic, Eq, Show)

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

    defaultI18nKeysEN :: Bool -> Internationalization
    defaultI18nKeysEN fallbacksAvailable = Internationalization [
            ("en_load",                pack "Loading Files" ),
            ("test",                   pack "test")
        ] fallbacksAvailable

    defaultI18nKeysHY :: Bool -> Internationalization
    defaultI18nKeysHY fallbacksAvailable = Internationalization [
            ("hy_load",                pack "Takes a ManiFile Source (a folder that contains a test_sv.mani and train_sv.mani),\n or a Generator (TBA)" ),
            ("test",                   pack "test")
        ] fallbacksAvailable

    getI18nBody :: String -> Internationalization -> Text
    getI18nBody key mp = snd $ (Prelude.filter (\x -> (fst x == key)) $ translations mp) !! 0

    liftInternationalizationOverlay :: Bool -> String -> WidgetNode s e -> WidgetNode s e
    liftInternationalizationOverlay mdl key widget = do
        case mdl of
            True  -> tooltip_ (getI18nBody key $ defaultI18nKeysEN False) [tooltipDelay 2,  tooltipFollow] (widget) `styleBasic` [textColor (Color 2 2 2 1), bgColor (Color 253 204 128 1)]
            False -> widget
