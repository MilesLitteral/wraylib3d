{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE DatatypeContexts #-}

module Manifest.Control.NetworkManager where 

-- import Manipipe.Client.Http ()
import Data.Aeson 
import Data.Aeson.KeyMap 
import Data.Maybe
import Data.Word 
import qualified Data.UUID as UD
import Data.Map.Strict
import qualified Data.Text as T
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Exception(catch, SomeException)

import Control.Lens hiding ((.=))
import Manipipe.Util.Network
import Manifest.Utils.Log
import Numeric (showHex)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Char8 as B8
import qualified Data.Time.Clock as T
import qualified Data.Time.Clock.System as T

-- Make a Rule for checking if camera is available, apply to all endpoints
-- GET      /camera             -- List default configuration settings (shutterspeed, aperture, iso)
-- GET      /camera/{setting}   -- Returns configuration setting for setting
-- POST     /camera             -- Opens connection to camera
-- DELETE   /camera             -- Closes connection to camera
-- PUT      /camera             -- Sets all configurations listed in request body

-- POST     /preview            --Transfers image preview to memory
-- POST     /preview/all        --Transfers previews for all images on camera
-- GET      /preview/{name}     --Returns preview for image with given name

-- POST     /image              --Transfers all specified images to memory
-- GET      /image/list         --Lists out all images saved on camera memory card
-- GET      /image/{name}       --Returns the image with the given name
-- DELETE   /image              --Clears SD card on camera

-- POST     /trigger            --Presses down the cameras trigger button
-- DELETE   /trigger            --Releases cameras trigger button

-- POST     /capture            -- Captures an image on the camera
-- GET      /capture            -- Lists the number of captures on camera this session

-- GET      /ping               -- Sends OK as long as server is running
-- GET      /ping/camera        -- Sends OK if communication with camera still possible

-- data CommandType
--     = CommandTrigger
--     | CommandUnTrigger
--     | CommandInitialize
--     | CommandSelect
--     | CommandSelectBroadcast
--     | CommandUpload
--     | CommandDownload
--     | CommandDownloadBroadcast

-- class IsCommand command where
--     verify :: command -> CommandType

-- instance IsCommand String where
--     verify a = case a of
--         "HELLO" -> CommandTrigger

-- Attach a list of presets as the body of a Trigger event
-- The Pi will see the Preset and act accordingly,
-- Sending On/Off States to the lights on it's GPIO pins

data LightPreset
    = LSTATE0 -- 00 /0
    | LSTATE1 -- 01 /1
    | LSTATE2 -- 10 /4
    | LSTATE3 -- 11 /7
    deriving(Show, Eq)

data ServerResponse a =
    ValidServerResponse {srResponse     :: a}
  | ErrorServerResponse {srErrorMessage :: String}
  deriving (Eq, Show)

data HardwareCameraResponse = CameraResponse {
    camera ::  Int
    }  deriving (Eq, Show)

data HardwareConfigResponse 
    = ConfigResponse {
        aperture     :: String,
        iso          :: String,
        shutterspeed :: String
    }  deriving (Eq, Show)

data HardwareErrorResponse 
    = ErrorResponse {
        errorMessage :: String
    }  deriving (Eq, Show)

-- PTP(IEEE1588) CODE START
data EventPacketData =
    CamCaptureEventData {
      capTrigger  :: Bool,
      capUuid     :: UD.UUID
    }
  | CamFocusEventData
  | LightEventData {
      lightState  :: Int
    } deriving (Show)

data EventPacket = EventPacket{
    eTime  :: T.SystemTime,
    eData  :: EventPacketData
    } deriving (Show)

-- data EventPacket s = EventPacket{
--     eTime  :: T.SystemTime,
--     eState :: s,
--     eUUID  :: UD.UUID
--     } deriving (Show)

instance ToJSON EventPacket where
    toJSON (EventPacket st d) = do 
        object [ 
                ("seconds",      toJSON $ (showHex (T.systemSeconds    st) "")),
                ("microseconds", toJSON $ (showHex (systemMicroseconds st) "")),
                ("data",         toJSON d)
            ]

instance ToJSON EventPacketData where
    toJSON eventData = do 
        case eventData of 
            CamCaptureEventData s u-> object [ 
                                              ("type",  "capture"),
                                              ("state", toJSON s),
                                              ("uuid",  toJSON $ UD.toString u)
                                             ]
            CamFocusEventData -> object [("type", "focus")]
            LightEventData s -> object [ 
                                        ("type",  "light"),
                                        ("state", toJSON s)
                                       ]  
                                       
instance (FromJSON a) => FromJSON (ServerResponse a) where
    parseJSON (Object v) = do 
        result <- v .:? "response"
        case result of 
            Just res -> ValidServerResponse <$> parseJSON res
            Nothing  -> do  
                err <- v .:? "errorMessage"
                case err of
                    Just errMsg -> return $ ErrorServerResponse errMsg
                    Nothing -> return $ ErrorServerResponse "Response too malformed to get error message."
    parseJSON _ = return $ ErrorServerResponse "Cannot parse server response"
    
instance ToJSON HardwareCameraResponse where 
    toJSON s = object    [ "camera"     .= camera s ]

instance FromJSON HardwareCameraResponse where
    parseJSON (Object v) = CameraResponse <$> v .: "camera"

instance ToJSON   HardwareConfigResponse where 
    toJSON s = object    [ "aperture"          .= aperture s,
                            "iso"              .= Manifest.Control.NetworkManager.iso s, 
                            "shutterspeed"     .= shutterspeed s]

instance FromJSON HardwareConfigResponse where
    parseJSON (Object v) = do
        resp     <- v    .: "response" 
        aperture <- resp .: "aperture"
        iso      <- resp .: "iso"
        shutter  <- resp .: "shutterspeed"
        return $ ConfigResponse aperture iso shutter
     
instance ToJSON HardwareErrorResponse where 
    toJSON s = object    [ "errorMessage"     .= Manifest.Control.NetworkManager.errorMessage s ]
    
instance FromJSON HardwareErrorResponse where
    parseJSON (Object v) = ErrorResponse <$> v .: "errorMessage"

systemMicroseconds :: T.SystemTime -> Word32
systemMicroseconds (T.MkSystemTime _s ns) = floor $ fromIntegral ns / 1000.0

addMsToSystemTime :: T.SystemTime -> Word32 -> T.SystemTime
addMsToSystemTime (T.MkSystemTime s ns) ms = 
    let new_ns = ns + (ms * 1000000)
    in T.MkSystemTime s new_ns

sendSequence ::  [(String, [EventPacket])] -> IO [String] --[(Network.Stream.Result (Response String))]--String))]
sendSequence urls = mapM (\x -> do
    urlInfo <- urlInfoFromString ("http://" ++ (fst x) ++ "/events") 
    let events :: Data.Map.Strict.Map T.Text [EventPacket] = Data.Map.Strict.fromList [("events", (snd x))]
    --orderedMessageS MANI_LOG_BODY ("PTP Events to be sent(raw request): " ++ (show $ encode events)) 
    eResponse <- catch (Right <$> requestWithJson asIgnore POST urlInfo events) (\(_ :: SomeException)-> return $ Left "It broke")
    case eResponse of 
        Left msg -> return msg 
        Right () -> return $ (fst x) ++ "sequence sent"
    ) urls
-- PTP(IEEE1588) SPECIFIC CODE END

extractServerResponse :: forall a . (FromJSON a) => String -> ServerResponse a
extractServerResponse respStr =
    let response = decode (BL.fromStrict $ BC.pack respStr) :: Maybe (ServerResponse a) in
    case response of
        Just resp -> resp
        Nothing   -> ErrorServerResponse "Server response parsing error."

makeServerIgnoreRequest :: IO () -> IO (ServerResponse ())
makeServerIgnoreRequest request = do
    -- catches HTTP errors and inserts them into a ServerResponse
    -- In the future it will filter HTTP status code errors to make error messages more clear.
    response <- catch (Right <$> request) (\(e::SomeException)-> return $ Left $ show e)
    case response of
        Left msg -> return $ ErrorServerResponse ("Request failed: " ++ msg)
        Right _ -> return $ ValidServerResponse ()

makeServerByteStringRequest :: IO B.ByteString -> IO (ServerResponse B.ByteString)
makeServerByteStringRequest request = do
    -- catches HTTP errors and inserts them into a ServerResponse
    -- In the future it will filter HTTP status code errors to make error messages more clear.
    response <- catch (Right <$> request) (\(e::SomeException)-> return $ Left $ show e)
    case response of
        Left msg -> return $ ErrorServerResponse ("Request failed: " ++ msg)
        Right bytes -> return $ ValidServerResponse bytes

makeServerJSONRequest :: (FromJSON a) => IO (ServerResponse a) -> IO (ServerResponse a)
makeServerJSONRequest request = do
    -- catches HTTP errors and inserts them into a ServerResponse
    -- In the future it will filter HTTP status code errors to make error messages more clear.
    response <- catch (Right <$> request) (\(e::SomeException)-> return $ Left $ show e)
    case response of
        Left msg -> return $ ErrorServerResponse ("Request failed: " ++ msg)
        Right (serverResponse :: ServerResponse a) -> 
            case serverResponse of
                ErrorServerResponse msg -> return $ ErrorServerResponse ("Server returned error: " ++ msg)
                ValidServerResponse resp -> return $ ValidServerResponse resp
 
initializeCameraServers ::  [String] -> IO [String]
initializeCameraServers urls = mapM (\host -> do
    url <- urlInfoFromString ("http://" ++ host ++ "/camera")
    (response :: ServerResponse String) <- makeServerJSONRequest $ requestWithNone asJSON POST url
    case response of 
        ValidServerResponse responseStr -> return responseStr
        ErrorServerResponse msg -> do
            putStrLn msg
            return msg
    ) urls

pingCameraServers   :: [String] -> IO [Int]
pingCameraServers urls  = mapM (\host -> do
    url <- urlInfoFromString ("http://" ++ host ++ "/ping")
    (eResponse :: ServerResponse (Data.Map.Strict.Map T.Text Int)) <- makeServerJSONRequest $ requestWithNone asJSON GET url
    case eResponse of 
        ValidServerResponse responseJson -> return $ responseJson ! "camera"
        ErrorServerResponse msg -> do
            putStrLn msg
            return (-1)     
    ) urls


pingCameraDirect   :: [String] -> IO [Int]
pingCameraDirect urls  = mapM (\host -> do
    url <- urlInfoFromString ("http://" ++ host ++ "/ping/camera")
    (response :: ServerResponse (Data.Map.Strict.Map T.Text Int)) <- makeServerJSONRequest $ requestWithNone asJSON GET url
    case response of 
        ValidServerResponse responseJson -> return $ responseJson ! "camera"
        ErrorServerResponse msg -> do
            putStrLn msg
            return (-1)
    ) urls

getCameraServersConfig   :: String -> IO [(String, [String])]
getCameraServersConfig camHost  = do
    let defaultConfigs = ["shutterspeed", "aperture", "iso", "autopoweroff"]
    let unavailable    = "Unavailable"
    print camHost 
    url <- urlInfoFromString ("http://" ++ camHost ++ "/camera")
    (response :: ServerResponse (Data.Map.Strict.Map T.Text String)) <- makeServerJSONRequest $ requestWithNone asJSON GET url
    activeCfgs <- case response of
        ValidServerResponse responseJson -> return responseJson
        ErrorServerResponse msg -> do
            putStrLn msg
            return Data.Map.Strict.empty
    url <- urlInfoFromString ("http://" ++ camHost ++ "/camera?config=yes")
    (response :: ServerResponse (Data.Map.Strict.Map T.Text [String])) <- makeServerJSONRequest $ requestWithNone asJSON GET url
    cfgOptions <- case response of 
        ValidServerResponse responseJson -> return responseJson
        ErrorServerResponse msg -> do
            putStrLn msg
            return Data.Map.Strict.empty
    return $ Prelude.map (\settingName -> 
        let active  = findWithDefault unavailable settingName activeCfgs
            options = findWithDefault [] settingName cfgOptions
        in (active, options)) defaultConfigs

setCameraServersConfigSetting   :: [String] -> HardwareConfigResponse -> IO ()
setCameraServersConfigSetting  urls newConfig = mapM_ (\host -> do
    url <- urlInfoFromString ("http://" ++ host ++ "/camera")
    response <- makeServerIgnoreRequest $ requestWithJson asIgnore PUT url newConfig
    case response of 
        ErrorServerResponse msg -> do 
            putStrLn msg
        ValidServerResponse _ -> return ()
    ) urls

deleteCameraServers :: [String] -> IO [Int]--[(Network.Stream.Result    (Response String))]
deleteCameraServers urls = mapM (\host -> do
    url <- urlInfoFromString ("http://" ++ host ++ "/camera")
    (response :: ServerResponse (Data.Map.Strict.Map T.Text Int)) <- makeServerJSONRequest $ requestWithNone asJSON DELETE url
    case response of 
        ValidServerResponse responseJson -> return $ responseJson ! "camera"
        ErrorServerResponse msg -> do
            putStrLn msg
            return (-1)
    ) urls

triggerCameraServers :: [String] -> IO [Int] --[(Network.Stream.Result (Response String))]
triggerCameraServers urls = mapM (\host -> do
    url <- urlInfoFromString ("http://" ++ host ++ "/trigger")
    (response :: ServerResponse (Data.Map.Strict.Map T.Text Int)) <- makeServerJSONRequest $ requestWithNone asJSON POST url
    case response of 
        ValidServerResponse responseJson -> return $ responseJson ! "camera"
        ErrorServerResponse msg -> do
            putStrLn msg
            return (-1)
    ) urls

-- triggerCameraServersWithLightStates :: [String] -> [LightPreset] -> IO [Int] --[(Network.Stream.Result (Response String))]
-- triggerCameraServersWithLightStates urls lights = mapM (\x -> do
--     urlInfo <- urlInfoFromString ("http://" ++ x ++ "/trigger")  
--     eResponse <- catch (Right <$> requestWithNone asJSON POST <$> urlInfo lights) (\(_ ::SomeException)-> return $ Left "It broke")
--     case eResponse of 
--         Left _ -> return (-1)
--         Right (response :: IO (ServerResponse (Data.Map.Strict.Map T.Text Int))) -> do
--             response2 <- response
--             case response2 of 
--                 ValidServerResponse responseJson -> return $ responseJson ! "camera"
--                 ErrorServerResponse msg -> do
--                     putStrLn $ "Server error: " ++ msg
--                     return (-1)
--     ) urls
    
untriggerCameraServers :: [String] -> IO [Int] --[(Network.Stream.Result (Response String))]
untriggerCameraServers urls = mapM (\host -> do
    url <- urlInfoFromString ("http://" ++ host ++ "/trigger")
    (response :: ServerResponse (Data.Map.Strict.Map T.Text Int)) <- makeServerJSONRequest $ requestWithNone asJSON DELETE url
    case response of 
        ValidServerResponse responseJson -> return $ responseJson ! "camera"
        ErrorServerResponse msg -> do
            putStrLn msg
            return (-1)
    ) urls

getAllPreviews :: String -> IO [(String, B.ByteString)]
getAllPreviews host = do
    -- post preview all
    url <- urlInfoFromString ("http://" ++ host ++ "/preview/all")
    (response :: ServerResponse (Data.Map.Strict.Map T.Text [T.Text])) <- makeServerJSONRequest $ requestWithNone asJSON POST url
    imageList <- case response of 
        ValidServerResponse responseJson -> return $ responseJson ! "images"
        ErrorServerResponse msg -> do
            putStrLn msg
            return []
    mapM (\imageName -> do
        url <- urlInfoFromString ("http://" ++ host ++ "/image/" ++ T.unpack imageName)
        response <- makeServerByteStringRequest $ requestWithNone asByteString GET url
        case response of 
            ErrorServerResponse msg   -> error msg
            ValidServerResponse bytes -> return (T.unpack imageName, bytes) 
        ) imageList

getAllCaptures :: [(String, [UD.UUID])] -> Bool -> IO (Data.Map.Strict.Map UD.UUID B.ByteString)
getAllCaptures desiredCaptures preview = do
    let endpoint = if preview then "/preview" else "/image"
    transferReqResults <- mapM (\(host, desiredUuids)-> do
        url <- urlInfoFromString ("http://" ++ host ++ endpoint)
        let payload = Data.Map.Strict.fromList [("captures"::T.Text, Prelude.map UD.toString desiredUuids)]
        response <- makeServerIgnoreRequest $ requestWithJson asIgnore POST url payload
        case response of
            ErrorServerResponse msg -> do
                putStrLn msg
                return False
            ValidServerResponse _ -> return True
        ) desiredCaptures
    downloadedImages <- mapM (\(host, desiredUuids)-> do
        Data.Map.Strict.fromList <$> mapM (\uuid -> do
                url <- urlInfoFromString ("http://" ++ host ++ "/image/" ++ UD.toString uuid)
                response <- makeServerByteStringRequest $ requestWithNone asByteString GET url
                case response of
                    ErrorServerResponse msg -> error msg
                    ValidServerResponse img -> return (uuid, img)
            ) desiredUuids
        ) desiredCaptures
    return $ Data.Map.Strict.unions downloadedImages

downloadCameraServersPreviewContents :: [String] -> IO [Int] --[(Network.Stream.Result (Response String))]
downloadCameraServersPreviewContents urls = mapM (\host -> do
    url <- urlInfoFromString ("http://" ++ host ++ "/trigger")
    (response:: ServerResponse (Data.Map.Strict.Map T.Text Int)) <- makeServerJSONRequest $ requestWithNone asJSON POST url
    case response of 
        ValidServerResponse responseJson -> return $ responseJson ! "camera"
        ErrorServerResponse msg -> do
            putStrLn msg
            return (-1)
    ) urls
    
downloadCameraSelectServersContents :: [String] -> IO [Int] --[(Network.Stream.Result (Response String))]
downloadCameraSelectServersContents urls = mapM (\host -> do
    url <- urlInfoFromString ("http://" ++ host)
    (response:: ServerResponse (Data.Map.Strict.Map T.Text Int)) <- makeServerJSONRequest $ requestWithNone asJSON GET url
    case response of 
        ValidServerResponse responseJson -> return $ responseJson ! "camera"
        ErrorServerResponse msg -> do
            putStrLn msg
            return (-1)
    ) urls

downloadCameraServersContents :: String -> IO [(String, B.ByteString)] --[(Network.Stream.Result (Response String))]
downloadCameraServersContents url = do
    -- post preview all
    eResponse <- catch (Right <$> requestWithNone asJSON GET <$> urlInfoFromString ("http://" ++ url ++ "/image/list")) (\(e::SomeException)-> return $ Left $ "Preview Error: " ++ show e)
    imageList <- case eResponse of 
        Left msg -> error msg
        Right (response :: IO (ServerResponse (Data.Map.Strict.Map T.Text [T.Text]))) -> do
            response2 <- response
            case response2 of
                ValidServerResponse responseJson -> do
                    --print  $ responseJson
                    return $ responseJson ! "names"
                ErrorServerResponse msg -> do
                    putStrLn $ "Server error: " ++ msg
                    return []
    -- -- post imageList to /image/
    postUrl <- urlInfoFromString (url ++ "/image")
    let postJSON = Data.Map.Strict.fromList [("images", imageList)] :: Data.Map.Strict.Map T.Text [T.Text]
    -- -- end createJSON Body
    !ePostResponse <- catch (Right <$> requestWithJson asIgnore POST postUrl postJSON) (\(e::SomeException)-> return $ Left $ "Preview Error: " ++ show e)
    case ePostResponse of 
        Left msg -> error msg
        Right _ -> do
            print $ "Request sent"
            threadDelay 100000
            return ()
    mapM (\imageName -> do
        eImageResponse <- catch (Right <$> requestWithNone asByteString GET <$> urlInfoFromString ("http://" ++ url ++ "/image/" ++ (T.unpack imageName))) (\(e::SomeException)-> return $ Left $ show e)
        print $ "Response Received"
        case eImageResponse of 
            Left msg -> error "Bad Images Received" --return ("", B.empty)
            Right bytes -> do 
                print $ "Serializing Response"
                b <- (bytes :: IO B.ByteString)
                --B.writeFile ("./assets/rawImages/test.jpg") b
                print "Return Result"
                return (T.unpack imageName, b) 
        ) imageList

formatCameraServersContents   :: [String] -> IO [Int] --[(Network.Stream.Result (Response String))]
formatCameraServersContents   urls = mapM (\host -> do
    url <- urlInfoFromString ("http://" ++ host ++ "/image")
    (response :: ServerResponse (Data.Map.Strict.Map T.Text Int)) <- makeServerJSONRequest $ requestWithNone asJSON DELETE url
    case response of 
        ValidServerResponse responseJson -> return $ responseJson ! "camera"
        ErrorServerResponse msg -> do
            putStrLn msg
            return (-1)
    ) urls

closeCameraServers :: [String] -> IO [Int] --[(Network.Stream.Result (Response String))]
closeCameraServers urls = mapM (\host -> do
    url <- urlInfoFromString ("http://" ++ host ++ "/camera")
    (response :: ServerResponse (Data.Map.Strict.Map T.Text Int)) <- makeServerJSONRequest $ requestWithNone asJSON DELETE url
    case response of 
        ValidServerResponse responseJson -> return $ responseJson ! "camera"
        ErrorServerResponse msg -> do
            putStrLn msg
            return (-1)
    ) urls

    