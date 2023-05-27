module HRayLib3d.Network.UDP  where  --{-# WARNING "This is a prototype module for UDP use only. Subject to change" #-} 

import Data.Word  () 
import Data.Aeson ()
import Data.Map.Strict () 

import qualified Data.UUID as UD

type UDP_Address = String
type UDP_Port    = String 

-- import Manipipe.Util.Network
-- import Manifest.Utils.Log
-- import Numeric (showHex)

-- import Control.Monad             (forever, replicateM_)
-- import Control.Concurrent        (forkIO, threadDelay)
-- import Control.Monad.IO.Class
-- import Control.Exception(catch, SomeException)
-- import qualified Data.ByteString.Char8 as C
-- import qualified Data.Text as T
-- import qualified Data.Time.Clock.System as T
-- import Network.Socket hiding     (recv, recvFrom, sendTo)
-- import Network.Socket.ByteString (recv, recvFrom, sendTo, sendAll)

-- --Since we don't know Red Specific Config Info, This is a placeholder data structure
-- data VideoHardwareConfigResponse 
--     = VideoConfigResponse {
--         vResolution   :: String,
--         vFrameRate    :: String,
--         vBitrate      :: String,
--         vAperture     :: String,
--         vIso          :: String,
--         vShutterspeed :: String
--         --vWhiteBalance :: String,
--         --vMic        :: String
--     }  deriving (Eq, Show)

-- -- PTP(IEEE1588) VIDEO CODE START
-- data VideoEventPacketData =
--     CamCaptureVideoEventData {
--       capDirector :: String,
--       capShooter  :: String,
--       capUuid     :: UD.UUID
--     }
--   | LightVideoEventData {
--       lightState  :: Int
--     } deriving (Show)

-- data VideoEventPacket = VideoEventPacket{
--     eTimestampStart  :: T.SystemTime, --Should probably be an Interval of Time?
--     eTimestampEnd    :: T.SystemTime,
--     eData  :: VideoEventPacketData
--     } deriving (Show)

-- instance ToJSON VideoEventPacket where
--     toJSON (VideoEventPacket st ed d) = do 
--         object [ 
--                 ("video_start_time",      toJSON $ (showHex (T.systemSeconds    st) "")),
--                 ("video_end_time",        toJSON $ (showHex (systemVideoMicroseconds ed) "")),
--                 ("data",                  toJSON d)
--             ]

-- instance ToJSON VideoEventPacketData where
--     toJSON eventData = do 
--         case eventData of 
--             CamCaptureVideoEventData d s u -> object [ 
--                                               ("type",      "video_capture"),
--                                               ("director",  toJSON d),
--                                               ("shooter",   toJSON s),
--                                               ("uuid",      toJSON $ UD.toString u)
--                                             ]
--             LightVideoEventData s -> object [ 
--                                         ("type",  "video_light"),
--                                         ("state", toJSON s)
--                                        ]  

-- systemVideoMicroseconds :: T.SystemTime -> Word32
-- systemVideoMicroseconds (T.MkSystemTime _s ns) = floor $ fromIntegral ns / 1000.0

-- addMsToSystemTimeVideo :: T.SystemTime -> Word32 -> T.SystemTime
-- addMsToSystemTimeVideo (T.MkSystemTime s ns) ms = 
--     let new_ns = ns + (ms * 1000000)
--     in T.MkSystemTime s new_ns

-- sendExecution ::  [(String, [VideoEventPacket])] -> IO [String] --[(Network.Stream.Result (Response String))]--String))]
-- sendExecution urls = mapM (\x -> do
--     urlInfo <- urlInfoFromString ("http://" ++ (fst x) ++ "/video_events") --Sent to the "Director" Camera, which then sets two other cameras to PTPSlaves and itself as the PTPMaster
--     let events :: Data.Map.Strict.Map T.Text [VideoEventPacket] = Data.Map.Strict.fromList [("video_events", (snd x))]
--     --orderedMessageS MANI_LOG_BODY ("PTP Events to be sent(raw request): " ++ (show $ encode events)) 
--     eResponse <- catch (Right <$> requestWithJson asIgnore POST urlInfo events) (\(_ :: SomeException)-> return $ Left "It broke")
--     case eResponse of 
--         Left msg -> return msg 
--         Right () -> return $ (fst x) ++ "sequence sent"
--     ) urls
-- -- PTP(IEEE1588) VIDEO SPECIFIC CODE END

-- {-# WARNING runVideoTakingSession "This is an experimental function, use with caution, it is VERY much subject to change" #-}
-- runVideoTakingSession :: Maybe UDP_Address -> Maybe UDP_Port -> IO ()
-- runVideoTakingSession addr port = do 
--   addrinfos <- getAddrInfo Nothing addr port -- (Just "127.0.0.1") (Just "7000")
--   let serveraddr = head addrinfos
--   -- It is important to remember to use Datagram to receive data via UDP. Then we can bind the socket to the address and wait to receive data.
--   sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
--   bind sock (addrAddress serveraddr)
--   print "UDP server is waiting..."
--   recv sock 4096 >>= \message -> print ("UDP server received: " ++ (C.unpack message))
--   print "Transmission Complete, UDP server socket is closing now."
--   close sock

-- {-# WARNING runVideoTakingSessionLong "This is an experimental function, use with caution, it is VERY much subject to change" #-}
-- runVideoTakingSessionLong :: Int -> Maybe UDP_Port -> IO ()
-- runVideoTakingSessionLong  longtime port = do
--   sock <- socket AF_INET6 Datagram defaultProtocol
--   addr:_ <- getAddrInfo (Just defaultHints
--                           { addrFamily = AF_INET6, addrSocketType = Datagram })
--                         (Just "::1") port --(Just "7331")
--   bind sock (addrAddress addr)
--   replicateM_ 16 $ forkIO $ videoTakingWorker sock
--   forever $ threadDelay longtime
--   --where longtime = 10^12

-- {-# WARNING videoTakingWorker "This function is intended to be used with runVideoTakingSessionLong only" #-}
-- videoTakingWorker :: Socket -> IO ()
-- videoTakingWorker sock = forever $ do
--   (msg, client) <- recvFrom sock 4096
--   threadDelay 1000000   -- simulate some processing or do something
--   sendTo sock msg client