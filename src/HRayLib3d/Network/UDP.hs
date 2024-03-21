{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module HRayLib3d.Network.UDP  where

import Data.Word       ()
import Data.Aeson      (ToJSON(..), object)
import Data.Map.Strict (Map, fromList)
import Numeric         (showHex)
import qualified Data.UUID as UD
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import Control.Exception (SomeException, catch)
import Control.Monad ( forever, replicateM_ )
import Control.Concurrent ( threadDelay, forkIO )
import Network.Socket
    ( defaultHints,
      getAddrInfo,
      bind,
      socket,
      close,
      defaultProtocol,
      AddrInfo(addrAddress, addrFamily, addrSocketType),
      Family(AF_INET6),
      Socket,
      SocketType(..) ) 
import Network.Socket.Address hiding (bind)
import Network.Socket.ByteString (recv)
import Network.HTTP.Types 
import HRayLib3d.Network.Requests ( requestWithJson, asIgnore, urlInfoFromString ) 
-- #UDP
-- A Special UDP module for utilizing UDP instead of TCP
-- largely for Multiplayer, and Realm communication whereas
-- TCP(Requests) is more oriented towards traditional HTTP 
-- communication.

type UDP_Address = String
type UDP_Port    = String

data UDPResponse
    = UDPResponse {
        udpHost       :: String,
        udpMap        :: String,
        udpPlayerCnt  :: String,
        udpPrivate    :: String
    }  deriving (Eq, Show)

-- PTP(IEEE1588) VIDEO CODE START
data UDPPacketData =
    MultiplayerEventData {
      mpHost           :: String,
      mpPlayer         :: String,
      mpIdentifier     :: UD.UUID
    }
  | RealmEventData {
      rlmState         :: Int
    } deriving (Show)

instance ToJSON UDPPacketData where
    toJSON eventData = do
        case eventData of
            MultiplayerEventData d s u -> object [
                                              ("type",      "mp_data"),
                                              ("host",      toJSON d),
                                              ("player",    toJSON s),
                                              ("uuid",      toJSON $ UD.toString u)
                                            ]
            RealmEventData s -> object [
                                        ("type",  "realm_data"),
                                        ("state", toJSON s)
                                       ]


-- {-# Run a UDP Session #-}
runUDPSession :: Maybe UDP_Address -> Maybe UDP_Port -> IO ()
runUDPSession addr port = do
  addrinfos <- getAddrInfo Nothing addr port -- (Just "127.0.0.1") (Just "7000")
  let serveraddr = head addrinfos
  -- It is important to remember to use Datagram to receive data via UDP. Then we can bind the socket to the address and wait to receive data.
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  bind sock (addrAddress serveraddr)
  print "UDP server is waiting..."
  recv sock 4096 >>= \message -> print ("UDP server received: " ++ C.unpack message)
  print "Transmission Complete, UDP server socket is closing now."
  close sock

-- {-# Run a UDP Session with a Service-Worker #-}
runUDPSessionWithWorker :: Int -> Maybe UDP_Port -> IO ()
runUDPSessionWithWorker  longtime port = do
  sock <- socket AF_INET6 Datagram defaultProtocol
  addr:_ <- getAddrInfo (Just defaultHints { addrFamily = AF_INET6, addrSocketType = Datagram })
                        (Just "::1") port --(Just "7331")
  bind sock (addrAddress addr)
  replicateM_ 16 $ forkIO $ udpWorker sock
  forever $ threadDelay longtime
  --where longtime = 10^12

-- {-# udpWorker, use with udp session functions to act over time. Useful for pinging a RESTful API #-}
udpWorker :: Socket -> IO ()
udpWorker sock = forever $ do
  (msg, client) <- recvFrom sock 4096 :: IO (C.ByteString, String)
  threadDelay 1000000   -- simulate some processing or do something
  sendTo sock msg client

sendPackets ::  [(String, [UDPPacketData])] -> IO [String]
sendPackets = mapM (\x -> do
    urlInfo <- urlInfoFromString ("http://" ++ fst x ++ "/mp/send")
    let events :: Data.Map.Strict.Map T.Text [UDPPacketData] = Data.Map.Strict.fromList [("multiplayer_events", snd x)]
    --orderedMessageS LOG_BODY ("PTP Events to be sent(raw request): " ++ (show $ encode events)) 
    eResponse <- catch (Right <$> requestWithJson asIgnore POST urlInfo events) (\(_ :: SomeException) -> return $ Left "It broke")
    case eResponse of
        Left msg -> return msg
        Right () -> return $ fst x ++ "sequence sent"
    )