{-# LANGUAGE DataKinds, TypeSynonymInstances, FlexibleInstances, TemplateHaskell, ScopedTypeVariables, DeriveGeneric, OverloadedStrings  #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module HRayLib3d.Network.Requests where

    import Data.Text
    import Data.Proxy
    import Data.Aeson
    import Data.Aeson.KeyMap
    import Data.Maybe
    import Data.Word
    import GHC.Generics
    import Text.URI (URI)
    import Data.Map.Strict
    import Control.Concurrent
    import Control.Monad.IO.Class
    import Control.Exception (catch, SomeException)
    import Control.Lens hiding ((.=))

    -- import Network.Socket hiding (send, sendTo, recv, recvFrom)
    -- import Network.Socket.ByteString

    import Numeric (showHex)
    import qualified Text.URI  as URI
    import qualified Data.UUID as UD
    import qualified Data.Text as T
    import qualified Data.Time.Clock as T
    import qualified Data.Time.Clock.System as T
    import qualified Data.ByteString as B
    import qualified Data.ByteString.Lazy as BL
    import qualified Data.ByteString.Char8 as BC
    import qualified Network.HTTP.Req as R
    import qualified Network.HTTP.Client.MultipartFormData as R
    import qualified Network.HTTP.Client as C

    data ServerResponse a =
        ValidServerResponse   {srResponse     :: a}
        | ErrorServerResponse {srErrorMessage :: String}
        deriving (Eq, Show)

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

    requestWithNone :: ( MonadIO m
                   , R.HttpMethod method
                   , R.HttpResponse response
                   , R.HttpBodyAllowed (R.AllowsBody method) 'R.NoBody
                   , Show response
                --    , CanConstructUrl name
                   )
                => ( Proxy response
                   , response -> m a
                   )
                -> method
                -> (R.Url R.Http, R.Option R.Http)
                -> m a
    requestWithNone toResponse method urlInfo = do
        requestHttp toResponse method urlInfo R.NoReqBody
    
    requestWithJson :: ( MonadIO m
                , R.HttpMethod method
                , R.HttpResponse response
                , R.HttpBodyAllowed (R.AllowsBody method) 'R.CanHaveBody
                , ToJSON a
                , Show response
                )
            => ( Proxy response
                , response -> m b
                )
            -> method
            -> (R.Url R.Http, R.Option R.Http)
            -> a
            -> m b
    requestWithJson toResponse method urlInfo body = do
        requestHttp toResponse method urlInfo (R.ReqBodyJson body)

    asIgnore :: ( Monad m
        )
        => ( Proxy R.IgnoreResponse
        , R.IgnoreResponse -> m ()
        )
    asIgnore =
        ( R.ignoreResponse
        , const (return ())
        )

    asJSON :: ( Show a
        , Monad m
        , FromJSON a)
     => ( Proxy (R.JsonResponse a)
        , R.JsonResponse a -> m a
        )
    asJSON =
        ( R.jsonResponse
        , return
        . R.responseBody
        )

    requestHttp   :: ( MonadIO m
        , R.HttpMethod method
        , R.HttpBody bodyType
        , R.HttpResponse response
        , R.HttpBodyAllowed (R.AllowsBody method) (R.ProvidesBody bodyType)
        , Show response
       --  , CanConstructUrl name
        )
     => ( Proxy response
        , response -> m a)
     -> method
     -> (R.Url R.Http, R.Option R.Http)
     -> bodyType
     -> m a
    requestHttp (responseType, processResponse) method (url, options) bodyType = do
        response <- R.runReq R.defaultHttpConfig $ R.req method url bodyType responseType options
        processResponse response

    urlInfoFromString :: MonadIO m => String -> m (R.Url R.Http, R.Option R.Http)
    urlInfoFromString stringUrl = do
        uri <- liftIO $ URI.mkURI $ T.pack stringUrl
        let  mUrl = R.useHttpURI uri
        case mUrl of
            Nothing  -> return (R.http "", mempty)
            Just a   -> return a

    -- Sends a Request expecting a JSON response
    sendHTTPRequest   :: String -> IO String
    sendHTTPRequest http = do
        url <- urlInfoFromString (http)
        (response :: ServerResponse (Data.Map.Strict.Map T.Text String)) <- makeServerJSONRequest $ requestWithNone asJSON R.GET url
        case response of
            ValidServerResponse responseJson -> do
                return $ responseJson ! "response"
            ErrorServerResponse msg -> do error msg
                
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
        response <- catch (Right <$> request) (\(e :: SomeException) -> return $ Left $ show e)
        case response of
            Left msg -> return $ ErrorServerResponse ("Request failed: " ++ msg)
            Right _ -> return $ ValidServerResponse ()
    
    makeServerByteStringRequest :: IO B.ByteString -> IO (ServerResponse B.ByteString)
    makeServerByteStringRequest request = do
        -- catches HTTP errors and inserts them into a ServerResponse
        -- In the future it will filter HTTP status code errors to make error messages more clear.
        response <- catch (Right <$> request) (\(e :: SomeException) -> return $ Left $ show e)
        case response of
            Left  msg   -> return $ ErrorServerResponse ("Request failed: " ++ msg)
            Right bytes -> return $ ValidServerResponse bytes
    
    makeServerJSONRequest :: (FromJSON a) => IO (ServerResponse a) -> IO (ServerResponse a)
    makeServerJSONRequest request = do
        -- catches HTTP errors and inserts them into a ServerResponse
        -- In the future it will filter HTTP status code errors to make error messages more clear.
        response <- catch (Right <$> request) (\(e :: SomeException) -> return $ Left $ show e)
        case response of
            Left msg -> return $ ErrorServerResponse ("Request failed: " ++ msg)
            Right (serverResponse :: ServerResponse a) -> 
                case serverResponse of
                    ErrorServerResponse msg  -> return $ ErrorServerResponse ("Server returned error: " ++ msg)
                    ValidServerResponse resp -> return $ ValidServerResponse resp


--TCP
-- procRequests :: MVar () -> Socket -> IO ()
-- procRequests lock mastersock = do
--   (connsock, clientaddr) <- accept mastersock
--   forkIO $ procMessages lock connsock clientaddr
--   procRequests lock mastersock

-- -- Use a file handle instead of a socket. Because we keep a stick connection, we can use a file handle to abstract the reading from the socket.
-- -- Each thread reads the message from the connection

-- -- Converts a socket (connsock) to a handle
-- connhdl <- socketToHandle connsock ReadMode
-- -- Set handle to buffering mode
-- hSetBuffering connhdl LineBuffering
-- -- Read contents
-- messages <- hGetContents connhdl
-- -- Print messages
-- mapM_ (handle lock clientaddr) (lines messages)
-- -- Close connection
-- hClose connhdl

-- UDP
-- addrinfos <- getAddrInfo
--                (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
--                Nothing
--                (Just port)
-- For the publishing mode, the docs say:

-- If the AI_PASSIVE flag is not set in hints.ai_flags, then the returned socket addresses will be suitable for use with connect(2), sendto(2), or sendmsg(2)

-- In our client code we then do:

-- addrinfos <- getAddrInfo
--                Nothing
--                (Just hostname)
--                (Just port)
-- The socket() function

-- A socket is like a network file descriptor. The socket() function takes the family domain, type of socket and the protocol. It’s not clear from the docs what this protocol refers to, expect that 0 is the default protocol and it’s dependent of the address family (first parameter). [3] Suggests it’s the application layer protocol (e.g. HTTP, POP3).

-- Since we’re going to use UDP, the arguments passed to the socket function in our Haskell code are:

-- sock <- socket
--           (addrFamily serveraddr)
--           Datagram
--           defaultProtocol
-- Server: Listening
-- With the socket file descriptor, we can bind an address to it using the bind function. It takes a socket file descriptor and the address and returns 0 on success or -1 on error.
-- To receive the messages, we use the recvfrom() function, which takes the socket, the maximum size of the packet and will return the message and the address of the sender. In the Haskell version, we have recvFrom implemented in Network.Socket. The documentation has the following warning though:
-- Do not use the send and recv functions defined in this module in new code, as they incorrectly represent binary data as a Unicode string. As a result, these functions are inefficient and may lead to bugs in the program. Instead use the send and recv functions defined in the ByteString module.
-- We can use the ByteString version by doing


-- We also need to update all the places we use Strings with ByteString.

--Client: Sending data
-- From the client side, we can use the sendto() function, providing the socket file descriptor, 
--the data and the address of the server. The function will return the number of bytes sent.
-- In our Haskell code, we have:

-- sendTo (slSocket syslogh) omsg (slAddress syslogh)

