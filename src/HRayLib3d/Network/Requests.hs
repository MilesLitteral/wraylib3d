{-# LANGUAGE DataKinds, TypeSynonymInstances, FlexibleInstances, TemplateHaskell, ScopedTypeVariables, DeriveGeneric, OverloadedStrings  #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module HRayLib3d.Network.Requests where
    import Data.Proxy ( Proxy )
    import Data.Aeson ( decode, (.:?), FromJSON(parseJSON), Value(Object), ToJSON )
    import Data.Hashable (Hashable(..))
    import Data.Map.Strict ( (!), Map )

    import GHC.Exts ( IsList )
    import Control.Monad.IO.Class ( MonadIO(..) )
    import Control.Exception (catch, SomeException)

    import qualified Text.URI  as URI
    import qualified Data.Text as T
    import qualified Data.ByteString as B
    import qualified Data.ByteString.Lazy as BL
    import qualified Data.ByteString.Char8 as BC
    import qualified Network.HTTP.Req as R


    -- # Requests 
    -- All HTTP request related functions go here.
    -- These functions would ideally be used
    -- for multiplayer and web support settings.
    -- This module also includes all response types.

    data (IsList a) => WebRequest a = WebRequest { request :: String, response :: a } deriving (Ord, Eq, Show)

    instance Hashable (WebRequest String) where
        hashWithSalt salt lb = hashWithSalt salt (lb) 
        hash              lb = hash (lb)

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