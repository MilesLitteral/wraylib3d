{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE    FlexibleContexts    #-}
{-# LANGUAGE    FlexibleInstances   #-}
{-# LANGUAGE    ScopedTypeVariables #-}
{-# LANGUAGE    OverloadedStrings   #-}
module HRayLib3d.Network.FTP (searchDirectory, searchBooks, retrieveFromFTP) where

    import Control.Monad
    import System.FilePath
    import System.Directory
    import Data.Either.Extra 
    import qualified Data.Text as T

    import Control.Lens
    import Control.Exception
    import HRayLib3d.WindowSystem.Core
    -- import Control.Monad.Extra (partitionM) -- from the "extra" package
    import qualified Network.Wreq as W
    import qualified Network.Wreq.Session as Sess

    -- sess <- Sess.newAPISession
    -- The idea is that the user passes a FilePath -> Bool function to filter unwanted directories; also an initial state b and a transition function b -> FilePath -> IO b that processes file names, updates the b state and possibly has some side effects. Notice that the type of the state is chosen by the caller, who might put useful things there.
    -- If we only want to print file names as they are produced, we can do something like this:
    -- traverseDir (\_ -> True) (\() path -> print path) () "/tmp/somedir"
    -- We are using () as a dummy state because we don't really need it here.
    -- If we want to accumulate the files into a list, we can do it like this:
    -- traverseDir (\_ -> True) (\fs f -> pure (f : fs)) [] "/tmp/somedir" 

    -- | Traverse from 'top' directory and return all the files by
    -- filtering out the 'exclude' predicate.
    traverseDir :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
    traverseDir top exclude = do
        ds <- getDirectoryContents top
        paths <- forM (filter (not.exclude) ds) $ \d -> do
            let path = top </> d
            exists <- doesPathExist path
            if exists
            then traverseDir path exclude
            else return [path]
        return (concat paths)

    searchDirectory :: String -> IO BooksEvt
    searchDirectory path = do
        putStrLn . T.unpack $ "Searching For: " <> T.pack path
        result <- traverseDir path (\x -> x == path)
        return $ AppSearchDir result

    -- uncomment this function when you are ready to use it
    retrieveFromFTP :: Sess.Session -> T.Text -> IO BooksEvt
    retrieveFromFTP sess apiQuery = do
        putStrLn . T.unpack $ "Searching: " <> apiQuery
        result <- catchAny (fetch url) (return . Left . T.pack . show)
        case result of
            Right resp  -> return (FTPSearchResult resp)
            Left  err   -> return (FTPSearchError  err)
        where
            url = "https://openlibrary.org/search.json?q=" <> T.unpack apiQuery 
            checkEmpty resp
                | null (resp ^. docs) = Nothing
                | otherwise = Just resp
            fetch http = do
                resp  <- Sess.get sess http >>= W.asJSON <&> preview (W.responseBody . _Just)
                return $ maybeToEither "Empty response" (resp >>= checkEmpty)

    searchBooks :: Sess.Session -> T.Text -> IO BooksEvt
    searchBooks sess apiQuery = do
        putStrLn . T.unpack $ "Searching: " <> apiQuery
        result <- catchAny (fetch url) (return . Left . T.pack . show)
        case result of
            Right resp  -> return (BooksSearchResult resp)
            Left  err   -> return (BooksSearchError  err)
        where
        url = "https://openlibrary.org/search.json?q=" <> T.unpack apiQuery
        checkEmpty resp
            | null (resp ^. docs) = Nothing
            | otherwise = Just resp
        fetch http = do
            resp  <- (Sess.get sess http >>= W.asJSON) <&> preview (W.responseBody . _Just)
            return $ maybeToEither "Empty response" (resp >>= checkEmpty)
                
    -- Utility function to avoid the "Ambiguous type variable..." error
    catchAny :: IO a -> (SomeException -> IO a) -> IO a
    catchAny = catch