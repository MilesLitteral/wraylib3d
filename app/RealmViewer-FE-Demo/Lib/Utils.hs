module Lib.Utils where

rerr  :: Maybe Text -> Text
rerr r  = fromMaybe "" r

indexList :: [a] -> [(Int, a)]
indexList l = zip [1..] l

-- Utility function to avoid the "Ambiguous type variable..." error
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch

remove :: Int -> [a] -> [a]
remove idx ls = take idx ls ++ drop (idx + 1) ls