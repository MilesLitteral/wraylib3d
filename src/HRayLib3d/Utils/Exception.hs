module HRayLib3d.Utils.Exception (try', catchAny) where

    import Control.Exception

    try' :: IO a ->  IO (Either IOException a)
    try' =  try 

    -- Avoid the "Ambiguous type variable..." error
    catchAny :: IO a -> (SomeException -> IO a) -> IO a
    catchAny = catch