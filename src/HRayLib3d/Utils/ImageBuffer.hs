{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HRayLib3d.Utils.ImageBuffer where

    import Monomer
    import Data.Text hiding (take)
    import Data.Word
    import qualified Data.ByteString as BS 

    data ImageBuffer = ImageBuffer {
        name    :: Text,
        bytes   :: BS.ByteString,
        size    :: Size
    } deriving (Eq, Show)

    emptyImageBuffer :: ImageBuffer
    emptyImageBuffer = ImageBuffer "" BS.empty (Size 0 0)

    defaultImageBuffer :: ImageBuffer
    defaultImageBuffer = do
        let v :: [Word8] = take (512 * 512 * 4) [255, 255..]
        ImageBuffer "Default"    (BS.pack v) (Size 512 512) 