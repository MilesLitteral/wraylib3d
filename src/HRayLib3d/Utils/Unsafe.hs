{-# LANGUAGE MagicHash, UnboxedTuples #-}

module HRayLib3d.Utils.Unsafe
   ( inlinePerformIO,
   )
where

-- import GHC.Prelude.Basic ()

import GHC.Exts
import GHC.IO   (IO(..))

-- Just like unsafeDupablePerformIO, but we inline it.
{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #)   -> r