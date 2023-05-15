{-# LANGUAGE MagicHash, DeriveDataTypeable, ScopedTypeVariables, CPP #-}
#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI,
             GHCForeignImportPrim,
             UnliftedFFITypes,
             UnboxedTuples
  #-}
#endif

module GHCJS.Prim ( JSVal(..), JSVal#
                  , JSException(..)
                  , WouldBlockException(..)
#ifdef ghcjs_HOST_OS
                  , toIO
                  , resolve
                  , resolveIO
                  , mkJSException
                  , fromJSString
                  , toJSString
                  , toJSArray
                  , fromJSArray
                  , fromJSInt
                  , toJSInt
                  , isNull
                  , isUndefined
                  , jsNull
                  , getProp
                  , getProp'
                  , getProp#
                  , unsafeGetProp
                  , unsafeGetProp'
                  , unsafeGetProp#
                  , unpackJSString#
                  , unpackJSStringUtf8#
                  , unsafeUnpackJSString#
                  , unsafeUnpackJSStringUtf8#
                  , unpackJSStringUtf8##
                  , unsafeUnpackJSStringUtf8##

#endif
                  ) where

import           Data.Typeable (Typeable)
import           Unsafe.Coerce (unsafeCoerce)

import           GHC.Prim
import qualified GHC.Exception as Ex
import qualified GHC.Exts as Exts
import qualified GHC.CString as GHC
import           GHC.IO

{-
  JSVal is a boxed type that can be used as FFI
  argument or result.
-}
#ifdef ghcjs_HOST_OS
data JSVal  = JSVal ByteArray#
type JSVal# = ByteArray#
#else
data JSVal  = JSVal Addr#
type JSVal# = Addr#
#endif

{-
  When a JavaScript exception is raised inside
  a safe or interruptible foreign call, it is converted
  to a JSException
 -}
data JSException = JSException JSVal String
  deriving (Typeable)

instance Ex.Exception JSException

instance Show JSException where
  show (JSException _ xs) = "JavaScript exception: " ++ xs

#ifdef ghcjs_HOST_OS

{-# NOINLINE toIO #-}
toIO :: Exts.Any -> IO Exts.Any
toIO x = pure x

{-# NOINLINE resolve #-}
resolve :: JSVal# -> JSVal# -> Exts.Any -> IO ()
resolve accept reject x = resolveIO accept reject (pure x)

{-# NOINLINE resolveIO #-}
resolveIO :: JSVal# -> JSVal# -> IO Exts.Any -> IO ()
resolveIO accept reject x =
  (x >>= evaluate >>= js_callback_any accept) `catch`
  (\(e::Ex.SomeException) -> do
    exceptionText <- evaluate (toJSString $ Ex.displayException e) `catch`
                         (\(_::Ex.SomeException) -> evaluate (toJSString "unknown exception"))
    js_callback_jsval reject exceptionText)

mkJSException :: JSVal -> IO JSException
mkJSException ref =
  return (JSException (unsafeCoerce ref) (fromJSString ref))

{- | Low-level conversion utilities for packages that cannot
     depend on ghcjs-base
 -}

{- | returns an empty string if the JSVal does not contain
     a string
 -}
fromJSString :: JSVal -> String
fromJSString = unsafeCoerce . js_fromJSString
{-# INLINE fromJSString #-}

toJSString :: String -> JSVal
toJSString = js_toJSString . unsafeCoerce . seqList
{-# INLINE [0] toJSString #-}
{-# RULES
"GHCJSPRIM toJSString/literal" forall a.
  toJSString (GHC.unpackCString# a) = JSVal (unsafeUnpackJSStringUtf8## a)
"GHCJSPRIM toJSString/literalUtf8" forall a.
  toJSString (GHC.unpackCStringUtf8# a) = JSVal (unsafeUnpackJSStringUtf8## a)
  #-}

fromJSArray :: JSVal -> IO [JSVal]
fromJSArray = unsafeCoerce . js_fromJSArray
{-# INLINE fromJSArray #-}

toJSArray :: [JSVal] -> IO JSVal
toJSArray = js_toJSArray . unsafeCoerce . seqList
{-# INLINE toJSArray #-}

{- | returns zero if the JSVal does not contain a number
 -}
fromJSInt :: JSVal -> Int
fromJSInt = js_fromJSInt
{-# INLINE fromJSInt #-}

toJSInt :: Int -> JSVal
toJSInt = js_toJSInt
{-# INLINE toJSInt #-}

isNull :: JSVal -> Bool
isNull = js_isNull
{-# INLINE isNull #-}

isUndefined :: JSVal -> Bool
isUndefined = js_isUndefined
{-# INLINE isUndefined #-}

jsNull :: JSVal
jsNull = js_null
{-# INLINE CONLIKE jsNull #-}

getProp :: JSVal -> String -> IO JSVal
getProp o p = js_getProp o (unsafeCoerce $ seqList p)
{-# INLINE [0] getProp #-}
{-# RULES
"GHCJSPRIM getProp/literal" forall o a.
  getProp o (GHC.unpackCString# a) = getProp# o a
"GHCJSPRIM getProp/literalUtf8" forall o a.
  getProp o (GHC.unpackCStringUtf8# a) = getPropUtf8# o a
  #-}

-- | only safe on immutable object
unsafeGetProp :: JSVal -> String -> JSVal
unsafeGetProp o p = js_unsafeGetProp o (unsafeCoerce $ seqList p)
{-# INLINE [0] unsafeGetProp #-}
{-# RULES
"GHCJSPRIM unsafeGetProp/literal" forall o a.
  unsafeGetProp o (GHC.unpackCString# a) = unsafeGetProp# o a
"GHCJSPRIM unsafeGetProp/literalUtf8" forall o a.
  unsafeGetProp o (GHC.unpackCStringUtf8# a) = unsafeGetPropUtf8# o a
  #-}

getProp' :: JSVal -> JSVal -> IO JSVal
getProp' o p = js_getProp' o p
{-# INLINE [0] getProp' #-}
{-# RULES
"GHCJSPRIM getProp'/literal" forall o a.
  getProp' o (unsafeUnpackJSString# a) = getProp# o a
"GHCJSPRIM getProp'/literalUtf8" forall o a.
  getProp' o (unsafeUnpackJSStringUtf8# a) = getPropUtf8# o a
  #-}

-- | only safe on immutable object
unsafeGetProp' :: JSVal -> JSVal -> JSVal
unsafeGetProp' o p = js_unsafeGetProp' o p
{-# INLINE [0] unsafeGetProp' #-}
{-# RULES
"GHCJSPRIM unsafeGetProp'/literal" forall o a.
  unsafeGetProp' o (unsafeUnpackJSString# a) = unsafeGetPropUtf8# o a
"GHCJSPRIM unsafeGetProp'/literalUtf8" forall o a.
  unsafeGetProp' o (unsafeUnpackJSStringUtf8# a) = unsafeGetPropUtf8# o a
  #-}


-- | only safe on immutable Addr#
getProp# :: JSVal -> Addr# -> IO JSVal
getProp# (JSVal o) p = IO $
  \s -> case getPropUtf8## o p s of (# s', v #) -> (# s', JSVal v #)
{-# INLINE [0] getProp# #-}
-- js_getProp# o p

-- | only safe on immutable Addr#
getPropUtf8# :: JSVal -> Addr# -> IO JSVal
getPropUtf8# (JSVal o) p = IO $
  \s -> case getPropUtf8## o p s of (# s', v #) -> (# s', JSVal v #)
{-# INLINE [0] getPropUtf8# #-}

getPropUtf8## :: JSVal# -> Addr# -> State# s -> (# State# s, JSVal# #)
getPropUtf8## o p = js_getPropUtf8## o p
{-# NOINLINE getPropUtf8## #-}

-- | only safe on immutable Addr# and JSVal
unsafeGetProp# :: JSVal -> Addr# -> JSVal
unsafeGetProp# (JSVal o) p = JSVal (unsafeGetPropUtf8## o p)
{-# INLINE [0] unsafeGetProp# #-}

-- | only safe on immutable Addr# and JSVal
unsafeGetPropUtf8# :: JSVal -> Addr# -> JSVal
unsafeGetPropUtf8# (JSVal o) p = JSVal (unsafeGetPropUtf8## o p)
{-# INLINE [0] unsafeGetPropUtf8# #-}

unsafeGetPropUtf8## :: JSVal# -> Addr# -> JSVal#
unsafeGetPropUtf8## o p = js_unsafeGetPropUtf8## o p
{-# NOINLINE unsafeGetPropUtf8## #-}

unpackJSString# :: Addr# -> IO JSVal
unpackJSString# a = IO $
  \s -> case unpackJSStringUtf8## a s of (# s', v #) -> (# s', JSVal v #)
{-# INLINE [0] unpackJSString# #-}

unpackJSStringUtf8# :: Addr# -> IO JSVal
unpackJSStringUtf8# a = IO $
  \s -> case unpackJSStringUtf8## a s of (# s', v #) -> (# s', JSVal v #)
{-# INLINE [0] unpackJSStringUtf8# #-}

unpackJSStringUtf8## :: Addr# -> State# s -> (# State# s, JSVal# #)
unpackJSStringUtf8## a s = js_unpackJSStringUtf8## a s
{-# NOINLINE unpackJSStringUtf8## #-}

-- | only safe on immutable Addr#
unsafeUnpackJSString# :: Addr# -> JSVal
unsafeUnpackJSString# a = JSVal (unsafeUnpackJSStringUtf8## a)
  -- js_unsafeUnpackJSString# a
{-# INLINE [0] unsafeUnpackJSString# #-}

-- | only safe on immutable Addr#
unsafeUnpackJSStringUtf8# :: Addr# -> JSVal
unsafeUnpackJSStringUtf8# a = JSVal (unsafeUnpackJSStringUtf8## a)
{-# INLINE [0] unsafeUnpackJSStringUtf8# #-}

unsafeUnpackJSStringUtf8## :: Addr# -> JSVal#
unsafeUnpackJSStringUtf8## a = js_unsafeUnpackJSStringUtf8## a
{-# NOINLINE unsafeUnpackJSStringUtf8## #-}


-- reduce the spine and all list elements to whnf
seqList :: [a] -> [a]
seqList xs = go xs `seq` xs
  where go (x:xs) = x `seq` go xs
        go []     = ()

seqListSpine :: [a] -> [a]
seqListSpine xs = go xs `seq` xs
  where go (x:xs) = go xs
        go []     = ()

foreign import javascript unsafe "h$toHsString($1)"
  js_fromJSString :: JSVal -> Exts.Any

foreign import javascript unsafe "h$fromHsString($1)"
  js_toJSString :: Exts.Any -> JSVal

foreign import javascript unsafe "h$toHsListJSVal($1)"
  js_fromJSArray :: JSVal -> IO Exts.Any

foreign import javascript unsafe "h$fromHsListJSVal($1)"
  js_toJSArray :: Exts.Any -> IO JSVal

foreign import javascript unsafe "$1 === null"
  js_isNull :: JSVal -> Bool

foreign import javascript unsafe "$1 === undefined"
  js_isUndefined :: JSVal -> Bool

foreign import javascript unsafe "$r = typeof($1) === 'number' ? ($1|0) : 0;"
  js_fromJSInt :: JSVal -> Int

foreign import javascript unsafe "$r = $1;"
  js_toJSInt :: Int -> JSVal

foreign import javascript unsafe "$r = null;"
  js_null :: JSVal

foreign import javascript unsafe "$1[h$fromHsString($2)]"
  js_getProp :: JSVal -> Exts.Any -> IO JSVal

foreign import javascript unsafe "$1[h$fromHsString($2)]"
  js_unsafeGetProp :: JSVal -> Exts.Any -> JSVal

foreign import javascript unsafe "$1[$2]"
  js_getProp' :: JSVal -> JSVal -> IO JSVal

foreign import javascript unsafe "$1[$2]"
  js_unsafeGetProp' :: JSVal -> JSVal -> JSVal

foreign import javascript unsafe "$1[h$decodeUtf8z($2_1, $2_2)]"
  js_getPropUtf8## :: JSVal# -> Addr# -> State# s -> (# State# s, JSVal# #)

foreign import javascript unsafe "$1[h$decodeUtf8z($2_1, $2_2)]"
  js_unsafeGetPropUtf8## :: JSVal# -> Addr# -> JSVal#

foreign import javascript unsafe "h$decodeUtf8z($1_1, $1_2)"
  js_unpackJSStringUtf8## :: Addr# -> State# s -> (# State# s, JSVal# #)


foreign import javascript unsafe "h$decodeUtf8z($1_1, $1_2)"
  js_unsafeUnpackJSStringUtf8## :: Addr# -> JSVal#

foreign import javascript unsafe "$1($2)"
  js_callback_any :: JSVal# -> Exts.Any -> IO ()

foreign import javascript unsafe "$1($2)"
  js_callback_jsval :: JSVal# -> JSVal -> IO ()

#endif

{- | If a synchronous thread tries to do something that can only
     be done asynchronously, and the thread is set up to not
     continue asynchronously, it receives this exception.
 -}
data WouldBlockException = WouldBlockException
  deriving (Typeable)

instance Show WouldBlockException where
  show _ = "thread would block"

instance Ex.Exception WouldBlockException
