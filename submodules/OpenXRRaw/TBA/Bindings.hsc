{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances, CPP #-}

#include "shim.h"
#include <ruby.h>
module HRayLib3d.ScriptEngine.Bindings where

import Foreign
import Foreign.C

-- The Foreign Interface
type FRValue  = Ptr CULong

-- | The Ruby ID type, mostly used for symbols.
type RID = CULong

-- Uncomment when you can put RubyC entirely in cbits and build it correctly (therefore embedding Ruby in this Engine)
-- You may need to make a Library especially configured to work with GHC in this
-- case

-- data ShimDispatch = ShimDispatch FRValue RID [FRValue]
-- instance Storable ShimDispatch where
--     sizeOf _ = (#size struct s_dispatch)
--     alignment = sizeOf
--     peek ptr = do a <- peek ((#ptr struct s_dispatch, receiver) ptr)
--                   b <- peek ((#ptr struct s_dispatch, methodid) ptr)
--                   return (ShimDispatch a b [])
--     poke ptr (ShimDispatch c m vals) = do
--         (#poke struct s_dispatch, receiver) ptr c
--         (#poke struct s_dispatch, methodid) ptr m
--         (#poke struct s_dispatch, nbargs) ptr (length vals)
--         let arrayblock = (#ptr struct s_dispatch, args) ptr
--         pokeArray arrayblock vals

-- -- | The ruby built-in types
-- data RBuiltin = RNONE
--               | RNIL
--               | ROBJECT
--               | RCLASS
--               | RICLASS
--               | RMODULE
--               | RFLOAT
--               | RSTRING
--               | RREGEXP
--               | RARRAY
--               | RFIXNUM
--               | RHASH
--               | RSTRUCT
--               | RBIGNUM
--               | RFILE
--               | RTRUE
--               | RFALSE
--               | RDATA
--               | RMATCH
--               | RSYMBOL
--               | RUNDEF
--               | RNODE
--               deriving (Show)

-- -- | Ruby native types, as encoded in the Value type.
-- data RType = RFixNum
--            | RNil
--            | RFalse
--            | RTrue
--            | RSymbol
--            | RUndef
--            | RBuiltin RBuiltin
--            deriving (Show)

-- type Registered0 = IO FRValue
-- type Registered1 = FRValue -> IO FRValue
-- type Registered2 = FRValue -> FRValue -> IO FRValue

-- -- | Creates a function pointer suitable for usage with `rb_define_global_function` of type `Registered0` (with 0 arguments).
-- foreign import ccall "wrapper" mkRegistered0 :: Registered0 -> IO (FunPtr Registered0)
-- -- | Creates a function pointer suitable for usage with `rb_define_global_function` of type `Registered1` (with 1 `FRValue` arguments).
-- foreign import ccall "wrapper" mkRegistered1 :: Registered1 -> IO (FunPtr Registered1)
-- -- | Creates a function pointer suitable for usage with `rb_define_global_function` of type `Registered2` (with 2 `FRValue` arguments).
-- foreign import ccall "wrapper" mkRegistered2 :: Registered2 -> IO (FunPtr Registered2)

-- type RegisteredCB3 = FRValue -> FRValue -> FRValue -> IO Int
-- foreign import ccall "wrapper" mkRegisteredCB3 :: RegisteredCB3 -> IO (FunPtr RegisteredCB3)

-- foreign import ccall "ruby_finalize"                    ruby_finalize               :: IO ()
-- foreign import ccall "ruby_initialization"              ruby_initialization         :: IO ()
-- foreign import ccall "rb_str_new_cstr"                  c_rb_str_new2               :: CString -> IO FRValue
-- foreign import ccall "rb_ary_new_capa"                  rb_ary_new2                 :: CLong -> IO FRValue
-- foreign import ccall "rb_ary_new_from_values"           rb_ary_new4                 :: CLong -> Ptr FRValue -> IO FRValue
-- foreign import ccall   safe "rb_load_protect"           c_rb_load_protect           :: FRValue -> Int -> Ptr Int -> IO ()
-- foreign import ccall   safe "rb_funcall"                c_rb_funcall_0              :: FRValue -> RID -> Int -> IO FRValue
-- foreign import ccall   safe "rb_funcall"                c_rb_funcall_1              :: FRValue -> RID -> Int -> FRValue -> IO FRValue
-- foreign import ccall   safe "rb_funcall"                c_rb_funcall_2              :: FRValue -> RID -> Int -> FRValue -> FRValue -> IO FRValue
-- foreign import ccall   safe "rb_funcall"                c_rb_funcall_3              :: FRValue -> RID -> Int -> FRValue -> FRValue -> FRValue -> IO FRValue
-- foreign import ccall   safe "rb_funcall"                c_rb_funcall_4              :: FRValue -> RID -> Int -> FRValue -> FRValue -> FRValue -> FRValue -> IO FRValue
-- foreign import ccall   safe "rb_funcall"                c_rb_funcall_5              :: FRValue -> RID -> Int -> FRValue -> FRValue -> FRValue -> FRValue -> FRValue -> IO FRValue
-- foreign import ccall   safe "rb_funcall_with_block"     c_rb_funcall_with_block     :: FRValue -> RID -> Int -> Ptr FRValue -> FRValue -> IO FRValue
-- foreign import ccall unsafe "rb_gv_get"                 c_rb_gv_get                 :: CString -> IO FRValue
-- foreign import ccall unsafe "rb_intern"                 c_rb_intern                 :: CString -> IO RID
-- foreign import ccall unsafe "rb_id2name"                rb_id2name                  :: RID -> IO CString
-- foreign import ccall unsafe "rb_string_value_ptr"       c_rb_string_value_ptr       :: Ptr FRValue -> IO CString
-- foreign import ccall unsafe "&rb_cObject"               rb_cObject                  :: Ptr FRValue
-- foreign import ccall unsafe "rb_iv_set"                 c_rb_iv_set                 :: FRValue -> CString -> FRValue -> IO FRValue
-- foreign import ccall   safe "rb_define_class"           c_rb_define_class           :: CString -> FRValue -> IO FRValue
-- foreign import ccall   safe "rb_define_method"          c_rb_define_method          :: FRValue -> CString -> FunPtr a -> Int -> IO ()
-- foreign import ccall   safe "rb_define_singleton_method" c_rb_define_singleton_method :: FRValue -> CString -> FunPtr a -> Int -> IO ()
-- foreign import ccall   safe "rb_define_module_function" c_rb_define_module_function :: FRValue -> CString -> FunPtr a -> Int -> IO ()
-- foreign import ccall   safe "rb_define_global_function" c_rb_define_global_function :: CString -> FunPtr a -> Int -> IO ()
-- foreign import ccall unsafe "rb_const_get"              rb_const_get                :: FRValue -> RID -> IO FRValue
-- foreign import ccall   safe "&safeCall"                 safeCallback                :: FunPtr (FRValue -> IO FRValue)
-- foreign import ccall   safe "&getRubyCObject"           getRubyCObjectCallback      :: FunPtr (FRValue -> IO FRValue)
-- foreign import ccall   safe "rb_protect"                c_rb_protect                :: FunPtr (FRValue -> IO FRValue) -> FRValue -> Ptr Int -> IO FRValue
-- foreign import ccall unsafe "rb_string_value_cstr"      c_rb_string_value_cstr      :: Ptr FRValue -> IO CString
-- foreign import ccall unsafe "rb_ary_new"                rb_ary_new                  :: IO FRValue
-- foreign import ccall unsafe "rb_ary_push"               rb_ary_push                 :: FRValue -> FRValue -> IO FRValue
-- foreign import ccall unsafe "rb_ary_entry"              rb_ary_entry                :: FRValue -> CLong -> IO FRValue
-- foreign import ccall   safe "rb_hash_foreach"           rb_hash_foreach             :: FRValue -> FunPtr a -> FRValue -> IO ()
-- foreign import ccall unsafe "rb_big2str"                rb_big2str                  :: FRValue -> CInt -> IO FRValue
-- foreign import ccall unsafe "rb_cstr_to_inum"           rb_cstr_to_inum             :: CString -> CInt -> CInt -> IO FRValue
-- foreign import ccall unsafe "newFloat"                  newFloat                    :: Double -> IO FRValue
-- foreign import ccall unsafe "rb_hash_new"               rb_hash_new                 :: IO FRValue
-- foreign import ccall unsafe "rb_hash_aset"              rb_hash_aset                :: FRValue -> FRValue -> FRValue -> IO FRValue
-- foreign import ccall unsafe "rb_define_module"          c_rb_define_module          :: CString -> IO ()

-- foreign import ccall unsafe "arrayLength"        arrayLength                 :: FRValue -> IO CLong
-- foreign import ccall unsafe "rubyType"           rubyType                    :: FRValue -> IO CInt
-- foreign import ccall unsafe "num2dbl"            num2dbl                     :: FRValue -> IO Double
-- foreign import ccall unsafe "int2num"            int2num                     :: CLong  -> IO FRValue
-- foreign import ccall unsafe "num2long"           num2long                    :: FRValue -> IO CLong
-- foreign import ccall unsafe "id2sym"             id2sym                      :: RID -> FRValue
-- foreign import ccall unsafe "sym2id"             sym2id                      :: FRValue -> RID

-- rbFalse,rbTrue,rbNil,rbUndef :: FRValue
-- rbFalse = intPtrToPtr 0x00
-- rbTrue  = intPtrToPtr 0x14
-- rbNil   = intPtrToPtr 0x08
-- rbUndef = intPtrToPtr 0x34

-- rtype :: FRValue -> IO RType
-- rtype v = rubyType v >>= \x -> case x of
--     (#const RUBY_T_NONE)   -> return RUndef
--     (#const RUBY_T_OBJECT) -> return (RBuiltin ROBJECT)
--     (#const RUBY_T_CLASS)  -> return (RBuiltin RCLASS)
--     (#const RUBY_T_MODULE) -> return (RBuiltin RMODULE)
--     (#const RUBY_T_FLOAT)  -> return (RBuiltin RFLOAT)
--     (#const RUBY_T_STRING) -> return (RBuiltin RSTRING)
--     (#const RUBY_T_REGEXP) -> return (RBuiltin RREGEXP)
--     (#const RUBY_T_ARRAY)  -> return (RBuiltin RARRAY)
--     (#const RUBY_T_HASH)   -> return (RBuiltin RHASH)
--     (#const RUBY_T_STRUCT) -> return (RBuiltin RSTRUCT)
--     (#const RUBY_T_BIGNUM) -> return (RBuiltin RBIGNUM)
--     (#const RUBY_T_FILE)   -> return (RBuiltin RFILE)
--     (#const RUBY_T_DATA)   -> return (RBuiltin RDATA)
--     (#const RUBY_T_MATCH)  -> return (RBuiltin RMATCH)
--     (#const RUBY_T_NIL)    -> return RNil
--     (#const RUBY_T_TRUE)   -> return RTrue
--     (#const RUBY_T_FALSE)  -> return RFalse
--     (#const RUBY_T_SYMBOL) -> return RSymbol
--     (#const RUBY_T_FIXNUM) -> return RFixNum
--     (#const RUBY_T_UNDEF)  -> return (RBuiltin RUNDEF)
--     (#const RUBY_T_NODE)   -> return (RBuiltin RNODE)
--     (#const RUBY_T_ICLASS) -> return (RBuiltin RICLASS)
--     _                      -> return RUndef

-- rb_string_value_cstr :: FRValue -> IO String
-- rb_string_value_cstr v = do
--     pv <- new v
--     o <- c_rb_string_value_cstr pv >>= peekCString
--     free pv
--     return o

-- -- | Defines a global function that can be called from the Ruby world. This function must only accept `FRValue`s as arguments.
-- rb_define_global_function :: String -- ^ Name of the function
--                           -> FunPtr a -- ^ Pointer to the function (created with something like `mkRegistered0`)
--                           -> Int -- ^ Number of arguments the function accepts.
--                           -> IO ()
-- rb_define_global_function s f i = withCString s (\cs -> c_rb_define_global_function cs f i)

-- -- | Defines an instance method.
-- --
-- -- The Haskell function must accept the receiver as the first argument.
-- -- If @argc >= 0@, the type of the arguments of the function must be `FRValue`.
-- rb_define_method
--   :: FRValue   -- ^ Object to which a method is added
--   -> String   -- ^ Name of the method
--   -> FunPtr a -- ^ Pointer to the Haskell function (created with something like `mkRegistered1`)
--   -> Int      -- ^ @-1@, @-2@, or number of arguments the function accepts other than the receiver.
--   -> IO ()
-- rb_define_method r s f i = withCString s (\cs -> c_rb_define_method r cs f i)

-- -- | Defines a singleton method.
-- --
-- -- Arguments are the same as 'rb_define_method'.
-- rb_define_singleton_method :: FRValue -> String -> FunPtr a -> Int -> IO ()
-- rb_define_singleton_method r s f i = withCString s (\cs -> c_rb_define_singleton_method r cs f i)

-- -- | Defines a module function (@module_function@) to a module.
-- --
-- -- Arguments are the same as 'rb_define_method'.
-- rb_define_module_function :: FRValue -> String -> FunPtr a -> Int -> IO ()
-- rb_define_module_function r s f i = withCString s (\cs -> c_rb_define_module_function r cs f i)

-- rb_define_class :: String  -> FRValue -> IO FRValue
-- rb_define_class str rv = withCString str (\s -> c_rb_define_class s rv)

-- rb_str_new2 :: String -> IO FRValue
-- rb_str_new2 str = withCString str c_rb_str_new2

-- rb_define_module :: String -> IO ()
-- rb_define_module str = withCString str c_rb_define_module

-- -- | Sets an instance variable
-- rb_iv_set :: FRValue -- ^ The object
--           -> String -- ^ The variable name (without the @)
--           -> FRValue -- ^ The value to set
--           -> IO FRValue -- ^ The value you just set
-- rb_iv_set obj varname vaFRValue = withCString ('@' : varname) $ \cvarname -> c_rb_iv_set obj cvarname vaFRValue

-- -- | Loads a ruby script (and executes it).
-- rb_load_protect :: String -- ^ Path to the script
--                 -> Int -- ^ Just set this to 0, unless you know what you are doing
--                 -> IO Int -- ^ Return code, equal to 0 if everything went right. `showErrorStack` can be used in case of errors.
-- rb_load_protect rv a = do
--     bptr <- new 0
--     rvs <- rb_str_new2 rv
--     c_rb_load_protect rvs a bptr
--     status <- peek bptr
--     free bptr
--     return status

-- rb_funcall :: FRValue -> RID -> [FRValue] -> IO FRValue
-- rb_funcall a b []          = c_rb_funcall_0 a b 0
-- rb_funcall a b [d]         = c_rb_funcall_1 a b 1 d
-- rb_funcall a b [d,e]       = c_rb_funcall_2 a b 2 d e
-- rb_funcall a b [d,e,f]     = c_rb_funcall_3 a b 3 d e f
-- rb_funcall a b [d,e,f,g]   = c_rb_funcall_4 a b 4 d e f g
-- rb_funcall a b [d,e,f,g,h] = c_rb_funcall_5 a b 5 d e f g h
-- rb_funcall _ _ _           = error "Can't call functions with that many arguments"

-- rbMethodCall :: String -> String -> [FRValue] -> IO FRValue
-- rbMethodCall classname methodname args = do
--     c <- getClass classname
--     m <- rb_intern methodname
--     rb_funcall c m args

-- getClass :: String -> IO FRValue
-- getClass s = do
--     i <- rb_intern s
--     o <- peek rb_cObject
--     rb_const_get o i

-- rb_gv_get :: String -> IO FRValue
-- rb_gv_get s = withCString s c_rb_gv_get

-- rb_intern :: String -> IO RID
-- rb_intern s = withCString s c_rb_intern

-- rb_string_value_ptr :: FRValue -> IO String
-- rb_string_value_ptr rv = do
--     rvp <- new rv
--     o <- c_rb_string_value_ptr rvp >>= peekCString
--     free rvp
--     return o