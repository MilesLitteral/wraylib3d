{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module HRayLib3d.ScriptEngine.HLua.HLuaHS where

  import Data.Map
  import System.IO
  import GHC.Exts
  import qualified Data.Bifunctor
  
  -- Haskell Side Representation of Ruby Data Types (HRayLib3d Inteface)
  type Identifier = String -- the name of all declarations
  type Flag       = ()     -- For Symbols (:name)
  type FlagValue  = ()     -- For Symbols (:name)
  type family     LuaObject t d -- for catching whole gems/modules or custom classes on the Ruby end 
                                -- (IE: RubyObject Identifier [RValue] || RubyObject Identifier [RClass] || RubyObject Identifier [Identifier] || RubyObject Identifier RClass)

  data LuaInterpreterProcess = LuaInterpreterProcess {hin :: Handle, hout :: Handle}
  data LuaValueHS =
    LUA_INT       {luaInt    :: Int}
    |LUA_FLOAT32  {luaFloat  :: Float}
    |LUA_DOUBLE   {luaDouble :: Double}
    |LUA_CHAR     {luaChar   :: Char}
    |LUA_STR      {luaStr    :: String}
    |LUA_LIST     {luaList   :: [LuaValueHS]}
    |LUA_DICT     {luaDict   :: [(String, LuaValueHS)]}
    deriving (Eq, Show)

  data LUA_Module =
    LUA_Module {
      luaModule            :: Identifier,
      luaModule_functions  :: Maybe [Identifier]
    } deriving (Eq, Show)

  data LUA_Class  =
    LUA_Class {
      luaClass           :: Identifier,
      instance_vars      :: Maybe (Map Identifier LuaValueHS),
      symbols            :: Maybe (Map Flag FlagValue),
      luaClass_functions :: Maybe [Identifier]
    } deriving (Eq, Show)

  class ToLuaHS  a where
    toLuaHS   :: a -> LuaValueHS

  class FromLuaHS a where
    fromLuaHS :: LuaValueHS -> a

  instance  ToLuaHS Int where
    toLuaHS = LUA_INT

  instance  ToLuaHS Float where
    toLuaHS = LUA_FLOAT32

  instance  ToLuaHS Double where
    toLuaHS = LUA_DOUBLE

  instance  ToLuaHS Char where
    toLuaHS = LUA_CHAR

  instance  ToLuaHS String where
    toLuaHS = LUA_STR

  createListLuaHS :: ToLuaHS a => [a] -> LuaValueHS
  createListLuaHS a = LUA_LIST $ Prelude.map toLuaHS a

  createDictLuaHS  :: ToLuaHS b => [(String, b)] -> LuaValueHS
  createDictLuaHS a = LUA_DICT $ Prelude.map (Data.Bifunctor.second toLuaHS) a
