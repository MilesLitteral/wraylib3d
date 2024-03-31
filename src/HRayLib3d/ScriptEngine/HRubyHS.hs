{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module HRayLib3d.ScriptEngine.HRubyHS where

  import Data.Map
  import System.IO
  import GHC.Exts
  import qualified Data.Bifunctor
  
  -- Haskell Side Representation of Ruby Data Types (HRayLib3d Inteface)
  type Identifier = String -- the name of all declarations
  type Flag       = ()     -- For Symbols (:name)
  type FlagValue  = ()     -- For Symbols (:name)
  type family     RubyObject t d -- for catching whole gems/modules or custom classes on the Ruby end 
                                 -- (IE: RubyObject Identifier [RValue] || RubyObject Identifier [RClass] || RubyObject Identifier [Identifier] || RubyObject Identifier RClass)

  data RubyInterpreterProcess = RubyInterpreterProcess {hin :: Handle, hout :: Handle}
  data RValueHS =
    RINT       {rInt    :: Int}
    |RFLOAT32  {rFloat  :: Float}
    |RDOUBLE   {rDouble :: Double}
    |RCHAR     {rChar   :: Char}
    |RSTR      {rStr    :: String}
    |RLIST     {rList   :: [RValueHS]}
    |RDICT     {rDict   :: [(String, RValueHS)]}
    deriving (Eq, Show)

  data RModule =
    RModule {
      rmodule            :: Identifier,
      rmodule_functions  :: Maybe [Identifier]
    } deriving (Eq, Show)

  data RClass  =
    RClass {
      rclass           :: Identifier,
      instance_vars    :: Maybe (Map Identifier RValueHS),
      symbols          :: Maybe (Map Flag FlagValue),
      rclass_functions :: Maybe [Identifier]
    } deriving (Eq, Show)

  class ToRubyHS  a where
    toRubyHS   :: a -> RValueHS

  class FromRubyHS a where
    fromRubyHS :: RValueHS -> a

  instance  ToRubyHS Int where
    toRubyHS = RINT

  instance  ToRubyHS Float where
    toRubyHS = RFLOAT32

  instance  ToRubyHS Double where
    toRubyHS = RDOUBLE

  instance  ToRubyHS Char where
    toRubyHS = RCHAR

  instance  ToRubyHS String where
    toRubyHS = RSTR

  createListRubyHS :: ToRubyHS a => [a] -> RValueHS
  createListRubyHS a = RLIST $ Prelude.map toRubyHS a

  createDictRubyHS :: ToRubyHS b => [(String, b)] -> RValueHS
  createDictRubyHS a = RDICT $ Prelude.map (Data.Bifunctor.second toRubyHS) a
