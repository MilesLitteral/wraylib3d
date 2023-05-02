{-# LANGUAGE TypeFamilies #-}
module HRayLib3d.ScriptEngine.HRubyHS where

  import Data.Map
  import System.IO

  -- Haskell Side Representation of Ruby Data Types (HRayLib3d Inteface)
  type Identifier = String -- the name of all declarations
  type Flag       = ()     -- For Symbols (:name)
  type FlagValue  = ()     -- For Symbols (:name)
  type family     RubyObject t d -- for catching whole gems/modules or custom classes on the Ruby end 
                                 -- (IE: RubyObject Identifier [RValue] || RubyObject Identifier [RClass] || RubyObject Identifier [Identifier] || RubyObject Identifier RClass)

  data RubyInterpreterProcess = RubyInterpreterProcess {hin :: Handle, hout :: Handle}
  data RValue = 
    RINT       {rint    :: Int}
    |RFLOAT32  {rfloat  :: Float}
    |RDOUBLE   {rdouble :: Double}
    |RCHAR     {rchar   :: Char}
    |RSTR      {rstr    :: String}
    |RLIST     {rlist   :: [RValue]}
    |RDICT     {rdict   :: [(String, RValue)]}
    deriving (Eq, Show)

  data RModule =
    RModule {
      rmodule            :: Identifier,
      rmodule_functions  :: Maybe [Identifier]
    } deriving (Eq, Show)

  data RClass  = 
    RClass {
      rclass           :: Identifier,
      instance_vars    :: Maybe (Map Identifier RValue),
      symbols          :: Maybe (Map Flag FlagValue),
      rclass_functions :: Maybe [Identifier]
    } deriving (Eq, Show)

  class ToRuby  a where 
    toRuby   :: a -> RValue
  
  class FromRuby a where
    fromRuby :: RValue -> a
                           