{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module HRayLib3d.ScriptEngine.HPython.HPythonHS where

  import Data.Map
  import System.IO
  import GHC.Exts
  import qualified Data.Bifunctor
  
  -- Haskell Side Representation of Ruby Data Types (HRayLib3d Inteface)
  type Identifier = String -- the name of all declarations
  type Flag       = ()     -- For Symbols (:name)
  type FlagValue  = ()     -- For Symbols (:name)
  type family     PythonObject t d -- for catching whole gems/modules or custom classes on the Ruby end 
                                 -- (IE: RubyObject Identifier [RValue] || RubyObject Identifier [RClass] || RubyObject Identifier [Identifier] || RubyObject Identifier RClass)

  data PythonInterpreterProcess = PythonInterpreterProcess {hin :: Handle, hout :: Handle}
  data PyValueHS =
    PyINT       {pyInt    :: Int}
    |PyFLOAT32  {pyFloat  :: Float}
    |PyDOUBLE   {pyDouble :: Double}
    |PyCHAR     {pyChar   :: Char}
    |PySTR      {pyStr    :: String}
    |PyLIST     {pyList   :: [PyValueHS]}
    |PyDICT     {pyDict   :: [(String, PyValueHS)]}
    deriving (Eq, Show)

  data PyModule =
    PyModule {
      rmodule            :: Identifier,
      rmodule_functions  :: Maybe [Identifier]
    } deriving (Eq, Show)

  data PyClass  =
    PyClass {
      pyClass           :: Identifier,
      instance_vars    :: Maybe (Map Identifier PyValueHS),
      symbols          :: Maybe (Map Flag FlagValue),
      pyClass_functions :: Maybe [Identifier]
    } deriving (Eq, Show)

  class ToPythonHS  a where
    toPythonHS   :: a -> PyValueHS

  class FromPythonHS a where
    fromPythonHS :: PyValueHS -> a

  instance  ToPythonHS Int where
    toPythonHS = PyINT

  instance  ToPythonHS Float where
    toPythonHS = PyFLOAT32

  instance  ToPythonHS Double where
    toPythonHS = PyDOUBLE

  instance  ToPythonHS Char where
    toPythonHS = PyCHAR

  instance  ToPythonHS String where
    toPythonHS = PySTR

  createListPythonHS :: ToPythonHS a => [a] -> PyValueHS
  createListPythonHS a = PyLIST $ Prelude.map toPythonHS a

  createDictPythonHS :: ToPythonHS b => [(String, b)] -> PyValueHS
  createDictPythonHS a = PyDICT $ Prelude.map (Data.Bifunctor.second toPythonHS) a
