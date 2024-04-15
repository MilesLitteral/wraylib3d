{-# language TypeApplications #-}
module Recase (recase) where

import Control.Lens.Setter (over)
import Control.Lens.Plated (transform)
import Data.Char (toUpper)

import qualified Data.Text.IO as Text

import Language.Python.Parse
import Language.Python.Optics.Idents
import Language.Python.Render (showModule)
import Language.Python.Syntax (identValue)

snakeToCamel :: String -> String
snakeToCamel =
  transform $
  \s -> case s of
    '_' : c : cs -> toUpper c : cs
    _ -> s

recase :: IO ()
recase = do
  m <- readModule "C:/Users/Manda/OneDrive/Documentos/GitHub/wraylib3d/submodules/hpython/example/snake_cased.py" -- @(ParseError SrcInfo)
  case m of
    Failure err -> error $ show err
    Success a -> do
      let fixed = over (_Idents.identValue) snakeToCamel a
      Text.putStrLn $ showModule fixed