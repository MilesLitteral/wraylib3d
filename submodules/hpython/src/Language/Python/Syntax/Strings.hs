{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}

{-|
Module      : Language.Python.Syntax.Strings
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable

Python string literals.

See <https://docs.python.org/3.5/reference/lexical_analysis.html#string-and-bytes-literals>
-}

module Language.Python.Syntax.Strings
  ( -- * Datatypes
    -- ** Characters
    PyChar(..)
  , fromHaskellString
    -- ** String information
  , QuoteType(..)
  , StringType(..)
    -- ** String prefixes
  , StringPrefix(..)
  , RawStringPrefix(..)
  , BytesPrefix(..)
  , RawBytesPrefix(..)
  , hasPrefix
    -- ** String literals
  , StringLiteral(..)
    -- *** Lenses
  , stringLiteralStringType
  , stringLiteralQuoteType
  , stringLiteralValue
  , stringLiteralWhitespace
    -- * Rendering
  , showQuoteType
  , showStringPrefix
  , showRawStringPrefix
  , showBytesPrefix
  , showRawBytesPrefix
    -- * Extra functions
  , isEscape
  )
where

import Control.Lens.Lens (Lens', lens)
import Control.Lens.TH (makeLensesFor)
import Data.Digit.Octal (OctDigit)
import Data.Digit.Hexadecimal.MixedCase (HeXDigit(..))
import Data.Generics.Product.Typed (typed)
import Data.Maybe (isJust)
import Data.Text (Text)
import GHC.Generics (Generic)

import Language.Python.Syntax.Ann
import Language.Python.Syntax.Whitespace

-- | Double or single quotation marks?
--
-- @
-- "Double quotes"
-- """Double quotes"""
-- 'Single quotes'
-- '''Single quotes'''
-- @
data QuoteType
  = SingleQuote
  | DoubleQuote
  deriving (Eq, Ord, Show, Generic)

-- | Three pairs of quotations or one?
--
-- @
-- """Long string"""
-- '''Also long string'''
-- "Short string"
-- 'Also short string'
-- @
data StringType
  = ShortString
  | LongString
  deriving (Eq, Ord, Show, Generic)

-- | In Python 3.5, a prefix of @u@ or @U@ is allowed, but doesn't have any
-- meaning. They exist for backwards compatibility with Python 2.
--
-- See <https://www.python.org/dev/peps/pep-0414/>
data StringPrefix
  = Prefix_u
  | Prefix_U
  deriving (Eq, Ord, Show, Generic)

-- | Raw strings are prefixed with either @r@ or @R@.
data RawStringPrefix
  = Prefix_r
  | Prefix_R
  deriving (Eq, Ord, Show, Generic)

-- | This prefix indicates it's a bytes literal rather than a string literal.
data BytesPrefix
  = Prefix_b
  | Prefix_B
  deriving (Eq, Ord, Show, Generic)

-- | A string of raw bytes can be indicated by a number of prefixes
data RawBytesPrefix
  = Prefix_br
  | Prefix_Br
  | Prefix_bR
  | Prefix_BR
  | Prefix_rb
  | Prefix_rB
  | Prefix_Rb
  | Prefix_RB
  deriving (Eq, Ord, Show, Generic)

-- | Most types of 'StringLiteral' have prefixes. Plain old strings may have
-- an optional prefix, but it is meaningless.
hasPrefix :: StringLiteral a -> Bool
hasPrefix RawStringLiteral{} = True
hasPrefix RawBytesLiteral{} = True
hasPrefix (StringLiteral _ a _ _ _ _) = isJust a
hasPrefix BytesLiteral{} = True

-- | A 'StringLiteral', complete with a prefix, information about
-- quote type and number, and a list of 'PyChar's.
--
-- Like many other data types in hpython, it has an annotation and
-- trailing whitespace.
data StringLiteral a
  = RawStringLiteral
  { _stringLiteralAnn :: Ann a
  , _unsafeRawStringLiteralPrefix :: RawStringPrefix
  , _stringLiteralStringType :: StringType
  , _stringLiteralQuoteType :: QuoteType
  , _stringLiteralValue :: [PyChar]
  , _stringLiteralWhitespace :: [Whitespace]
  }
  | StringLiteral
  { _stringLiteralAnn :: Ann a
  , _unsafeStringLiteralPrefix :: Maybe StringPrefix
  , _stringLiteralStringType :: StringType
  , _stringLiteralQuoteType :: QuoteType
  , _stringLiteralValue :: [PyChar]
  , _stringLiteralWhitespace :: [Whitespace]
  }
  | RawBytesLiteral
  { _stringLiteralAnn :: Ann a
  , _unsafeRawBytesLiteralPrefix :: RawBytesPrefix
  , _stringLiteralStringType :: StringType
  , _stringLiteralQuoteType :: QuoteType
  , _stringLiteralValue :: [PyChar]
  , _stringLiteralWhitespace :: [Whitespace]
  }
  | BytesLiteral
  { _stringLiteralAnn :: Ann a
  , _unsafeBytesLiteralPrefix :: BytesPrefix
  , _stringLiteralStringType :: StringType
  , _stringLiteralQuoteType :: QuoteType
  , _stringLiteralValue :: [PyChar]
  , _stringLiteralWhitespace :: [Whitespace]
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance HasAnn StringLiteral where
  annot :: forall a. Lens' (StringLiteral a) (Ann a)
  annot = typed @(Ann a)

instance HasTrailingWhitespace (StringLiteral a) where
  trailingWhitespace =
    lens
      (\case
          RawStringLiteral _ _ _ _ _ ws -> ws
          StringLiteral _ _ _ _ _ ws -> ws
          RawBytesLiteral _ _ _ _ _ ws -> ws
          BytesLiteral _ _ _ _ _ ws -> ws)
      (\s ws -> case s of
          StringLiteral a b c d e _ -> StringLiteral a b c d e ws
          RawStringLiteral a b c d e _ -> RawStringLiteral a b c d e ws
          BytesLiteral a b c d e _ -> BytesLiteral a b c d e ws
          RawBytesLiteral a b c d e _ -> RawBytesLiteral a b c d e ws)

-- | A character in a string literal. This is a large sum type, with a
-- catch-all of a Haskell 'Char'.
data PyChar
  -- | @\\newline@
  = Char_newline
  -- | @\\1@
  | Char_octal1 OctDigit
  -- | @\\12@
  | Char_octal2 OctDigit OctDigit
  -- | @\\123@
  | Char_octal3 OctDigit OctDigit OctDigit
  -- | @\\xFb@
  | Char_hex HeXDigit HeXDigit
  -- | @\\u12aD@
  | Char_uni16
      HeXDigit
      HeXDigit
      HeXDigit
      HeXDigit
  -- | @\\Udeadbeef@
  | Char_uni32
      HeXDigit
      HeXDigit
      HeXDigit
      HeXDigit
      HeXDigit
      HeXDigit
      HeXDigit
      HeXDigit
  -- | @\\\\@
  | Char_esc_bslash
  -- | @\\\'@
  | Char_esc_singlequote
  -- | @\\\"@
  | Char_esc_doublequote
  -- | @\\a@
  | Char_esc_a
  -- | @\\b@
  | Char_esc_b
  -- | @\\f@
  | Char_esc_f
  -- | @\\n@
  | Char_esc_n
  -- | @\\r@
  | Char_esc_r
  -- | @\\t@
  | Char_esc_t
  -- | @\\v@
  | Char_esc_v
  -- | Any character
  | Char_lit Char
  deriving (Eq, Ord, Show, Generic)

-- | Determine whether a 'PyChar' is an escape character or not.
isEscape :: PyChar -> Bool
isEscape c =
  case c of
    Char_newline -> True
    Char_octal1{} -> True
    Char_octal2{} -> True
    Char_octal3{} -> True
    Char_hex{} -> True
    Char_uni16{} -> True
    Char_uni32{} -> True
    Char_esc_bslash -> True
    Char_esc_singlequote -> True
    Char_esc_doublequote -> True
    Char_esc_a -> True
    Char_esc_b -> True
    Char_esc_f -> True
    Char_esc_n -> True
    Char_esc_r -> True
    Char_esc_t -> True
    Char_esc_v -> True
    Char_lit{} -> False

-- | Convert a Haskell string to a list of 'PyChar'. This is useful when
-- writing Python in Haskell.
fromHaskellString :: String -> [PyChar]
fromHaskellString =
  fmap
  (\c -> case c of
    '\\' -> Char_esc_bslash
    '\'' -> Char_esc_singlequote
    '\"' -> Char_esc_doublequote
    '\a' -> Char_esc_a
    '\b' -> Char_esc_b
    '\f' -> Char_esc_f
    '\n' -> Char_esc_n
    '\r' -> Char_esc_r
    '\t' -> Char_esc_t
    '\v' -> Char_esc_v
    '\0' -> Char_hex HeXDigit0 HeXDigit0
    _ -> Char_lit c)

showStringPrefix :: StringPrefix -> Text
showStringPrefix sp =
  case sp of
    Prefix_u -> "u"
    Prefix_U -> "U"

showRawStringPrefix :: RawStringPrefix -> Text
showRawStringPrefix sp =
  case sp of
    Prefix_r -> "r"
    Prefix_R -> "R"

showBytesPrefix :: BytesPrefix -> Text
showBytesPrefix sp =
  case sp of
    Prefix_b -> "b"
    Prefix_B -> "B"

showRawBytesPrefix :: RawBytesPrefix -> Text
showRawBytesPrefix sp =
  case sp of
    Prefix_br -> "br"
    Prefix_Br -> "Br"
    Prefix_bR -> "bR"
    Prefix_BR -> "BR"
    Prefix_rb -> "rb"
    Prefix_rB -> "rB"
    Prefix_Rb -> "Rb"
    Prefix_RB -> "RB"

showQuoteType :: QuoteType -> Char
showQuoteType qt =
  case qt of
    DoubleQuote -> '\"'
    SingleQuote -> '\''

makeLensesFor
  [ ("_stringLiteralValue", "stringLiteralValue")
  , ("_stringLiteralStringType", "stringLiteralStringType")
  , ("_stringLiteralQuoteType", "stringLiteralQuoteType")
  , ("_stringLiteralWhitespace", "stringLiteralWhitespace")
  ]
  ''StringLiteral