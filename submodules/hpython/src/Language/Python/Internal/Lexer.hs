{-# language BangPatterns #-}
{-# language TypeApplications #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Language.Python.Internal.Lexer
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Internal.Lexer
  ( tokenizeWithTabs
    -- * Source Information
  , SrcInfo(..), initialSrcInfo, withSrcInfo
    -- * Errors
  , AsLexicalError(..), unsafeFromLexicalError
  , AsTabError(..), AsIncorrectDedent(..), fromTabError, TabError(..)
    -- * Miscellaneous
  , tokenize
  , insertTabs
    -- * Megaparsec re-exports
  , Parsec.ParseError(..)
  )
where

import Control.Applicative ((<|>), many, optional)
import Control.Lens.Getter ((^.))
import Control.Lens.Iso (from)
import Control.Lens.Prism (Prism')
import Control.Lens.Review ((#))
import Control.Monad ((<=<), when, replicateM)
import Control.Monad.Except (throwError)
import Control.Monad.State (StateT, evalStateT, get, modify, put)
import Data.Bifunctor (first)
import Data.Digit.Binary (parseBinary)
import Data.Digit.Class.D0 (parse0)
import Data.Digit.Decimal (parseDecimal, parseDecimalNoZero)
import Data.Digit.Hexadecimal.MixedCase (parseHeXaDeCiMaL)
import Data.Digit.Octal (parseOctal)
import Data.FingerTree (FingerTree, Measured(..))
import Data.Foldable (asum)
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty(..), some1, toList)
import Data.Monoid (Sum(..))
import Data.Set (Set, fromList, fold)
import Data.Semigroup (Semigroup, (<>))
import Data.Semigroup.Foldable (foldMap1)
import Data.These (These(..))
import Data.Void (Void)
import GHC.Stack (HasCallStack)
import Text.Megaparsec (MonadParsec, ParseError, parse, unPos)
import Text.Megaparsec.Parsers
  ( ParsecT, CharParsing, LookAheadParsing, lookAhead, unParsecT, satisfy, text
  , char, manyTill, try
  , notFollowedBy, anyChar, digit, oneOf
  )

import qualified Data.FingerTree as FingerTree
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Text.Megaparsec as Parsec

import Language.Python.Internal.Token (PyToken(..), pyTokenAnn)
import Language.Python.Syntax.Ann
import Language.Python.Syntax.Comment
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Numbers
import Language.Python.Syntax.Strings
import Language.Python.Syntax.Whitespace
import Control.Lens (Traversal)
import Control.Lens.Fold (preview)

data SrcInfo
  = SrcInfo
  { _srcInfoName :: FilePath
  , _srcInfoLineStart :: !Int
  , _srcInfoLineEnd :: !Int
  , _srcInfoColStart :: !Int
  , _srcInfoColEnd :: !Int
  , _srcInfoOffsetStart :: !Int
  , _srcInfoOffsetEnd :: !Int
  }
  deriving (Eq, Show)

instance Semigroup SrcInfo where
  SrcInfo _ ls le cs ce os oe <> SrcInfo n' ls' le' cs' ce' os' oe' =
    SrcInfo n' (min ls ls') (max le le') (min cs cs') (max ce ce') (min os os') (max oe oe')

initialSrcInfo :: FilePath -> SrcInfo
initialSrcInfo fp = SrcInfo fp 0 0 0 0 0 0

{-# inline withSrcInfo #-}
withSrcInfo :: (Parsec.TraversableStream s, MonadParsec e s m) => m (SrcInfo -> a) -> m a
withSrcInfo m =
  (\(Parsec.SourcePos name l c) o f (Parsec.SourcePos _ l' c') o' ->
     f $ SrcInfo name (unPos l) (unPos l') (unPos c) (unPos c') o o') <$>
  Parsec.getSourcePos  <*>
  Parsec.getOffset     <*>
  m <*>
  Parsec.getSourcePos  <*>
  Parsec.getOffset

  --Parsec.getPosition <*>
  --Parsec.getTokensProcessed <*>
  --Parsec.getPosition <*>
  --Parsec.getTokensProcessed

newline :: CharParsing m => m Newline
newline = LF <$ char '\n' <|> char '\r' *> (CRLF <$ char '\n' <|> pure CR)

parseNewline :: (CharParsing m, Monad m) => m (SrcInfo -> PyToken SrcInfo)
parseNewline = TkNewline <$> newline

parseComment :: (CharParsing m, Monad m) => m (SrcInfo -> PyToken SrcInfo)
parseComment =
  (\a b -> TkComment (MkComment (Ann b) a)) <$ char '#' <*>
  many (satisfy (`notElem` ['\r', '\n']))

stringOrBytesPrefix
  :: CharParsing m
  => m (Either
          (Either RawStringPrefix StringPrefix)
          (Either RawBytesPrefix BytesPrefix))
stringOrBytesPrefix =
  (char 'r' *>
   (Right (Left Prefix_rb) <$ char 'b' <|>
    Right (Left Prefix_rB) <$ char 'B' <|>
    pure (Left $ Left Prefix_r))) <|>
  (char 'R' *>
   (Right (Left Prefix_Rb) <$ char 'b' <|>
    Right (Left Prefix_RB) <$ char 'B' <|>
    pure (Left $ Left Prefix_R))) <|>
  (char 'b' *>
   (Right (Left Prefix_br) <$ char 'r' <|>
    Right (Left Prefix_bR) <$ char 'R' <|>
    pure (Right $ Right Prefix_b))) <|>
  (char 'B' *>
   (Right (Left Prefix_Br) <$ char 'r' <|>
    Right (Left Prefix_BR) <$ char 'R' <|>
    pure (Right $ Right Prefix_B))) <|>
  (Left (Right Prefix_u) <$ char 'u') <|>
  (Left (Right Prefix_U) <$ char 'U')

rawStringChar :: CharParsing m => m [PyChar]
rawStringChar =
  (\a -> [Char_lit '\\', Char_lit a]) <$ char '\\' <*> anyChar <|>
  pure . Char_lit <$> anyChar

stringChar :: (CharParsing m, LookAheadParsing m) => m PyChar
stringChar =
  (try (char '\\' <* lookAhead (oneOf "\"'U\\abfntuvx01234567")) *>
   (escapeChar <|> unicodeChar <|> octChar <|> hexChar)) <|>
  other
  where
    other = Char_lit <$> anyChar
    escapeChar =
      asum @[]
      [ Char_esc_bslash <$ char '\\'
      , Char_esc_singlequote <$ char '\''
      , Char_esc_doublequote <$ char '"'
      , Char_esc_a <$ char 'a'
      , Char_esc_b <$ char 'b'
      , Char_esc_f <$ char 'f'
      , char 'n' *> (Char_newline <$ text "ewline" <|> pure Char_esc_n)
      , Char_esc_r <$ char 'r'
      , Char_esc_t <$ char 't'
      , Char_esc_v <$ char 'v'
      ]

    unicodeChar =
      char 'U' *>
      ((\[a, b, c, d, e, f, g, h] -> Char_uni32 a b c d e f g h) <$>
       replicateM 8 parseHeXaDeCiMaL)
      <|>
      char 'u' *>
      ((\[a, b, c, d] -> Char_uni16 a b c d) <$>
       replicateM 4 parseHeXaDeCiMaL)

    hexChar = Char_hex <$ char 'x' <*> parseHeXaDeCiMaL <*> parseHeXaDeCiMaL
    octChar =
      (\a b c ->
         maybe
           (Char_octal1 a)
           (\b' -> maybe (Char_octal2 a b') (Char_octal3 a b') c)
           b) <$>
      parseOctal <*>
      optional parseOctal <*>
      optional parseOctal

number :: (CharParsing m, Monad m) => m (a -> PyToken a)
number = do
  zero <- optional parse0
  case zero of
    Nothing -> do
      nn <- optional $ (:|) <$> parseDecimalNoZero <*> many parseDecimal
      case nn of
        Just n ->
          (\x j ann ->
             case x of
               Nothing ->
                 maybe
                   (TkInt $ IntLiteralDec (Ann ann) n)
                   (TkImag . ImagLiteralInt (Ann ann) n) j
               Just (Right e) ->
                 let
                   f = FloatLiteralWhole (Ann ann) n e
                 in
                   maybe (TkFloat f) (TkImag . ImagLiteralFloat (Ann ann) f) j
               Just (Left (Left e)) ->
                 let
                   f = FloatLiteralFull (Ann ann) n (Just (That e))
                 in
                   maybe (TkFloat f) (TkImag . ImagLiteralFloat (Ann ann) f) j
               Just (Left (Right (a, b))) ->
                 let
                   f = FloatLiteralFull (Ann ann) n $
                     case (a, b) of
                       (Nothing, Nothing) -> Nothing
                       (Just x, Nothing) -> Just $ This x
                       (Nothing, Just x) -> Just $ That x
                       (Just x, Just y) -> Just $ These x y
                 in
                   maybe (TkFloat f) (TkImag . ImagLiteralFloat (Ann ann) f) j) <$>
          optional
            (Left <$ char '.' <*>
             (Left <$> floatExp <|>
              Right <$> ((,) <$> optional (some1 parseDecimal) <*> optional floatExp)) <|>
             Right <$> floatExp) <*>
          optional jJ
        Nothing ->
          (\a b j ann ->
             let
               f = FloatLiteralPoint (Ann ann) a b
             in
               maybe (TkFloat f) (TkImag . ImagLiteralFloat (Ann ann) f) j) <$>
          -- try is necessary here to prevent the intercepting of dereference tokens
          try (char '.' *> some1 parseDecimal) <*>
          optional floatExp <*>
          optional jJ
    Just z ->
      (\xX a b -> TkInt (IntLiteralHex (Ann b) xX a)) <$>
      (True <$ char 'X' <|> False <$ char 'x') <*>
      some1 parseHeXaDeCiMaL
      <|>
      (\bB a b -> TkInt (IntLiteralBin (Ann b) bB a)) <$>
      (True <$ char 'B' <|> False <$ char 'b') <*>
      some1 parseBinary
      <|>
      (\oO a b -> TkInt (IntLiteralOct (Ann b) oO a)) <$>
      (True <$ char 'O' <|> False <$ char 'o') <*>
      some1 parseOctal
      <|>
      (\n j a ->
         maybe
           (TkInt $ IntLiteralDec (Ann a) (z :| n))
           (TkImag . ImagLiteralInt (Ann a) (z :| n)) j) <$>
      try (many parse0 <* notFollowedBy (char '.' <|> char 'e' <|> char 'E' <|> digit)) <*>
      optional jJ
      <|>
      (\n' a ann ->
         case a of
           Left (Left (b, c, j)) ->
             let
               f = FloatLiteralFull (Ann ann) (z :| n') $
                 case (b, c) of
                   (Nothing, Nothing) -> Nothing
                   (Just x, Nothing) -> Just $ This x
                   (Nothing, Just x) -> Just $ That x
                   (Just x, Just y) -> Just $ These x y
             in
               maybe (TkFloat f) (TkImag . ImagLiteralFloat (Ann ann) f) j
           Left (Right (x, j)) ->
             let
               f = FloatLiteralWhole (Ann ann) (z :| n') x
             in
               maybe (TkFloat f) (TkImag . ImagLiteralFloat (Ann ann) f) j
           Right j -> TkImag $ ImagLiteralInt (Ann ann) (z :| n') j) <$>
      many parseDecimal <*>
      (Left <$>
       (Left <$>
        ((,,) <$ char '.' <*>
         optional (some1 parseDecimal) <*>
         optional floatExp <*>
         optional jJ) <|>
        Right <$>
        ((,) <$> floatExp <*> optional jJ)) <|>
      Right <$> jJ)
  where
    jJ = False <$ char 'j' <|> True <$ char 'J'
    floatExp =
      FloatExponent <$>
      (EE <$ char 'E' <|> Ee <$ char 'e') <*>
      optional (Pos <$ char '+' <|> Neg <$ char '-') <*>
      some1 parseDecimal

{-# inline parseToken #-}
parseToken
  :: (Monad m, CharParsing m, LookAheadParsing m, Parsec.TraversableStream s, MonadParsec e s m)
  => m (PyToken SrcInfo)
parseToken =
  withSrcInfo $
  try
    (asum
     [ TkIf <$ text "if"
     , TkElse <$ text "else"
     , TkElif <$ text "elif"
     , TkWhile <$ text "while"
     , TkAssert <$ text "assert"
     , TkDef <$ text "def"
     , TkReturn <$ text "return"
     , TkPass <$ text "pass"
     , TkBreak <$ text "break"
     , TkContinue <$ text "continue"
     , TkTrue <$ text "True"
     , TkFalse <$ text "False"
     , TkNone <$ text "None"
     , TkOr <$ text "or"
     , TkAnd <$ text "and"
     , TkIs <$ text "is"
     , TkNot <$ text "not"
     , TkGlobal <$ text "global"
     , TkNonlocal <$ text "nonlocal"
     , TkDel <$ text "del"
     , TkLambda <$ text "lambda"
     , TkImport <$ text "import"
     , TkFrom <$ text "from"
     , TkAs <$ text "as"
     , TkRaise <$ text "raise"
     , TkTry <$ text "try"
     , TkExcept <$ text "except"
     , TkFinally <$ text "finally"
     , TkClass <$ text "class"
     , TkWith <$ text "with"
     , TkFor <$ text "for"
     , TkIn <$ text "in"
     , TkYield <$ text "yield"
     ] <* notFollowedBy (satisfy isIdentifierChar))

    <|>

    asum
    [ number
    , TkRightArrow <$ text "->"
    , TkEllipsis <$ text "..."
    , TkSpace <$ char ' '
    , TkTab <$ char '\t'
    , TkLeftBracket <$ char '['
    , TkRightBracket <$ char ']'
    , TkLeftParen <$ char '('
    , TkRightParen <$ char ')'
    , TkLeftBrace <$ char '{'
    , TkRightBrace <$ char '}'
    , char '<' *>
      (TkLte <$ char '=' <|>
       char '<' *> (TkShiftLeftEq <$ char '=' <|> pure TkShiftLeft) <|>
       pure TkLt)
    , char '=' *> (TkDoubleEq <$ char '=' <|> pure TkEq)
    , char '>' *>
      (TkGte <$ char '=' <|>
       char '>' *> (TkShiftRightEq <$ char '=' <|> pure TkShiftRight) <|>
       pure TkGt)
    , char '*' *>
      (char '*' *> (TkDoubleStarEq <$ char '=' <|> pure TkDoubleStar) <|>
       TkStarEq <$ char '=' <|>
       pure TkStar)
    , char '/' *>
      (char '/' *> (TkDoubleSlashEq <$ char '=' <|> pure TkDoubleSlash) <|>
       TkSlashEq <$ char '=' <|>
       pure TkSlash)
    , TkBangEq <$ text "!="
    , char '^' *> (TkCaretEq <$ char '=' <|> pure TkCaret)
    , char '|' *> (TkPipeEq <$ char '=' <|> pure TkPipe)
    , char '&' *> (TkAmpersandEq <$ char '=' <|> pure TkAmpersand)
    , char '@' *> (TkAtEq <$ char '=' <|> pure TkAt)
    , char '+' *> (TkPlusEq <$ char '=' <|> pure TkPlus)
    , char '-' *> (TkMinusEq <$ char '=' <|> pure TkMinus)
    , char '%' *> (TkPercentEq <$ char '=' <|> pure TkPercent)
    , TkTilde <$ char '~'
    , TkContinued <$ char '\\' <*> newline
    , TkColon <$ char ':'
    , TkSemicolon <$ char ';'
    , parseComment
    , parseNewline
    , TkComma <$ char ','
    , TkDot <$ char '.'
    , do
        sp <- try $ optional stringOrBytesPrefix <* char '"'
        case sp of
          Nothing ->
            TkString Nothing LongString DoubleQuote <$
            text "\"\"" <*>
            manyTill stringChar (text "\"\"\"")
            <|>
            TkString Nothing ShortString DoubleQuote <$> manyTill stringChar (char '"')
          Just (Left (Left prefix)) ->
            TkRawString prefix LongString DoubleQuote . concat <$
            text "\"\"" <*>
            manyTill rawStringChar (text "\"\"\"")
            <|>
            TkRawString prefix ShortString DoubleQuote . concat <$>
            manyTill rawStringChar (char '"')
          Just (Left (Right prefix)) ->
            TkString (Just prefix) LongString DoubleQuote <$
            text "\"\"" <*>
            manyTill stringChar (text "\"\"\"")
            <|>
            TkString (Just prefix) ShortString DoubleQuote <$> manyTill stringChar (char '"')
          Just (Right (Left prefix)) ->
            TkRawBytes prefix LongString DoubleQuote . concat <$
            text "\"\"" <*>
            manyTill rawStringChar (text "\"\"\"")
            <|>
            TkRawBytes prefix ShortString DoubleQuote . concat <$>
            manyTill rawStringChar (char '"')
          Just (Right (Right prefix)) ->
            TkBytes prefix LongString DoubleQuote <$
            text "\"\"" <*>
            manyTill stringChar (text "\"\"\"")
            <|>
            TkBytes prefix ShortString DoubleQuote <$> manyTill stringChar (char '"')
    , do
        sp <- try $ optional stringOrBytesPrefix <* char '\''
        case sp of
          Nothing ->
            TkString Nothing LongString SingleQuote <$
            text "''" <*>
            manyTill stringChar (text "'''")
            <|>
            TkString Nothing ShortString SingleQuote <$> manyTill stringChar (char '\'')
          Just (Left (Left prefix)) ->
            TkRawString prefix LongString SingleQuote . concat <$
            text "''" <*>
            manyTill rawStringChar (text "'''")
            <|>
            TkRawString prefix ShortString SingleQuote . concat <$>
            manyTill rawStringChar (char '\'')
          Just (Left (Right prefix)) ->
            TkString (Just prefix) LongString SingleQuote <$
            text "''" <*>
            manyTill stringChar (text "'''")
            <|>
            TkString (Just prefix) ShortString SingleQuote <$> manyTill stringChar (char '\'')
          Just (Right (Left prefix)) ->
            TkRawBytes prefix LongString SingleQuote . concat <$
            text "''" <*>
            manyTill rawStringChar (text "'''")
            <|>
            TkRawBytes prefix ShortString SingleQuote . concat <$>
            manyTill rawStringChar (char '\'')
          Just (Right (Right prefix)) ->
            TkBytes prefix LongString SingleQuote <$
            text "''" <*>
            manyTill stringChar (text "'''")
            <|>
            TkBytes prefix ShortString SingleQuote <$> manyTill stringChar (char '\'')
    , fmap TkIdent $
      (:) <$>
      satisfy isIdentifierStart <*>
      many (satisfy isIdentifierChar)
    ]

class AsLexicalError s t | s -> t where
  _LexicalError
    :: Prism'
         s
         ( Int --NonEmpty (ParseError s e)
         , Maybe    (Parsec.ErrorItem s)
         , Set      (Parsec.ErrorItem s)
         )

  --_LexicalError =  preview (getCompose . _LexicalError) (a, b, c)
-- | Convert a concrete 'ParseError' to a value that has an instance of 'AsLexicalError'
--
-- This function is partial, because our parser will never use 'Parsec.FancyError'
-- unsafeFromLexicalError
--   :: ( HasCallStack
--      , AsLexicalError s t
--      )
--   => ParseError t Void
--   -> s
--pure $ fmap (t f) . t g ≡ getCompose . t (Compose . fmap f . g)
--unsafeFromLexicalError :: Parsec.ParseErrorBundle s e -> [Parsec.Token s]
unsafeFromLexicalError :: AsLexicalError (Parsec.Token s) [PyToken SrcInfo] => Parsec.ParseErrorBundle s e -> Parsec.Token s
unsafeFromLexicalError (Parsec.ParseErrorBundle d _) = do
    case NonEmpty.head d of
        (Parsec.TrivialError a b c) -> _LexicalError # (a, b, c)
        Parsec.FancyError{}         -> Prelude.error "'fancy error' used in lexer"
        
      --rawList = toList d
  -- map unsafeFromError rawList

-- bundleErrors :: NonEmpty (ParseError s e)	
-- A collection of ParseErrors that is sorted by parse error offsets
-- bundlePosState :: PosState s	
-- The state that is used for line/column calculation

  -- do
  --   case map (toList) a of 
  --     (Parsec.TrivialError b c d) -> _LexicalError # (a, b, c) 
  --     Parsec.FancyError{}         -> error "'fancy error' used in lexer"

-- unsafeFromLexicalError (Parsec.TrivialError a b c) = _LexicalError # (a, b, c)
-- unsafeFromLexicalError Parsec.FancyError{}         = error "'fancy error' used in lexer"

instance AsLexicalError Char [PyToken SrcInfo]

{-# noinline tokenize #-}
-- | Convert some input to a sequence of tokens. Indent and dedent tokens are not added
-- (see 'insertTabs')
tokenize :: String -> Text.Text -> Either Char [PyToken SrcInfo]
tokenize fp = first unsafeFromLexicalError . parse (unParsecT tokens) fp
  where
    tokens :: ParsecT Void Text.Text Identity [PyToken SrcInfo]
    tokens = many parseToken <* Parsec.eof

data LogicalLine a
  = LogicalLine
      a -- annotation
      ([PyToken a], Indent) -- spaces
      [PyToken a] -- line
      (Maybe (PyToken a)) -- end
  | BlankLine
      [PyToken a] -- line
      (Maybe (PyToken a)) -- end
  deriving (Eq, Show)

logicalLineToTokens :: LogicalLine a -> [PyToken a]
logicalLineToTokens (LogicalLine _ _ ts m) = ts <> maybe [] pure m
logicalLineToTokens (BlankLine ts m) = ts <> maybe [] pure m

spaceToken :: PyToken a -> Maybe Whitespace
spaceToken TkSpace{} = Just Space
spaceToken TkTab{} = Just Tab
spaceToken (TkContinued nl _) = Just $ Continued nl []
spaceToken _ = Nothing

collapseContinue :: [(PyToken a, Whitespace)] -> [([PyToken a], Whitespace)]
collapseContinue [] = []
collapseContinue ((tk@TkSpace{}, Space) : xs) =
  ([tk], Space) : collapseContinue xs
collapseContinue ((tk@TkTab{}, Tab) : xs) =
  ([tk], Tab) : collapseContinue xs
collapseContinue ((tk@TkNewline{}, Newline nl) : xs) =
  ([tk], Newline nl) : collapseContinue xs
collapseContinue ((tk@TkContinued{}, Continued nl ws) : xs) =
  let
    xs' = collapseContinue xs
  in
    [(tk : (xs' >>= fst), Continued nl $ ws <> fmap snd xs')]
collapseContinue _ = Prelude.error "invalid token/whitespace pair in collapseContinue"

spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe f as =
  case as of
    [] -> ([], [])
    x : xs ->
      case f x of
        Nothing -> ([], as)
        Just b -> first (b :) $ spanMaybe f xs

-- | Acts like break, but encodes the "insignificant whitespace" rule for parens, braces
-- and brackets
breakOnNewline :: [PyToken a] -> ([PyToken a], Maybe (PyToken a, [PyToken a]))
breakOnNewline = go 0
  where
    go :: Int -> [PyToken a] -> ([PyToken a], Maybe (PyToken a, [PyToken a]))
    go _ [] = ([], Nothing)
    go !careWhen0 (tk : tks) =
      case tk of
        TkLeftParen{} -> first (tk :) $ go (careWhen0 + 1) tks
        TkLeftBracket{} -> first (tk :) $ go (careWhen0 + 1) tks
        TkLeftBrace{} -> first (tk :) $ go (careWhen0 + 1) tks
        TkRightParen{} -> first (tk :) $ go (max 0 $ careWhen0 - 1) tks
        TkRightBracket{} -> first (tk :) $ go (max 0 $ careWhen0 - 1) tks
        TkRightBrace{} -> first (tk :) $ go (max 0 $ careWhen0 - 1) tks
        TkNewline{}
          | careWhen0 == 0 -> ([], Just (tk, tks))
          | otherwise -> first (tk :) $ go careWhen0 tks
        _ -> first (tk :) $ go careWhen0 tks

logicalLines :: [PyToken a] -> [LogicalLine a]
logicalLines [] = []
logicalLines tks =
  let
    (spaces, rest) = spanMaybe (\a -> (,) a <$> spaceToken a) tks
    (line, rest') = breakOnNewline rest
    spaces' = collapseContinue spaces
  in
    (if
       not (any (\case; Continued{} -> True; _ -> False) $ snd <$> spaces) &&
       all isBlankToken line
     then
       BlankLine (fmap fst spaces <> line) (fst <$> rest')
     else
       LogicalLine
         (case tks of
           [] -> Prelude.error "couldn't generate annotation for logical line"
           tk : _ -> pyTokenAnn tk)
         (spaces' >>= fst, fmap snd spaces' ^. from indentWhitespaces)
         line
         (fst <$> rest')) :
    logicalLines (maybe [] snd rest')

data IndentedLine a
  = Indent Int Indent a
  | Level (NonEmpty Whitespace) a
  | Dedent a
  | IndentedLine (LogicalLine a)
  deriving (Eq, Show)

isBlankToken :: PyToken a -> Bool
isBlankToken TkSpace{} = True
isBlankToken TkTab{} = True
isBlankToken TkComment{} = True
isBlankToken TkNewline{} = True
isBlankToken _ = False

data TabError a
  -- | Tabs and spaces were used inconsistently
  = TabError a
  -- | The dedent at the end of a block doesn't match and preceding indents
  --
  -- e.g.
  --
  -- @
  -- def a():
  --     if b:
  --         pass
  --     else:
  --         pass
  --   pass
  -- @
  --
  -- The final line will cause an 'IncorrectDedent' error
  | IncorrectDedent a
  deriving (Eq, Show)

class AsTabError s a | s -> a where
  _TabError :: Prism' s a

class AsIncorrectDedent s a | s -> a where
  _IncorrectDedent :: Prism' s a

-- | Convert a concrete 'TabError' to a value that has an instance of 'AsTabError'
fromTabError
  :: ( AsTabError s a
     , AsIncorrectDedent s a
     )
  => TabError a -> s
fromTabError (TabError a) = _TabError # a
fromTabError (IncorrectDedent a) = _IncorrectDedent # a

indentation :: Semigroup a => a -> [LogicalLine a] -> Either (TabError a) [IndentedLine a]
indentation ann lls =
  flip evalStateT (pure (ann, mempty)) $
  (<>) <$> (concat <$> traverse go lls) <*> finalDedents
  where
    finalDedents :: StateT (NonEmpty (a, Indent)) (Either (TabError a)) [IndentedLine a]
    finalDedents = do
      (ann, _) :| is <- get
      case is of
        [] -> pure []
        i' : is' -> do
          put $ i' :| is'
          (Dedent ann :) <$> finalDedents

    dedents
      :: a
      -> Int
      -> StateT (NonEmpty (a, Indent)) (Either (TabError a)) [IndentedLine a]
    dedents ann n = do
      is <- get
      let (popped, remainder) = NonEmpty.span ((> n) . indentLevel . snd) is
      when (n `notElem` fmap (indentLevel . snd) (NonEmpty.toList is)) .
        throwError $ IncorrectDedent ann
      put $ case remainder of
        [] -> Prelude.error "I don't know whether this can happen"
        x : xs -> x :| xs
      pure $ replicate (length popped) (Dedent ann)

    go
      :: Semigroup a
      => LogicalLine a
      -> StateT (NonEmpty (a, Indent)) (Either (TabError a)) [IndentedLine a]
    go ll@BlankLine{} = pure [IndentedLine ll]
    go ll@(LogicalLine ann (spTks, spcs) _ _) = do
      (_, i) :| _ <- get
      let
        et8 = absoluteIndentLevel 8 spcs
        et1 = absoluteIndentLevel 1 spcs
        et8i = absoluteIndentLevel 8 i
        et1i = absoluteIndentLevel 1 i
      when
        (not (et8 < et8i && et1 < et1i) &&
          not (et8 > et8i && et1 > et1i) &&
          not (et8 == et8i && et1 == et1i))
        (throwError $ TabError ann)
      let
        ilSpcs = indentLevel spcs
        ili = indentLevel i
        levelIndent =
          case (spTks, spcs ^. indentWhitespaces) of
            ([], []) -> []
            (x:xs, y:ys) -> [ Level (y:|ys) (foldMap1 pyTokenAnn $ x:|xs) ]
            _ -> Prelude.error "impossible"
      case compare ilSpcs ili of
        LT -> (<> (levelIndent <> [IndentedLine ll])) <$> dedents ann ilSpcs
        EQ ->
          pure $ levelIndent <> [ IndentedLine ll ]
        GT -> do
          modify $ NonEmpty.cons (ann, spcs)
          pure [Indent (ilSpcs - ili) spcs ann, IndentedLine ll]

newtype Summed a = Summed a
  deriving (Eq, Show, Ord, Num)

instance Num a => Measured (Sum a) (Summed a) where
  measure (Summed a) = Sum a

-- | Given a list of indentation jumps (first to last) and some whitespace,
-- divide the whitespace up into "blocks" which correspond to each jump
splitIndents :: FingerTree (Sum Int) (Summed Int) -> Indent -> [Indent]
splitIndents ns ws = go ns ws []
  where
    go :: FingerTree (Sum Int) (Summed Int) -> Indent -> [Indent] -> [Indent]
    go ns ws =
      case FingerTree.viewr ns of
        FingerTree.EmptyR -> (ws :)
        ns' FingerTree.:> n
          | FingerTree.null ns' -> (ws :)
          | otherwise ->
              let
                (befores, afters) =
                  FingerTree.split ((> getSum (measure ns')) . getIndentLevel) $ unIndent ws
              in
                if FingerTree.null afters
                then Prelude.error $ "could not carve out " <> show n <> " from " <> show ws
                else go ns' (MkIndent befores) . (MkIndent afters :)

chunked :: [IndentedLine a] -> [PyToken a]
chunked = go FingerTree.empty
  where
    go
      :: FingerTree (Sum Int) (Summed Int)
      -> [IndentedLine a]
      -> [PyToken a]
    go _ [] = []
    go leaps (Indent n i a : is) =
      let
        leaps' = leaps FingerTree.|> Summed n
      in
        TkIndent a (Indents (splitIndents leaps' i) (Ann a)) : go leaps' is
    go leaps (Dedent a : is) =
      case FingerTree.viewr leaps of
        FingerTree.EmptyR -> Prelude.error "impossible"
        leaps' FingerTree.:> _ -> TkDedent a : go leaps' is
    go leaps (IndentedLine ll : is) = logicalLineToTokens ll <> go leaps is
    go leaps (Level i a : is) =
      TkLevel a (Indents (splitIndents leaps $ NonEmpty.toList i ^. from indentWhitespaces) (Ann a)) : go leaps is

-- | Insert indent and dedent tokens
--
-- https://docs.python.org/3.5/reference/lexical_analysis.html#indentation
insertTabs
  :: ( Semigroup a
     , AsTabError s a
     , AsIncorrectDedent s a
     )
  => a -- ^ Initial source annotation
  -> [PyToken a] -- ^ Token stream
  -> Either s [PyToken a]
insertTabs a =
  first fromTabError .
  fmap chunked .
  indentation a .
  logicalLines

instance AsTabError Char SrcInfo
instance AsIncorrectDedent Char SrcInfo

-- | Tokenize an input file, inserting indent\/level\/dedent tokens in appropriate
-- positions according to the block structure.
tokenizeWithTabs fp = insertTabs (initialSrcInfo fp) <=< tokenize fp
