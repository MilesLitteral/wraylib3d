{-# language TemplateHaskell #-}
{-# language DataKinds, KindSignatures #-}
{-# language MultiParamTypeClasses, FlexibleInstances #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# language InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# language TypeFamilies #-}
{-# language LambdaCase #-}
{-# language UndecidableInstances #-}

{-|
Module      : Language.Python.Syntax.Statement
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Syntax.Statement
  ( -- * Statements
    Statement(..)
    -- ** Traversals
  , HasStatements(..)
  , HasStatements1(..)
    -- * Decorators
  , Decorator(..)
    -- ** Compound statements
  , CompoundStatement(..)
    -- ** Small statements
  , SmallStatement(..)
    -- ** Simple statements
  , SimpleStatement(..)
    -- *** @with ... as ...@
  , WithItem(..)
    -- **** Lenses
  , withItemAnn
  , withItemValue
  , withItemBinder
    -- *** @except ... as ...@
  , ExceptAs (..)
    -- **** Lenses
  , exceptAsAnn
  , exceptAsExpr
  , exceptAsName
    -- * Suites
  , Suite(..)
    -- * Blocks
  , Block(..)
    -- ** Lenses
  , blockBlankLines
  , blockHead
  , blockTail
    -- ** Traversals
  , HasBlocks(..)
  )
where

import Control.Lens.Cons (_last)
import Control.Lens.Fold (foldMapOf, folded)
import Control.Lens.Getter ((^.), to, view)
import Control.Lens.Lens (Lens')
import Control.Lens.Plated (Plated(..), gplate)
import Control.Lens.Prism (_Right)
import Control.Lens.Setter ((.~), over, mapped)
import Control.Lens.TH (makeLenses)
import Control.Lens.Traversal (Traversal, Traversal1, traverseOf)
import Control.Lens.Tuple (_1, _2, _3, _4)
import Data.Bifoldable (bifoldMap)
import Data.Bifunctor (bimap)
import Data.Bitraversable (bitraverse)
import Data.Coerce (coerce)
import Data.Functor.Apply ((<.>))
import Data.Generics.Product.Typed (typed)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.List.NonEmpty as NonEmpty

import Language.Python.Optics.Validated
import Language.Python.Syntax.Ann
import Language.Python.Syntax.AugAssign
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Comment
import Language.Python.Syntax.Expr
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Import
import Language.Python.Syntax.ModuleNames
import Language.Python.Syntax.Punctuation
import Language.Python.Syntax.Whitespace

-- See note [unsafeCoerce Validation] in Language.Python.Internal.Syntax.Expr
instance Validated Statement where; unvalidated = to unsafeCoerce
instance Validated SmallStatement where; unvalidated = to unsafeCoerce
instance Validated SimpleStatement where; unvalidated = to unsafeCoerce
instance Validated CompoundStatement where; unvalidated = to unsafeCoerce
instance Validated Block where; unvalidated = to unsafeCoerce
instance Validated Suite where; unvalidated = to unsafeCoerce
instance Validated WithItem where; unvalidated = to unsafeCoerce
instance Validated ExceptAs where; unvalidated = to unsafeCoerce
instance Validated Decorator where; unvalidated = to unsafeCoerce

-- | 'Traversal' over all the 'Statement's in a term
class HasStatements s where
  _Statements :: Traversal (s v a) (s '[] a) (Statement v a) (Statement '[] a)

class HasStatements s => HasStatements1 s where
  _Statements1 :: Traversal1 (s v a) (s '[] a) (Statement v a) (Statement '[] a)

-- | A 'Block' is an indented multi-line chunk of code, forming part of a
-- 'Suite'.
data Block (v :: [*]) a
  = Block
  { _blockBlankLines :: [(Blank a, Newline)] -- ^ Blank lines at the beginning of the block
  , _blockHead :: Statement v a -- ^ The first statement of the block
  , _blockTail :: [Either (Blank a, Newline) (Statement v a)] -- ^ The remaining items of the block, which may be statements or blank lines
  } deriving (Eq, Show, Generic)

instance Functor (Block v) where
  fmap f (Block a b c) =
    Block
      (over (mapped._1.mapped) f a)
      (fmap f b)
      (bimap (over (_1.mapped) f) (fmap f) <$> c)

instance Foldable (Block v) where
  foldMap f (Block a b c) =
    foldMapOf (folded._1.folded) f a <>
    foldMap f b <>
    foldMap (bifoldMap (foldMapOf (_1.folded) f) (foldMap f)) c

instance Traversable (Block v) where
  traverse f (Block a b c) =
    Block <$>
    traverseOf (traverse._1.traverse) f a <*>
    traverse f b <*>
    traverse (bitraverse (traverseOf (_1.traverse) f) (traverse f)) c

class HasBlocks s where
  -- | 'Traversal' targeting all the 'Block's in a structure
  _Blocks :: Traversal (s v a) (s '[] a) (Block v a) (Block '[] a)

instance HasBlocks Suite where
  _Blocks _ (SuiteOne a b c) = pure $ SuiteOne a b (c ^. unvalidated)
  _Blocks f (SuiteMany a b c d e) = SuiteMany a b c d <$> f e

instance HasBlocks CompoundStatement where
  _Blocks f (Fundef a decos idnt asyncWs ws1 name ws2 params ws3 mty s) =
    Fundef a
      (view unvalidated <$> decos) idnt asyncWs ws1 (coerce name) ws2
      (view unvalidated <$> params) ws3 (over (mapped._2) (view unvalidated) mty) <$>
    _Blocks f s
  _Blocks f (If idnt a ws1 e1 s elifs b') =
    If idnt a ws1 (e1 ^. unvalidated) <$>
    _Blocks f s <*>
    traverse (\(a, b, c, d) -> (,,,) a b (c ^. unvalidated) <$> _Blocks f d) elifs <*>
    traverseOf (traverse._3._Blocks) f b'
  _Blocks f (While idnt a ws1 e1 s els) =
    While idnt a ws1 (e1 ^. unvalidated) <$>
    _Blocks f s <*>
    traverseOf (traverse._3._Blocks) f els
  _Blocks fun (TryExcept idnt a b c d e f) =
    TryExcept idnt a (coerce b) <$>
    _Blocks fun c <*>
    traverse
      (\(a, b, c, d) -> (,,,) a b (view unvalidated <$> c) <$> _Blocks fun d)
      d <*>
    traverseOf (traverse._3._Blocks) fun e <*>
    traverseOf (traverse._3._Blocks) fun f
  _Blocks fun (TryFinally idnt a b c d e f) =
    TryFinally idnt a b <$>
    _Blocks fun c <*>
    pure d <*>
    pure e <*>
    _Blocks fun f
  _Blocks fun (For idnt a asyncWs b c d e f g) =
    For idnt a asyncWs b (c ^. unvalidated) d (view unvalidated <$> e) <$>
    _Blocks fun f <*>
    (traverse._3._Blocks) fun g
  _Blocks fun (ClassDef a decos idnt b c d e) =
    ClassDef a
      (view unvalidated <$> decos) idnt b
      (coerce c) (over (mapped._2.mapped.mapped) (view unvalidated) d) <$>
    _Blocks fun e
  _Blocks fun (With a b asyncWs c d e) =
    With a b asyncWs c (view unvalidated <$> d) <$> _Blocks fun e

instance HasStatements Block where
  _Statements f (Block a b c) =
    Block a <$> f b <*> (traverse._Right) f c

instance HasStatements1 Block  where
  _Statements1 f (Block a l m) = uncurry (Block a) <$> go l m
    where
      go b [] = (\b' -> (b', [])) <$> f b
      go b (Left c:cs) =
        (\(b', cs') -> (b', Left c:cs')) <$>
        go b cs
      go b (Right c:cs) =
        (,) <$>
        f b <.>
        fmap
          (\(c', cs') -> Right c' : cs')
          (go c cs)

instance HasStatements Suite where
  _Statements _ (SuiteOne a b c) = pure $ SuiteOne a b (c ^. unvalidated)
  _Statements f (SuiteMany a b c d e) = SuiteMany a b c d <$> _Statements f e

-- | See @simpl_stmt@ at <https://docs.python.org/3.5/reference/grammar.html>. The grammar
-- has the terminology mixed up - it should really be called @small_stmt@ there.
data SmallStatement (v :: [*]) a
  = MkSmallStatement
      (SimpleStatement v a)
      [(Semicolon a, SimpleStatement v a)]
      (Maybe (Semicolon a))
      (Maybe (Comment a))
      (Maybe Newline)
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

-- | A 'Statement' is either a 'SmallStatement' or a 'CompoundStatement'
--
-- https://docs.python.org/3.5/reference/compound_stmts.html#compound-statements
data Statement (v :: [*]) a
  = SmallStatement (Indents a) (SmallStatement v a)
  | CompoundStatement (CompoundStatement v a)
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasStatements Statement where
  _Statements = id

instance HasStatements1 Statement where
  _Statements1 = id

instance HasExprs SmallStatement where
  _Exprs f (MkSmallStatement s ss a b c) =
    MkSmallStatement <$>
    _Exprs f s <*>
    (traverse._2._Exprs) f ss <*>
    pure a <*>
    pure b <*>
    pure c

instance HasExprs Statement where
  _Exprs f (SmallStatement idnt a) = SmallStatement idnt <$> _Exprs f a
  _Exprs f (CompoundStatement c) = CompoundStatement <$> _Exprs f c

instance HasBlocks SmallStatement where
  _Blocks _ (MkSmallStatement a b c d e) =
    pure $
    MkSmallStatement
      (a ^. unvalidated)
      (over (mapped._2) (view unvalidated) b)
      c d e

instance HasBlocks Statement where
  _Blocks f (CompoundStatement c) = CompoundStatement <$> _Blocks f c
  _Blocks f (SmallStatement idnt a) = SmallStatement idnt <$> _Blocks f a

instance Plated (Statement '[] a) where
  plate _ (SmallStatement idnt s) = pure $ SmallStatement idnt s
  plate fun (CompoundStatement s) =
    CompoundStatement <$>
    case s of
      Fundef idnt a decos asyncWs ws1 b ws2 c ws3 mty s ->
        Fundef idnt a decos asyncWs ws1 b ws2 c ws3 mty <$> _Statements fun s
      If idnt a ws1 b s elifs sts' ->
        If idnt a ws1 b <$>
        _Statements fun s <*>
        (traverse._4._Statements) fun elifs <*>
        (traverse._3._Statements) fun sts'
      While idnt a ws1 b s els ->
        While idnt a ws1 b <$>
        _Statements fun s <*>
        (traverse._3._Statements) fun els
      TryExcept idnt a b c d e f ->
        TryExcept idnt a b <$> _Statements fun c <*>
        (traverse._4._Statements) fun d <*>
        (traverse._3._Statements) fun e <*>
        (traverse._3._Statements) fun f
      TryFinally idnt a b c d e f ->
        TryFinally idnt a b <$> _Statements fun c <*> pure d <*>
        pure e <*> _Statements fun f
      For idnt a asyncWs b c d e f g ->
        For idnt a asyncWs b c d e <$>
        _Statements fun f <*>
        (traverse._3._Statements) fun g
      ClassDef idnt a decos b c d e ->
        ClassDef idnt a decos b c d <$> _Statements fun e
      With a b asyncWs c d e -> With a b asyncWs c (coerce d) <$> _Statements fun e

-- | https://docs.python.org/3.5/reference/simple_stmts.html
data SimpleStatement (v :: [*]) a
  -- | @\'return\' \<spaces\> [\<expr\>]@
  --
  -- https://docs.python.org/3.5/reference/simple_stmts.html#the-return-statement
  = Return (Ann a) [Whitespace] (Maybe (Expr v a))
  -- | @\<expr\>@
  --
  -- https://docs.python.org/3.5/reference/simple_stmts.html#expression-statements
  | Expr (Ann a) (Expr v a)
  -- | @\<expr\> (\'=\' \<spaces\> \<expr\>)+@
  --
  -- https://docs.python.org/3.5/reference/simple_stmts.html#assignment-statements
  | Assign (Ann a) (Expr v a) (NonEmpty (Equals, Expr v a))
  -- | @\<expr\> \<augassign\> \<expr\>@
  --
  -- https://docs.python.org/3.5/reference/simple_stmts.html#augmented-assignment-statements
  | AugAssign (Ann a) (Expr v a) (AugAssign a) (Expr v a)
  -- | @\'pass\' \<spaces\>@
  --
  -- https://docs.python.org/3.5/reference/simple_stmts.html#the-pass-statement
  | Pass (Ann a) [Whitespace]
  -- | @\'break\' \<spaces\>@
  --
  -- https://docs.python.org/3.5/reference/simple_stmts.html#the-break-statement
  | Break (Ann a) [Whitespace]
  -- | @\'continue\' \<spaces\>@
  --
  -- https://docs.python.org/3.5/reference/simple_stmts.html#the-continue-statement
  | Continue (Ann a) [Whitespace]
  -- | @\'global\' \<spaces\> \<idents\>@
  --
  -- https://docs.python.org/3.5/reference/simple_stmts.html#the-global-statement
  | Global (Ann a) (NonEmpty Whitespace) (CommaSep1 (Ident v a))
  -- | @\'nonlocal\' \<spaces\> \<idents\>@
  --
  -- https://docs.python.org/3.5/reference/simple_stmts.html#the-nonlocal-statement
  | Nonlocal (Ann a) (NonEmpty Whitespace) (CommaSep1 (Ident v a))
  -- | @\'del\' \<spaces\> \<exprs\>@
  --
  -- https://docs.python.org/3.5/reference/simple_stmts.html#the-del-statement
  | Del (Ann a) [Whitespace] (CommaSep1' (Expr v a))
  -- | @\'import\' \<spaces\> \<modulenames\>@
  --
  -- https://docs.python.org/3.5/reference/simple_stmts.html#the-import-statement
  | Import
      (Ann a)
      (NonEmpty Whitespace)
      (CommaSep1 (ImportAs ModuleName v a))
  -- | @\'from\' \<spaces\> \<relative_module_name\> \'import\' \<spaces\> \<import_targets\>@
  --
  -- https://docs.python.org/3.5/reference/simple_stmts.html#the-import-statement
  | From
      (Ann a)
      [Whitespace]
      (RelativeModuleName v a)
      [Whitespace]
      (ImportTargets v a)
  -- | @\'raise\' \<spaces\> [\<expr\> [\'as\' \<spaces\> \<expr\>]]@
  --
  -- https://docs.python.org/3.5/reference/simple_stmts.html#the-raise-statement
  | Raise (Ann a)
      [Whitespace]
      (Maybe (Expr v a, Maybe ([Whitespace], Expr v a)))
  -- | @\'assert\' \<spaces\> \<expr\> [\',\' \<spaces\> \<expr\>]@
  --
  -- https://docs.python.org/3.5/reference/simple_stmts.html#the-assert-statement
  | Assert (Ann a)
      [Whitespace]
      (Expr v a)
      (Maybe (Comma, Expr v a))
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasAnn (SimpleStatement v) where
  annot :: forall a. Lens' (SimpleStatement v a) (Ann a)
  annot = typed @(Ann a)

instance Plated (SimpleStatement '[] a) where; plate = gplate

instance HasExprs SimpleStatement where
  _Exprs f (Assert a b c d) = Assert a b <$> f c <*> traverseOf (traverse._2) f d
  _Exprs f (Raise a ws x) =
    Raise a ws <$>
    traverse
      (\(b, c) -> (,) <$> f b <*> traverseOf (traverse._2) f c)
      x
  _Exprs f (Return a ws e) = Return a ws <$> traverse f e
  _Exprs f (Expr a e) = Expr a <$> f e
  _Exprs f (Assign a e1 es) = Assign a <$> f e1 <*> traverseOf (traverse._2) f es
  _Exprs f (AugAssign a e1 as e2) = AugAssign a <$> f e1 <*> pure as <*> f e2
  _Exprs _ p@Pass{} = pure $ p ^. unvalidated
  _Exprs _ p@Break{} = pure $ p ^. unvalidated
  _Exprs _ p@Continue{} = pure $ p ^. unvalidated
  _Exprs _ p@Global{} = pure $ p ^. unvalidated
  _Exprs _ p@Nonlocal{} = pure $ p ^. unvalidated
  _Exprs _ p@Del{} = pure $ p ^. unvalidated
  _Exprs _ p@Import{} = pure $ p ^. unvalidated
  _Exprs _ p@From{} = pure $ p ^. unvalidated

-- | See <https://docs.python.org/3.5/reference/compound_stmts.html#the-try-statement>
data ExceptAs (v :: [*]) a
  = ExceptAs
  { _exceptAsAnn :: Ann a
  , _exceptAsExpr :: Expr v a -- ^ @\<expr\>@
  , _exceptAsName :: Maybe ([Whitespace], Ident v a) -- ^ @[\'as\' \<ident\>]@
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasAnn (ExceptAs v) where
  annot :: forall a. Lens' (ExceptAs v a) (Ann a)
  annot = typed @(Ann a)

-- | A compound statement consists of one or more clauses.
-- A clause consists of a header and a suite.
data Suite (v :: [*]) a
  -- ':' <space> smallStatement
  = SuiteOne (Ann a) Colon (SmallStatement v a)
  | SuiteMany (Ann a)
      -- ':' <spaces> [comment] <newline>
      Colon (Maybe (Comment a)) Newline
      -- <block>
      (Block v a)
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasAnn (Suite v) where
  annot :: forall a. Lens' (Suite v a) (Ann a)
  annot = typed @(Ann a)

-- | See <https://docs.python.org/3.5/reference/compound_stmts.html#the-with-statement>
data WithItem (v :: [*]) a
  = WithItem
  { _withItemAnn :: Ann a
  , _withItemValue :: Expr v a -- ^ @\<expr\>@
  , _withItemBinder :: Maybe ([Whitespace], Expr v a) -- ^ @[\'as\' \<spaces\> \<expr\>]@
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasAnn (WithItem v) where
  annot :: forall a. Lens' (WithItem v a) (Ann a)
  annot = typed @(Ann a)

-- | Decorators on function definitions
--
-- <https://docs.python.org/3.5/reference/compound_stmts.html#function-definitions>
--
-- <https://docs.python.org/3.5/glossary.html#term-decorator>
data Decorator (v :: [*]) a
  = Decorator
  { _decoratorAnn :: Ann a
  , _decoratorIndents :: Indents a -- ^ Preceding indentation
  , _decoratorAt :: At -- ^ @\'\@\' \<spaces\>@
  , _decoratorExpr :: Expr v a -- ^ @\<expr\>@
  , _decoratorComment :: Maybe (Comment a) -- ^ Trailing comment
  , _decoratorNewline :: Newline -- ^ Trailing newline
  , _decoratorBlankLines :: [(Blank a, Newline)] -- ^ Trailing blank lines
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasAnn (Decorator v) where
  annot :: forall a. Lens' (Decorator v a) (Ann a)
  annot = typed @(Ann a)

-- | See <https://docs.python.org/3.5/reference/compound_stmts.html>
data CompoundStatement (v :: [*]) a
  -- | https://docs.python.org/3.5/reference/compound_stmts.html#function-definitions
  --
  -- https://docs.python.org/3.5/reference/compound_stmts.html#coroutine-function-definition
  = Fundef
  { _csAnn :: Ann a
  , _unsafeCsFundefDecorators :: [Decorator v a] -- ^ Preceding 'Decorator's
  , _csIndents :: Indents a -- ^ Preceding indentation
  , _unsafeCsFundefAsync :: Maybe (NonEmpty Whitespace) -- ^ @[\'async\' \<spaces\>]@
  , _unsafeCsFundefDef :: NonEmpty Whitespace -- ^ @\'def\' \<spaces\>@
  , _unsafeCsFundefName :: Ident v a -- ^ @\<ident\>@
  , _unsafeCsFundefLeftParen :: [Whitespace] -- ^ @\'(\' \<spaces\>@
  , _unsafeCsFundefParameters :: CommaSep (Param v a) -- ^ @\<parameters\>@
  , _unsafeCsFundefRightParen :: [Whitespace] -- ^ @\')\' \<spaces\>@
  , _unsafeCsFundefReturnType :: Maybe ([Whitespace], Expr v a) -- ^ @[\'->\' \<spaces\> \<expr\>]@
  , _unsafeCsFundefBody :: Suite v a -- ^ @\<suite\>@
  }
  -- | https://docs.python.org/3.5/reference/compound_stmts.html#the-if-statement
  | If
  { _csAnn :: Ann a
  , _csIndents :: Indents a -- ^ Preceding indentation
  , _unsafeCsIfIf :: [Whitespace] -- ^ @\'if\' \<spaces\>@
  , _unsafeCsIfCond :: Expr v a -- ^ @\<expr\>@
  , _unsafeCsIfBody :: Suite v a -- ^ @\<suite\>@
  , _unsafeCsIfElifs :: [(Indents a, [Whitespace], Expr v a, Suite v a)] -- ^ @(\'elif\' \<spaces\> \<expr\> \<suite\>)*@
  , _unsafeCsIfElse :: Maybe (Indents a, [Whitespace], Suite v a) -- ^ @[\'else\' \<spaces\> \<suite\>]@
  }
  -- | https://docs.python.org/3.5/reference/compound_stmts.html#the-while-statement
  | While
  { _csAnn :: Ann a
  , _csIndents :: Indents a -- ^ Preceding indentation
  , _unsafeCsWhileWhile :: [Whitespace] -- ^ @\'while\' \<spaces\>@
  , _unsafeCsWhileCond :: Expr v a -- ^ @\<expr\>@
  , _unsafeCsWhileBody :: Suite v a -- ^ @\<suite\>@
  , _unsafeCsWhileElse
    :: Maybe (Indents a, [Whitespace], Suite v a) -- ^ @[\'else\' \<spaces\> \<suite\>]@
  }
  -- | https://docs.python.org/3.5/reference/compound_stmts.html#the-try-statement
  | TryExcept
  { _csAnn :: Ann a
  , _csIndents :: Indents a -- ^ Preceding indentation
  , _unsafeCsTryExceptTry :: [Whitespace] -- ^ @\'try\' \<spaces\>@
  , _unsafeCsTryExceptBody :: Suite v a -- ^ @\<suite\>@
  , _unsafeCsTryExceptExcepts :: NonEmpty (Indents a, [Whitespace], Maybe (ExceptAs v a), Suite v a) -- ^ @(\'except\' \<spaces\> \<except_as\> \<suite\>)+@
  , _unsafeCsTryExceptElse :: Maybe (Indents a, [Whitespace], Suite v a) -- ^ @[\'else\' \<spaces\> \<suite\>]@
  , _unsafeCsTryExceptFinally :: Maybe (Indents a, [Whitespace], Suite v a) -- ^ @[\'finally\' \<spaces\> \<suite\>]@
  }
  -- | https://docs.python.org/3.5/reference/compound_stmts.html#the-try-statement
  | TryFinally
  { _csAnn :: Ann a
  , _csIndents :: Indents a -- ^ Preceding indentation
  , _unsafeCsTryFinallyTry :: [Whitespace] -- ^ @\'try\' \<spaces\>@
  , _unsafeCsTryFinallyTryBody :: Suite v a -- ^ @\<suite\>@
  , _unsafeCsTryFinallyFinallyIndents :: Indents a
  , _unsafeCsTryFinallyFinally :: [Whitespace] -- ^ @\'finally\' \<spaces\>@
  , _unsafeCsTryFinallyFinallyBody :: Suite v a -- ^ @\<suite\>@
  }
  -- | https://docs.python.org/3.5/reference/compound_stmts.html#the-for-statement
  --
  -- https://docs.python.org/3.5/reference/compound_stmts.html#the-async-for-statement
  | For
  { _csAnn :: Ann a
  , _csIndents :: Indents a -- ^ Preceding indentation
  , _unsafeCsForAsync :: Maybe (NonEmpty Whitespace) -- ^ @[\'async\' \<spaces\>]@
  , _unsafeCsForFor :: [Whitespace] -- ^ @\'for\' \<spaces\>@
  , _unsafeCsForBinder :: Expr v a -- ^ @\<expr\>@
  , _unsafeCsForIn :: [Whitespace] -- ^ @\'in\' \<spaces\>@
  , _unsafeCsForCollection :: CommaSep1' (Expr v a) -- ^ @\<exprs\>@
  , _unsafeCsForBody :: Suite v a -- ^ @\<suite\>@
  , _unsafeCsForElse :: Maybe (Indents a, [Whitespace], Suite v a) -- ^ @[\'else\' \<spaces\> \<suite\>]@
  }
  -- | https://docs.python.org/3.5/reference/compound_stmts.html#class-definitions
  | ClassDef
  { _csAnn :: Ann a
  , _unsafeCsClassDefDecorators :: [Decorator v a] -- ^ Preceding 'Decorator's
  , _csIndents :: Indents a -- ^ Preceding indentation
  , _unsafeCsClassDefClass :: NonEmpty Whitespace -- ^ @\'class\' \<spaces\>@
  , _unsafeCsClassDefName :: Ident v a -- ^ @\<ident\>@
  , _unsafeCsClassDefArguments :: Maybe ([Whitespace], Maybe (CommaSep1' (Arg v a)), [Whitespace]) -- ^ @[\'(\' \<spaces\> [\<args\>] \')\' \<spaces\>]@
  , _unsafeCsClassDefBody :: Suite v a -- ^ @\<suite\>@
  }
  -- | https://docs.python.org/3.5/reference/compound_stmts.html#the-with-statement
  --
  -- https://docs.python.org/3.5/reference/compound_stmts.html#the-async-with-statement
  | With
  { _csAnn :: Ann a
  , _csIndents :: Indents a -- ^ Preceding indentation
  , _unsafeCsWithAsync :: Maybe (NonEmpty Whitespace) -- ^ @[\'async\' \<spaces\>]@
  , _unsafeCsWithWith :: [Whitespace] -- ^ @\'with\' \<spaces\>@
  , _unsafeCsWithItems :: CommaSep1 (WithItem v a) -- ^ @\<with_items\>@
  , _unsafeCsWithBody :: Suite v a -- ^ @\<suite\>@
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance HasAnn (CompoundStatement v) where
  annot :: forall a. Lens' (CompoundStatement v a) (Ann a)
  annot = typed @(Ann a)

instance HasExprs ExceptAs where
  _Exprs f (ExceptAs ann e a) = ExceptAs ann <$> f e <*> pure (coerce a)

instance HasExprs Block where
  _Exprs f (Block a b c) =
    Block a <$> _Exprs f b <*> (traverse._Right._Exprs) f c

instance HasExprs Suite where
  _Exprs f (SuiteOne a b c) = (\c' -> SuiteOne a b c') <$> _Exprs f c
  _Exprs f (SuiteMany a b c d e) = SuiteMany a b c d <$> _Exprs f e

instance HasExprs WithItem where
  _Exprs f (WithItem a b c) = WithItem a <$> f b <*> traverseOf (traverse._2) f c

instance HasExprs Decorator where
  _Exprs fun (Decorator a b c d e f g) = (\d' -> Decorator a b c d' e f g) <$> _Exprs fun d

instance HasExprs CompoundStatement where
  _Exprs f (Fundef a decos idnt asyncWs ws1 name ws2 params ws3 mty s) =
    Fundef a <$>
    (traverse._Exprs) f decos <*>
    pure idnt <*>
    pure asyncWs <*>
    pure ws1 <*>
    pure (coerce name) <*>
    pure ws2 <*>
    (traverse._Exprs) f params <*>
    pure ws3 <*>
    traverseOf (traverse._2) f mty <*>
    _Exprs f s
  _Exprs fun (If idnt a ws1 e s elifs sts') =
    If idnt a ws1 <$>
    fun e <*>
    _Exprs fun s <*>
    traverse
      (\(a, b, c, d) -> (,,,) a b <$> fun c <*> _Exprs fun d)
      elifs <*>
    (traverse._3._Exprs) fun sts'
  _Exprs f (While idnt a ws1 e s els) =
    While idnt a ws1 <$>
    f e <*>
    _Exprs f s <*>
    (traverse._3._Exprs) f els
  _Exprs fun (TryExcept idnt a b c d e f) =
    TryExcept idnt a b <$> _Exprs fun c <*>
    traverse
      (\(a, b, c, d) -> (,,,) a b <$> traverse (_Exprs fun) c <*> _Exprs fun d)
      d <*>
    (traverse._3._Exprs) fun e <*>
    (traverse._3._Exprs) fun f
  _Exprs fun (TryFinally idnt a b c d e f) =
    TryFinally idnt a b <$> _Exprs fun c <*> pure d <*>
    pure e <*> _Exprs fun f
  _Exprs fun (For a idnt asyncWs b c d e f g) =
    For a idnt asyncWs b <$> fun c <*> pure d <*> traverse fun e <*>
    _Exprs fun f <*>
    (traverse._3._Exprs) fun g
  _Exprs fun (ClassDef a decos idnt b c d e) =
    ClassDef a <$>
    traverse (_Exprs fun) decos <*>
    pure idnt <*>
    pure b <*>
    pure (coerce c) <*>
    (traverse._2.traverse.traverse._Exprs) fun d <*>
    _Exprs fun e
  _Exprs fun (With a b asyncWs c d e) =
    With a b asyncWs c <$> traverseOf (traverse._Exprs) fun d <*> _Exprs fun e

instance HasTrailingNewline Statement where
  trailingNewline f x =
    case x of
      SmallStatement a b -> SmallStatement a <$> trailingNewline f b
      CompoundStatement c -> CompoundStatement <$> trailingNewline f c

  setTrailingNewline s n =
    case s of
      SmallStatement i a -> SmallStatement i $ setTrailingNewline a n
      CompoundStatement c -> CompoundStatement $ setTrailingNewline c n

instance HasTrailingNewline SmallStatement where
  trailingNewline f (MkSmallStatement a b c d e) =
    MkSmallStatement a b c d <$> traverse f e
  setTrailingNewline (MkSmallStatement a b c d _) n =
    MkSmallStatement a b c d (Just n)

instance HasTrailingNewline Suite where
  trailingNewline f x =
    case x of
      SuiteOne a b c -> SuiteOne a b <$> trailingNewline f c
      SuiteMany a b c d e -> SuiteMany a b c d <$> trailingNewline f e
  setTrailingNewline x n =
    case x of
      SuiteOne a b c -> SuiteOne a b $ setTrailingNewline c n
      SuiteMany a b c d e -> SuiteMany a b c d $ setTrailingNewline e n

instance HasTrailingNewline Block where
  trailingNewline f (Block a b []) = Block a <$> trailingNewline f b <*> pure []
  trailingNewline f (Block a b (c:cs)) =
    Block a b <$>
    traverseOf
      _last
      (bitraverse (traverseOf _2 f) (trailingNewline f))
      (c:cs)

  setTrailingNewline (Block a b []) n =
    Block a (setTrailingNewline b n) []
  setTrailingNewline (Block a b (c:cs)) n =
    Block a b (over _last (bimap (_2 .~ n) (flip setTrailingNewline n)) $ c:cs)

instance HasTrailingNewline CompoundStatement where
  trailingNewline fun s =
    case s of
      Fundef a b c d e f g h i j k ->
        Fundef a b c d e f g h i j <$> trailingNewline fun k
      If a b c d e f g ->
        If a b c d <$>
        (if null f && isNothing g
         then trailingNewline fun e
         else pure e) <*>
        (if isNothing g
         then (_last._4.trailingNewline) fun f
         else pure f)<*>
        (traverse._3.trailingNewline) fun g
      While a b c d e f ->
        While a b c d <$>
        (if isNothing f
         then trailingNewline fun e
         else pure e) <*>
        (traverse._3.trailingNewline) fun f
      TryExcept a b c d e f g ->
        TryExcept a b c d <$>
        (if isNothing f && isNothing g
         then
           fmap
             NonEmpty.fromList
             ((_last._4.trailingNewline) fun $ NonEmpty.toList e)
         else pure e) <*>
        (if isNothing g
         then (traverse._3.trailingNewline) fun f
         else pure f) <*>
        (traverse._3.trailingNewline) fun g
      TryFinally a b c d e f g ->
        TryFinally a b c d e f <$>
        trailingNewline fun g
      For a b c d e f g h i ->
        For a b c d e f g <$>
        (if isNothing i
         then trailingNewline fun h
         else pure h) <*>
        (traverse._3.trailingNewline) fun i
      ClassDef a b c d e f g ->
        ClassDef a b c d e f <$> trailingNewline fun g
      With a b c d e f ->
        With a b c d e <$> trailingNewline fun f
  setTrailingNewline s n =
    case s of
      Fundef a b c d e f g h i j k ->
        Fundef a b c d e f g h i j $ setTrailingNewline k n
      If a b c d e f g ->
        If a b c d
        (if null f && isNothing g
         then setTrailingNewline e n
         else e)
        (if isNothing g
         then over (_last._4) (flip setTrailingNewline n) f
         else f)
        (over (mapped._3) (flip setTrailingNewline n) g)
      While a b c d e f ->
        While a b c d
        (if isNothing f
         then setTrailingNewline e n
         else e)
        (over (mapped._3) (flip setTrailingNewline n) f)
      TryExcept a b c d e f g ->
        TryExcept a b c d
        (if isNothing f && isNothing g
         then
             NonEmpty.fromList
             (over (_last._4) (flip setTrailingNewline n) $ NonEmpty.toList e)
         else e)
        (if isNothing g
         then over (mapped._3) (flip setTrailingNewline n) f
         else f)
        (over (mapped._3) (flip setTrailingNewline n) g)
      TryFinally a b c d e f g ->
        TryFinally a b c d e f $
        setTrailingNewline g n
      For a b c d e f g h i ->
        For a b c d e f g
        (if isNothing i
         then setTrailingNewline h n
         else h)
        (over (mapped._3) (flip setTrailingNewline n) i)
      ClassDef a b c d e f g ->
        ClassDef a b c d e f $ setTrailingNewline g n
      With a b c d e f ->
        With a b c d e $ setTrailingNewline f n

makeLenses ''WithItem
makeLenses ''ExceptAs
makeLenses ''Block
