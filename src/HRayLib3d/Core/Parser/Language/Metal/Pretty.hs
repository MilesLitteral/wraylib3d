{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
module HRayLib3d.Core.Parser.Language.Metal.Pretty where

-- import Text.Printf
-- import Text.PrettyPrint.HughesPJClass
-- import HRayLib3d.Core.Parser.Language.Metal.Syntax
-- import HRayLib3d.Core.Parser.Language.Metal.Parser
-- import Prelude hiding ((<>))

-- ----------------------------------------------------------------------
-- -- helpers (TODO clean)
-- ----------------------------------------------------------------------

-- type Assoc = (Rational -> Rational, Rational -> Rational)

-- assocLeft, assocRight, assocNone :: Assoc
-- assocLeft  = (id,bump)
-- assocRight = (bump,id)
-- assocNone  = (bump,bump)

-- bump :: Rational -> Rational
-- bump = (+ 0.5)

-- prettyBinary :: Pretty a =>
--   PrettyLevel -> Rational -> Rational -> Assoc -> String -> a -> a -> Doc
-- prettyBinary l p op (lf,rf) o e1 e2 = prettyParen (p > op) $
--   pPrintPrec l (lf op) e1 <+> text o <+> pPrintPrec l (rf op) e2

-- option :: Pretty a => Maybe a -> Doc
-- option Nothing = empty
-- option (Just x) = pPrint x

-- indexing :: Pretty a => Maybe (Maybe a) -> Doc
-- indexing Nothing = empty
-- indexing (Just Nothing) = brackets empty
-- indexing (Just (Just e)) = brackets $ pPrint e

-- indexing' :: Pretty a => Maybe (String, Maybe a) -> Doc
-- indexing' Nothing = empty
-- indexing' (Just (i, Nothing)) = text i
-- indexing' (Just (i, Just e)) = text i <> brackets (pPrint e)

-- initialize :: Pretty a => Maybe a -> Doc
-- initialize Nothing = empty
-- initialize (Just e) = char ' ' <> equals <+> pPrint e

-- ident :: Pretty a => Maybe (String, Maybe (Maybe a)) -> Doc
-- ident Nothing = empty
-- ident (Just (i, Nothing)) = text i
-- ident (Just (i, Just Nothing)) = text i <> brackets empty
-- ident (Just (i, Just (Just e))) = text i <> brackets (pPrint e)

-- initialize' :: Pretty a => Maybe (String, Maybe a) -> Doc
-- initialize' Nothing = empty
-- initialize' (Just (i, Nothing)) = text i
-- initialize' (Just (i, Just e)) = text i <+> char '=' <+> pPrint e

-- ----------------------------------------------------------------------
-- -- Pretty instances
-- ----------------------------------------------------------------------

-- instance Pretty TranslationUnit where
--   pPrint (TranslationUnit ds) = vcat $ map pPrint ds
-- --  pPrint (Alternative p e) = text "(" <> nest 2 (vcat [pPrint p, pPrint e]) <> text ")"

-- instance Pretty ExternalDeclaration where
--   pPrint (FunctionDeclaration p) = pPrint p <> semi
--   pPrint (FunctionDefinition p s) = vcat [pPrint p, pPrint s]
--   pPrint (Declaration d) = pPrint d

-- instance Pretty Declaration where
--   pPrint (InitDeclaration it ds) = pPrint it <+> hsep (punctuate comma (map pPrint ds)) <> semi
--   pPrint (Precision pq t) = text "precision" <+> pPrint pq <+> pPrint t <> semi
--   pPrint (Block tq i ds n) = vcat [pPrint tq <+> text i, lbrace, nest 2 (vcat $ map pPrint ds), rbrace <+> ident n <> semi]
--   pPrint (TQ tq) = pPrint tq <> semi

-- instance Pretty InitDeclarator where
--   pPrint (InitDecl i a b) = text i <> indexing a <> initialize b

-- instance Pretty InvariantOrType where
--   pPrint InvariantDeclarator = text "invariant"
--   pPrint (TypeDeclarator ft) = pPrint ft

-- instance Pretty FullType where
--   pPrint (FullType tq ts) = option tq <+> pPrint ts

-- instance Pretty TypeQualifier where
--   pPrint (TypeQualSto sq) = pPrint sq
--   pPrint (TypeQualLay lq sq) = pPrint lq <+> option sq
--   pPrint (TypeQualInt iq sq) = pPrint iq <+> option sq
--   pPrint (TypeQualInv iq sq) = pPrint iq <+> option sq
--   pPrint (TypeQualInv3 iq iq' sq) = pPrint iq <+> pPrint iq' <+> pPrint sq

-- instance Pretty StorageQualifier where
--   pPrint q = case q of
--     Const -> text "const"
--     Attribute -> text "attribute"
--     Varying -> text "varying"
--     CentroidVarying -> text "centroid varying"
--     In -> text "in"
--     Out -> text "out"
--     CentroidIn -> text "centroid in"
--     CentroidOut -> text "centroid out"
--     Uniform -> text "uniform"

-- instance Pretty LayoutQualifier where
--   pPrint (Layout is) = text "layout" <+> char '(' <>
--     (hsep $ punctuate comma $ map pPrint is) <> char ')'

-- instance Pretty LayoutQualifierId where
--   pPrint (LayoutQualId i Nothing) = text i
--   pPrint (LayoutQualId i (Just e)) = text i <+> char '=' <+> pPrint e

-- instance Pretty InterpolationQualifier where
--   pPrint q = case q of
--     Smooth        -> text "smooth"
--     Flat          -> text "flat"
--     NoPerspective -> text "noperspective"

-- instance Pretty InvariantQualifier where
--   pPrint Invariant = text "invariant"

-- instance Pretty TypeSpecifier where
--   pPrint (TypeSpec (Just pq) t) = pPrint pq <+> pPrint t
--   pPrint (TypeSpec Nothing t) = pPrint t

-- instance Pretty PrecisionQualifier where
--   pPrint HighP = text "highp"
--   pPrint MediumP = text "mediump"
--   pPrint LowP = text "lowp"

-- instance Pretty TypeSpecifierNoPrecision where
--   pPrint (TypeSpecNoPrecision t a) = pPrint t <+> indexing a
 
-- instance Pretty TypeSpecifierNonArray where
--   pPrint t = case t of
--     Void        -> text "void"
--     Int         -> text "int"
--     Float       -> text "float"
--     Double      -> text "double"
--     Bool        -> text "bool"
--     UInt        -> text "uint"
--     UInt2       -> text "uint2"
--     Char        -> text "char"
--     UChar       -> text "uchar"
--     UShort      -> text "ushort"
--     ULong       -> text "ulong"
--     Short       -> text "short"
--     Long        -> text "long"
--     Half        -> text "half"
--     Half4       -> text "half4"
--     Size_t      -> text "size_t"
--     Ptrdiff_t   -> text "ptrdiff_t"
--     Intptr_t    -> text "intptr_t"
--     Uintpyt_t   -> text "uintpyt_t"
--     Kernel      -> text "kernel"
--     Device      -> text "device"
--     Texture     -> text "texture"
--     Texture2d   -> text "texture2d"
--     AccessRead  -> text "access::read"
--     AccessWrite -> text "access::write"
--     AttributeQualifier i ds ->
--       vcat [lAttrQualifier, pPrint ds, rAttrQualifier] -- text "struct" <+> i', 
--       where i' = case i of { Nothing -> empty ; Just n -> text n }

-- instance Pretty Field where
--   pPrint (Field tq s ds) =
--     option tq <+> pPrint s <+> hsep (punctuate comma $ map pPrint ds) <> semi

-- -- instance Pretty (AttributeQualifier (Maybe String) Field) where
-- --   pPrint (AttributeQualifier i e) = Just (i, e)

-- instance Pretty Expr where
--   pPrintPrec l p e = case e of
--   -- primaryExpression
--     Variable v -> text v
--     IntConstant Decimal i -> text (show i)
--     IntConstant Hexadecimal i -> text (printf "0x%x" i)
--     IntConstant Octal i -> text (printf "0%o" i)
--     FloatConstant f -> text (show f)
--     BoolConstant True -> text "true"
--     BoolConstant False -> text "false"
--   -- postfixExpression
--     Bracket e1 e2 -> prettyParen (p > 16) $
--       pPrintPrec l 16 e1 <> brackets (pPrint e2)
--     FieldSelection e1 f -> prettyParen (p > 16) $
--       pPrintPrec l 16 e1 <> char '.' <> text f
--     MethodCall e1 i ps -> prettyParen (p > 16) $
--       pPrintPrec l 16 e1 <> char '.' <> pPrint i <+> parens (pPrint ps)
--     FunctionCall i ps -> prettyParen (p > 16) $
--       pPrint i <+> parens (pPrint ps)
--     PostInc e1 -> prettyParen (p > 15) $
--       pPrintPrec l 15 e1 <+> text "++"
--     PostDec e1 -> prettyParen (p > 15) $
--       pPrintPrec l 15 e1 <+> text "--"
--     PreInc e1 -> prettyParen (p > 15) $
--       text "++" <+> pPrintPrec l 15 e1
--     PreDec e1 -> prettyParen (p > 15) $
--       text "--" <+> pPrintPrec l 15 e1
--   -- unary expression
--     UnaryPlus e1 -> prettyParen (p > 15) $
--       text "+" <> pPrintPrec l 15 e1
--     UnaryNegate e1 -> prettyParen (p > 15) $
--       text "-" <> pPrintPrec l 15 e1
--     UnaryNot e1 -> prettyParen (p > 15) $
--       text "!" <> pPrintPrec l 15 e1
--     UnaryOneComplement e1 -> prettyParen (p > 15) $
--       text "~" <> pPrintPrec l 15 e1
--   -- binary expression
--     Mul        e1 e2 -> prettyBinary l p 14 assocLeft "*" e1 e2
--     Div        e1 e2 -> prettyBinary l p 14 assocLeft "/" e1 e2
--     Mod        e1 e2 -> prettyBinary l p 14 assocLeft "%" e1 e2
--     Add        e1 e2 -> prettyBinary l p 13 assocLeft "+" e1 e2
--     Sub        e1 e2 -> prettyBinary l p 13 assocLeft "-" e1 e2
--     LeftShift  e1 e2 -> prettyBinary l p 12 assocLeft "<<" e1 e2
--     RightShift e1 e2 -> prettyBinary l p 12 assocLeft ">>" e1 e2
--     Lt         e1 e2 -> prettyBinary l p 11 assocLeft "<" e1 e2
--     Gt         e1 e2 -> prettyBinary l p 11 assocLeft ">" e1 e2
--     Lte        e1 e2 -> prettyBinary l p 11 assocLeft "<=" e1 e2
--     Gte        e1 e2 -> prettyBinary l p 11 assocLeft ">=" e1 e2
--     Equ        e1 e2 -> prettyBinary l p 10 assocLeft "==" e1 e2
--     Neq        e1 e2 -> prettyBinary l p 10 assocLeft "!=" e1 e2
--     BitAnd     e1 e2 -> prettyBinary l p 9 assocLeft "&" e1 e2
--     BitXor     e1 e2 -> prettyBinary l p 8 assocLeft "^" e1 e2
--     BitOr      e1 e2 -> prettyBinary l p 7 assocLeft "|" e1 e2
--     And        e1 e2 -> prettyBinary l p 6 assocLeft "&&" e1 e2
-- -- TODO Xor 5 "^^"
--     Or         e1 e2 -> prettyBinary l p 4 assocLeft "||" e1 e2
--     Selection e1 e2 e3 -> prettyParen (p > 3) $
--       pPrintPrec l 3 e1 <+> char '?' <+> pPrintPrec l 3 e2
--       <+> char ':' <+> pPrintPrec l 3 e3
--   -- assignment, the left Expr should be unary expression
--     Equal       e1 e2 -> prettyBinary l p 2 assocRight "=" e1 e2
--     MulAssign   e1 e2 -> prettyBinary l p 2 assocRight "*=" e1 e2
--     DivAssign   e1 e2 -> prettyBinary l p 2 assocRight "/=" e1 e2
--     ModAssign   e1 e2 -> prettyBinary l p 2 assocRight "%=" e1 e2
--     AddAssign   e1 e2 -> prettyBinary l p 2 assocRight "+=" e1 e2
--     SubAssign   e1 e2 -> prettyBinary l p 2 assocRight "-=" e1 e2
--     LeftAssign  e1 e2 -> prettyBinary l p 2 assocRight "<<=" e1 e2
--     RightAssign e1 e2 -> prettyBinary l p 2 assocRight ">>=" e1 e2
--     AndAssign   e1 e2 -> prettyBinary l p 2 assocRight "&=" e1 e2
--     XorAssign   e1 e2 -> prettyBinary l p 2 assocRight "^=" e1 e2
--     OrAssign    e1 e2 -> prettyBinary l p 2 assocRight "|=" e1 e2
--   -- sequence
--     Sequence e1 e2 -> prettyParen (p > 1) $
--       pPrintPrec l 1 e1 <> char ',' <+> pPrintPrec l 1 e2

-- instance Pretty FunctionIdentifier where
--   pPrint (FuncIdTypeSpec t) = pPrint t
--   pPrint (FuncId i) = text i

-- instance Pretty Parameters where
--   pPrint ParamVoid = empty
--   pPrint (Params es) = hsep $ punctuate comma $ map pPrint es

-- instance Pretty FunctionPrototype where
--   pPrint (FuncProt t i ps) = pPrint t <+> text i <+> char '(' <> hsep (punctuate comma $ map pPrint ps) <> text ")"

-- instance Pretty ParameterDeclaration where
--   pPrint (ParameterDeclaration tq q s i) =
--     option tq <+> option q <+> pPrint s <+> indexing' i

-- instance Pretty ParameterTypeQualifier  where
--   pPrint ConstParameter = text "const"

-- instance Pretty ParameterQualifier where
--   pPrint InParameter = text "in"
--   pPrint OutParameter = text "out"
--   pPrint InOutParameter = text "inout"

-- instance Pretty Statement where
--   pPrint s = case s of
--   -- declaration statement
--     DeclarationStatement d -> pPrint d
--   -- jump statement
--     Continue -> text "continue" <> semi
--     Break -> text "break" <> semi
--     Return e -> text "return" <+> option e <> semi
--     Discard -> text "discard" <> semi
--   -- compound statement
--     CompoundStatement c -> pPrint c
--   -- expression statement
--     ExpressionStatement e -> option e <> semi
--   -- selection statement
--     SelectionStatement e s1 s2 -> vcat [text "if" <+> parens (pPrint e), nest 2 $ pPrint s1, option s2]
--   -- switch statement
--     SwitchStatement e s1 -> vcat [text "switch" <+> parens (pPrint e), lbrace, nest 2 $ vcat $ map pPrint s1, rbrace]
--     CaseLabel l -> pPrint l
--   -- iteration statement
--     While c s1 -> vcat [text "while" <+> parens (pPrint c), pPrint s1]
--     DoWhile s1 e -> vcat [text "do", pPrint s1, text "while" <+> parens (pPrint e)]
--     For (Left e1) c e2 s1 -> vcat [text "for", parens (option e1 <+> semi <+> option c <+> semi <+> option e2), pPrint s1]
--     For (Right d) c e2 s1 -> vcat [text "for", parens (pPrint d <+> semi <+> option c <+> semi <+> option e2), pPrint s1]

-- instance Pretty Compound where
--   pPrint (Compound s) = vcat [lbrace, nest 2 $ vcat $ map pPrint s, rbrace]

-- instance Pretty Condition where
--   pPrint (Condition e) = pPrint e
--   pPrint (InitializedCondition t i e) = pPrint t <+> pPrint i <+> pPrint e

-- instance Pretty CaseLabel where
--   pPrint  (Case e) = text "case" <+> pPrint e <> colon
--   pPrint Default = text "default:"

