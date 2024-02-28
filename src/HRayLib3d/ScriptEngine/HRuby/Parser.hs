module HRayLib3d.ScriptEngine.HRuby.Parser where

import Prelude hiding (break, exponent)
import Text.ParserCombinators.Parsec hiding (State, parse)
import Text.ParserCombinators.Parsec.Expr
import HRayLib3d.ScriptEngine.HRuby.Syntax

----------------------------------------------------------------------
-- Parser state, hold a symbol table.
----------------------------------------------------------------------

data RS = RS

type P a = GenParser Char RS a

----------------------------------------------------------------------
-- Reserved words
----------------------------------------------------------------------

-- List of keywords.
keywords :: [String]
keywords = concat $ map words $
  [ "__ENCODING__"
  , "__LINE__"
  , "__FILE__"
  , "BEGIN END defined?"
  , "alias class module undef"
  , "def do"
  , "for while when case until unless if elsif else next then break redo"
  , "nil"
  , "and or in not"
  , "super yield"
  , "begin rescue retry ensure"
  , "self return"
  , "end"
  ]

-- List of keywords reserved for future use.
-- Use to update Ruby beyond 2.2.0
reservedWords :: [String]
reservedWords = concat $ map words $
  [ "=begin"
  , "=end"
  ]

----------------------------------------------------------------------
-- Convenience parsers
----------------------------------------------------------------------

comment :: P ()
comment = do
  _ <- string "=begin"
  _ <- manyTill anyChar (try $ string "=end")
  _ <- char '#'
  _ <- manyTill anyChar ((newline >> return ()) <|> eof)
  return ()

blank :: P ()
blank = try comment <|> (space >> return ())

-- Acts like p and discards any following space character.
lexeme :: P a -> P a
lexeme p = do
  x <- p
  skipMany blank
  return x

parse :: [Char] -> Either ParseError TranslationUnit
parse =
  runParser (do {skipMany blank ; r <- translationUnit ; eof ; return r})
    S "Ruby"

----------------------------------------------------------------------
-- Lexical elements (tokens)
----------------------------------------------------------------------

semicolon :: P ()
semicolon = lexeme $ char ';' >> return ()

comma :: P ()
comma = lexeme $ char ',' >> return ()

colon :: P ()
colon = lexeme $ char ':' >> return ()

lbrace :: P ()
lbrace = lexeme $ char '{' >> return ()

rbrace :: P ()
rbrace = lexeme $ char '}' >> return ()

lbracket :: P ()
lbracket = lexeme $ char '[' >> return ()

rbracket :: P ()
rbracket = lexeme $ char ']' >> return ()

lparen :: P ()
lparen = lexeme $ char '(' >> return ()

rparen :: P ()
rparen = lexeme $ char ')' >> return ()

-- Try to parse a given string, making sure it is not a
-- prefix of an identifier.
keyword :: String -> P ()
keyword w = lexeme $ try (string w >> notFollowedBy identifierTail)

-- Parses and returns an identifier.
-- TODO an identifier can't start with "gl_" unless
-- it is to redeclare a predeclared "gl_" identifier.
identifier :: P String
identifier = lexeme $ do
  h <- identifierHead
  t <- many identifierTail
  check (h:t)
  where check i | i `elem` reservedWords = fail $
          i ++ " is reserved"
                | i `elem` keywords = fail $
          i ++ " is a keyword"
                | otherwise = checkUnderscore i i
        checkUnderscore i ('_':'_':_) = fail $
          i ++ " is reserved (two consecutive underscores)"
        checkUnderscore i (_:cs) = checkUnderscore i cs
        checkUnderscore i [] = return i

-- TODO the size of the int should fit its type.
intConstant :: P Expr
intConstant = choice
  [ hexadecimal
  , octal
  , badOctal >> fail "Invalid octal number"
  , decimal
  ]

floatingConstant :: P Expr
floatingConstant = choice
  [ floatExponent
  , floatPoint
  , pointFloat
  ]

nilConstant :: P Expr
nilConstant = choice [ nil ]

-- Try to parse a given string, and allow identifier characters
-- (or anything else) to directly follow.
operator :: String -> P String
operator = lexeme . try . string

----------------------------------------------------------------------
-- Lexical elements helpers
----------------------------------------------------------------------

identifierHead :: P Char
identifierHead = letter <|> char '_'

identifierTail :: P Char
identifierTail = alphaNum <|> char '_'

hexadecimal :: P Expr
hexadecimal = lexeme $ try $ do
  _ <- char '0'
  _ <- oneOf "Xx"
  d <- many1 hexDigit
  m <- optionMaybe $ oneOf "Uu" -- TODO
  return $ IntConstant Hexadecimal $ read ("0x" ++ d)

nil :: P Expr
nil = lexeme $ try $ do
  d <- oneOf "nil" -- TODO
  if read d 
    then return $ Nil
    else return ()

octal :: P Expr
octal = lexeme $ try $ do
  _ <- char '0'
  d <- many1 octDigit
  m <- optionMaybe $ oneOf "Uu" -- TODO
  return $ IntConstant Octal $ read  ("0o" ++ d)

badOctal :: P ()
badOctal = lexeme $ try  $ char '0' >> many1 hexDigit >> return ()

decimal :: P Expr
decimal = lexeme $ try $ do
  d <- many1 digit
  notFollowedBy (char '.' <|> (exponent >> return ' '))
  m <- optionMaybe $ oneOf "Uu" -- TODO
  return $ IntConstant Decimal $ read d

floatExponent :: P Expr
floatExponent = lexeme $ try $ do
  d <- many1 digit
  e <- exponent
  m <- optionMaybe $ oneOf "Ff" -- TODO
  return $ FloatConstant $ read $ d ++ e

floatPoint :: P Expr
floatPoint = lexeme $ try $ do
  d <- many1 digit
  _ <- char '.'
  d' <- many digit
  let d'' = if null d' then "0" else d'
  e <- optionMaybe exponent
  m <- optionMaybe $ oneOf "Ff" -- TODO
  return $ FloatConstant $ read $ d ++ "." ++ d'' ++ maybe "" id e

pointFloat :: P Expr
pointFloat = lexeme $ try $ do
  _ <- char '.'
  d <- many1 digit
  e <- optionMaybe exponent
  m <- optionMaybe $ oneOf "Ff"
  return $ FloatConstant $ read $ "0." ++ d ++ maybe "" id e

exponent :: P String
exponent = lexeme $ try $ do
  _ <- oneOf "**"
  s <- optionMaybe (oneOf "+-")
  d <- many1 digit
  return $ "e" ++ maybe "" (:[]) s ++ d

----------------------------------------------------------------------
-- Tables for buildExpressionParser
----------------------------------------------------------------------

infixLeft :: String -> (a -> a -> a) -> Operator Char S a
infixLeft s r = Infix (lexeme (try $ string s) >> return r) AssocLeft

infixLeft' :: String -> (a -> a -> a) -> Operator Char S a
infixLeft' s r = Infix (lexeme (try $ string s >> notFollowedBy (char '=')) >> return r) AssocLeft

infixLeft'' :: Char -> (a -> a -> a) -> Operator Char S a
infixLeft'' c r = Infix (lexeme (try $ char c >> notFollowedBy (oneOf (c:"="))) >> return r) AssocLeft

infixRight :: String -> (a -> a -> a) -> Operator Char S a
infixRight s r = Infix (lexeme (try $ string s) >> return r) AssocRight

conditionalTable :: [[Operator Char S Expr]]
conditionalTable =
  [ [infixLeft' "*"  Mul, infixLeft' "/" Div, infixLeft' "%" Mod]
  , [infixLeft' "+"  Add, infixLeft' "-" Sub]
  , [infixLeft' "<<" LeftShift, infixLeft' ">>" RightShift]
  , [infixLeft' "<"  Lt, infixLeft' ">" Gt]
  , [infixLeft "<="  Lte, infixLeft ">=" Gte]
  , [infixLeft "=="  Equ, infixLeft ".eql"  Equ, infixLeft "!=" Neq]
  , [infixLeft "===" Equal, infixLeft "equal?"  Equal]
  , [infixLeft "<=>" Cbe]
  , [infixLeft'' '&' BitAnd]
  , [infixLeft' "^"  BitXor]
  , [infixLeft'' '|' BitOr]
  , [infixLeft "&&"  And]
  , [infixLeft "||"  Or]
  --, [infixLeft "!"  Neq]
  --, [infixLeft' "?" TernaryIf, infixLeft' ":" TernaryElse]
  ]

assignmentTable :: [[Operator Char S Expr]]
assignmentTable =
  [ [infixRight "="   Equal]
  , [infixRight "+="  AddAssign]
  , [infixRight "-="  SubAssign]
  , [infixRight "*="  MulAssign]
  , [infixRight "/="  DivAssign]
  , [infixRight "%="  ModAssign]
  , [infixRight "**=" ExpAssign]
  , [infixRight "||=" OrAssign]
  ]

expressionTable :: [[Operator Char S Expr]]
expressionTable =
  [ [infixLeft "," Sequence]
  ]

----------------------------------------------------------------------
-- Grammar
----------------------------------------------------------------------

primaryExpression :: P Expr
primaryExpression = choice
  [ Variable `fmap` try identifier
  -- int constant
  , intConstant
  -- float constant
  , floatingConstant
  -- bool constant
  , keyword "true"  >> return (BoolConstant True)
  , keyword "false" >> return (BoolConstant False)
  -- expression within parentheses
  , between lparen rparen expression
  ]

postfixExpression :: P Expr
postfixExpression = do
  e <- try (functionCallGeneric >>= \(i,p) -> return (FunctionCall i p))
       <|> primaryExpression
  p <- many $ choice
    [ between lbracket rbracket integerExpression >>= return . flip Bracket
    , dotFunctionCallGeneric
    , dotFieldSelection
    , operator "++" >> return PostInc
    , operator "--" >> return PostDec
    ]
  return $ foldl (flip ($)) e p

dotFunctionCallGeneric :: P (Expr -> Expr)
dotFunctionCallGeneric =
  lexeme (try $ string "." >> functionCallGeneric) >>= \(i,p) -> return (\e -> MethodCall e i p)

dotFieldSelection :: P (Expr -> Expr)
dotFieldSelection =
  lexeme (try $ string "." >> identifier) >>= return . flip FieldSelection

integerExpression :: P Expr
integerExpression = expression

-- Those productions are pushed inside postfixExpression.
-- functionCall = functionCallOrMethod
-- functionCallOrMethod = functionCallGeneric <|> postfixExpression DOT functionCallGeneric

functionCallGeneric :: P (FunctionIdentifier, Parameters)
functionCallGeneric = do
  i <- functionCallHeader
  p <- choice
    [ keyword "def" >> return ParamVoid -- TODO: Functions that take parameters or don't, void is an optional condition rather than a type in ruby
    , assignmentExpression `sepBy` comma >>= return . Params
    ]
  rparen
  return (i, p)
  
-- Those productions are pushed inside functionCallGeneric.
-- functionCallHeaderNoParameters = undefined
-- functionCallHeaderWithParameters = undefined

functionCallHeader :: P FunctionIdentifier
functionCallHeader = do
  i <- functionIdentifier
  lparen
  return i

functionIdentifier :: P FunctionIdentifier
functionIdentifier = choice
  [ try identifier >>= return . FuncId
  , typeSpecifier >>= return . FuncIdTypeSpec -- TODO if the 'identifier' is declared as a type, should be this case
  -- no need for fieldSelection
  ]

unaryExpression :: P Expr
unaryExpression = do
  p <- many $ choice
    [ operator "++" >> return PreInc
    , operator "--" >> return PreDec
    , operator "+" >> return UnaryPlus
    , operator "-" >> return UnaryNegate
    , operator "!" >> return UnaryNot
    , operator "~" >> return UnaryOneComplement
    ] 
  e <- postfixExpression
  return $ foldr ($) e p

conditionalExpression :: P Expr
conditionalExpression = do
  loe <- buildExpressionParser conditionalTable unaryExpression
  ter <- optionMaybe $ do
    _ <- lexeme (string "?")
    e <- expression
    _ <- lexeme (string ":")
    a <- assignmentExpression
    return (e, a)
  case ter of
    Nothing -> return loe
    Just (e, a) -> return $ Selection loe e a

assignmentExpression :: P Expr
assignmentExpression = buildExpressionParser assignmentTable conditionalExpression

expression :: P Expr
expression = buildExpressionParser expressionTable assignmentExpression

constantExpression :: P Expr
constantExpression = conditionalExpression

-- The Ruby grammar here includes a function definition but we don't
-- do this here because they should occur only at top level (page 28).
-- Function definitions are handled in externalDefinition instead.
declaration :: P Declaration
declaration = choice
  [ try $ do
       t <- fullySpecifiedType
       l <- idecl `sepBy` comma
       return $ InitDeclaration (TypeDeclarator t) l
  , do keyword "invariant"
       i <- idecl `sepBy` comma
       return $ InitDeclaration InvariantDeclarator i
  , do keyword "precision"
       q <- precisionQualifier
       s <- typeSpecifierNoPrecision
       return $ Precision q s
  , do q <- typeQualifier
       choice
         [ semicolon >> return (TQ q)
         , do i <- identifier
              lbrace
              s <- structDeclarationList
              rbrace
              m <- optionMaybe $ do
                j <- identifier
                n <- optionMaybe $ between lbracket rbracket $ optionMaybe constantExpression
                return (j,n)
              return $ Block q i s m
         ]
  ]
  where idecl = do
          i <- identifier
          m <- optionMaybe $ between lbracket rbracket $
            optionMaybe constantExpression
          j <- optionMaybe $ lexeme (string "=") >> initializer
          return $ InitDecl i m j

functionPrototype :: P FunctionPrototype
functionPrototype = do
  (t, i, p) <- functionDeclarator
  rparen
  return $ FuncProt t i p

functionDeclarator :: P (FullType, String, [ParameterDeclaration])
functionDeclarator = do
  (t, i) <- functionHeader
  p <- parameterDeclaration `sepBy` comma
  return (t, i, p)

-- inside functionDeclarator
-- functionHeaderWithParameters = undefined

functionHeader :: P (FullType, String)
functionHeader = do
  t <- fullySpecifiedType
  i <- identifier
  lparen
  return (t, i)

parameterDeclaration :: P ParameterDeclaration
parameterDeclaration = do
  tq <- optionMaybe parameterTypeQualifier
  q <- optionMaybe parameterQualifier
  s <- typeSpecifier
  m <- optionMaybe $ do
    i <- identifier
    b <- optionMaybe $ between lbracket rbracket constantExpression -- FIXME can't the bracket be empty, i.e. a[] ?
    return (i,b)
  return $ ParameterDeclaration tq q s m

parameterQualifier :: P ParameterQualifier
parameterQualifier = choice
  -- "empty" case handled in the caller
  [ (try . lexeme . string) "inout" >> return InOutParameter
  , (try . lexeme . string) "in" >> return InParameter
  , (try . lexeme . string) "out" >> return OutParameter
  ]

fullySpecifiedType :: P FullType
fullySpecifiedType = choice
  [ try typeSpecifier >>= return . FullType Nothing
  , do q <- typeQualifier
       s <- typeSpecifier
       return $ FullType (Just q) s
  ]

invariantQualifier :: P InvariantQualifier
invariantQualifier = keyword "invariant" >> return Invariant

interpolationQualifier :: P InterpolationQualifier
interpolationQualifier = choice
  [ keyword "smooth" >> return Smooth
  , keyword "flat" >> return Flat
  , keyword "noperspective" >> return NoPerspective
  ]

layoutQualifier :: P LayoutQualifier
layoutQualifier = do
  keyword "layout"
  lparen
  q <- layoutQualifierId `sepBy` comma
  rparen
  return $ Layout q

layoutQualifierId :: P LayoutQualifierId
layoutQualifierId = do
  i <- identifier
  c <- optionMaybe $ lexeme (string "=") >> intConstant
  return $ LayoutQualId i c

parameterTypeQualifier :: P ParameterTypeQualifier
parameterTypeQualifier = keyword "const" >> return ConstParameter

typeQualifier :: P TypeQualifier
typeQualifier = choice
  [ do s <- storageQualifier
       return $ TypeQualSto s
  , do l <- layoutQualifier
       s <- optionMaybe storageQualifier
       return $ TypeQualLay l s
  , do i <- interpolationQualifier
       s <- optionMaybe storageQualifier
       return $ TypeQualInt i s
  , do i <- invariantQualifier
       choice
         [ do j <- interpolationQualifier
              s <- storageQualifier
              return $ TypeQualInv3 i j s
         , do s <- optionMaybe storageQualifier
              return $ TypeQualInv i s
         ]
  ]

-- TODO see 4.3 for restrictions
storageQualifier :: P StorageQualifier
storageQualifier = choice
  [ keyword "const" >> return Const
  , keyword "attribute" >> return Attribute -- TODO vertex only, is deprecated
  , keyword "varying" >> return Varying -- deprecated
  , keyword "in" >> return In
  , keyword "out" >> return Out
  , keyword "centroid" >> (choice
    [ keyword "varying" >> return CentroidVarying -- deprecated
    , keyword "in" >> return CentroidIn
    , keyword "out" >> return CentroidOut
    ])
  , keyword "uniform" >> return Uniform
  ]

typeSpecifier :: P TypeSpecifier
typeSpecifier = choice
  [ do q <- try precisionQualifier
       s <- typeSpecifierNoPrecision
       return $ TypeSpec (Just q) s
  , typeSpecifierNoPrecision >>= return . TypeSpec Nothing
  ]

typeSpecifierNoPrecision :: P TypeSpecifierNoPrecision
typeSpecifierNoPrecision = do
  s <- typeSpecifierNonArray
  choice
    [ try (lbracket >> rbracket) >> return (TypeSpecNoPrecision s (Just Nothing))
    , lbracket >> constantExpression >>= \c -> rbracket >> return (TypeSpecNoPrecision s (Just $ Just c))
    , return $ TypeSpecNoPrecision s Nothing
    ]

-- Basic types, structs, and user-defined types.
typeSpecifierNonArray :: P TypeSpecifierNonArray
typeSpecifierNonArray = choice
  [ keyword "void" >> return Void
  , keyword "float" >> return Float
  , keyword "int" >> return Int
  , keyword "uint" >> return UInt
  , keyword "bool" >> return Bool
  , keyword "vec2" >> return Vec2
  , keyword "vec3" >> return Vec3
  , keyword "vec4" >> return Vec4
  , keyword "bvec2" >> return BVec2
  , keyword "bvec3" >> return BVec3
  , keyword "bvec4" >> return BVec4
  , keyword "ivec2" >> return IVec2
  , keyword "ivec3" >> return IVec3
  , keyword "ivec4" >> return IVec4
  , keyword "uvec2" >> return UVec2
  , keyword "uvec3" >> return UVec3
  , keyword "uvec4" >> return UVec4
  , keyword "mat2" >> return Mat2
  , keyword "mat3" >> return Mat3
  , keyword "mat4" >> return Mat4
  , keyword "mat2x2" >> return Mat2x2
  , keyword "mat2x3" >> return Mat2x3
  , keyword "mat2x4" >> return Mat2x4
  , keyword "mat3x2" >> return Mat3x2
  , keyword "mat3x3" >> return Mat3x3
  , keyword "mat3x4" >> return Mat3x4
  , keyword "mat4x2" >> return Mat4x2
  , keyword "mat4x3" >> return Mat4x3
  , keyword "mat4x4" >> return Mat4x4
  , keyword "sampler1D" >> return Sampler1D
  , keyword "sampler2D" >> return Sampler2D
  , keyword "sampler3D" >> return Sampler3D
  , keyword "samplerCube" >> return SamplerCube
  , keyword "sampler1DShadow" >> return Sampler1DShadow
  , keyword "sampler2DShadow" >> return Sampler2DShadow
  , keyword "samplerCubeShadow" >> return SamplerCubeShadow
  , keyword "sampler1DArray" >> return Sampler1DArray
  , keyword "sampler2DArray" >> return Sampler2DArray
  , keyword "sampler1DArrayShadow" >> return Sampler1DArrayShadow
  , keyword "sampler2DArrayShadow" >> return Sampler2DArrayShadow
  , keyword "isampler1D" >> return ISampler1D
  , keyword "isampler2D" >> return ISampler2D
  , keyword "isampler3D" >> return ISampler3D
  , keyword "isamplerCube" >> return ISamplerCube
  , keyword "isampler1DArray" >> return ISampler1DArray
  , keyword "isampler2DArray" >> return ISampler2DArray
  , keyword "usampler1D" >> return USampler1D
  , keyword "usampler2D" >> return USampler2D
  , keyword "usampler3D" >> return USampler3D
  , keyword "usamplerCube" >> return USamplerCube
  , keyword "usampler1DArray" >> return USampler1DArray
  , keyword "usampler2DArray" >> return USampler2DArray
  , keyword "sampler2DRect" >> return Sampler2DRect
  , keyword "sampler2DRectShadow" >> return Sampler2DRectShadow
  , keyword "isampler2DRect" >> return ISampler2DRect
  , keyword "usampler2DRect" >> return USampler2DRect
  , keyword "samplerBuffer" >> return SamplerBuffer
  , keyword "isamplerBuffer" >> return ISamplerBuffer
  , keyword "usamplerBuffer" >> return USamplerBuffer
  , keyword "sampler2DMS" >> return Sampler2DMS
  , keyword "isampler2DMS" >> return ISampler2DMS
  , keyword "usampler2DMS" >> return USampler2DMS
  , keyword "sampler2DMSArray" >> return Sampler2DMSArray
  , keyword "isampler2DMSArray" >> return ISampler2DMSArray
  , keyword "usampler2DMSArray" >> return USampler2DMSArray
  , structSpecifier
  , identifier >>= return . TypeName -- verify if it is declared
  ]

precisionQualifier :: P PrecisionQualifier
precisionQualifier = choice
  [ keyword "highp" >> return HighP
  , keyword "mediump" >> return MediumP
  , keyword "lowp" >> return LowP
  ]

structSpecifier :: P TypeSpecifierNonArray
structSpecifier = do
  keyword "struct"
  i <- optionMaybe identifier
  lbrace
  d <- structDeclarationList
  rbrace
  return $ StructSpecifier i d

structDeclarationList :: P [Field]
structDeclarationList = many1 structDeclaration

structDeclaration :: P Field
structDeclaration = do
  q <- optionMaybe typeQualifier
  s <- typeSpecifier
  l <- structDeclaratorList
  semicolon
  return $ Field q s l

structDeclaratorList :: P [StructDeclarator]
structDeclaratorList = structDeclarator `sepBy` comma

structDeclarator :: P StructDeclarator
structDeclarator = do
  i <- identifier
  choice
    [ do lbracket
         e <- optionMaybe constantExpression
         rbracket
         return $ StructDeclarator i (Just e)
    , return $ StructDeclarator i Nothing
    ]

initializer :: P Expr
initializer = assignmentExpression

declarationStatement :: P Declaration
declarationStatement = declaration

statement :: P Statement
statement = CompoundStatement `fmap` compoundStatement
  <|> simpleStatement

simpleStatement :: P Statement
simpleStatement = choice
  [ declarationStatement >>= return . DeclarationStatement
  , expressionStatement >>= return . ExpressionStatement
  , selectionStatement
  , switchStatement
  , caseLabel >>= return . CaseLabel
  , iterationStatement
  , jumpStatement
  ]

compoundStatement :: P Compound
compoundStatement = choice
  [ try (lbrace >> rbrace) >> return (Compound [])
  , between lbrace rbrace statementList >>= return . Compound
  ]

statementNoNewScope :: P Statement
statementNoNewScope = CompoundStatement `fmap` compoundStatementNoNewScope
  <|> simpleStatement

compoundStatementNoNewScope :: P Compound
compoundStatementNoNewScope = compoundStatement

statementList :: P [Statement]
statementList = many1 statement

expressionStatement :: P (Maybe Expr)
expressionStatement = choice
  [ semicolon >> return Nothing
  , expression >>= \e -> semicolon >> return (Just e)
  ]

selectionStatement :: P Statement
selectionStatement = do
  keyword "if"
  lparen
  c <- expression
  rparen
  t <- statement
  f <- optionMaybe (keyword "else" >> statement)
  return $ SelectionStatement c t f
  
-- inside selectionStatement
-- selectionRestStatement = undefined

condition :: P Condition
condition = choice
  [ expression >>= return . Condition
  , do t <- fullySpecifiedType
       i <- identifier
       _ <- lexeme (string "=")
       j <- initializer
       return $ InitializedCondition t i j
  ]

switchStatement :: P Statement
switchStatement = do
  keyword "switch"
  lparen
  e <- expression
  rparen
  lbrace
  l <- switchStatementList
  rbrace
  return $ SwitchStatement e l

switchStatementList :: P [Statement]
switchStatementList = many statement

caseLabel :: P CaseLabel
caseLabel = choice
  [ keyword "case"    >> expression >>= \e -> colon >> return (Case e)
  , keyword "default" >> colon >> return Default
  ]

iterationStatement :: P Statement
iterationStatement = choice
  [ do keyword "while"
       lparen
       c <- condition
       rparen
       s <- statementNoNewScope
       return $ While c s
  , do keyword "do"
       s <- statement
       keyword "while"
       lparen
       e <- expression
       rparen
       return $ DoWhile s e
  , do keyword "for"
       lparen
       i <- forInitStatement
       c <- optionMaybe condition
       e <- optionMaybe expression
       rparen
       s <- statementNoNewScope
       return $ For i c e s
  ]

forInitStatement :: P (Either (Maybe Expr) Declaration)
forInitStatement = (expressionStatement >>= return . Left)
  <|> (declarationStatement >>= return . Right)

jumpStatement :: P Statement
jumpStatement = choice
  [ keyword "continue" >> semicolon >> return Continue
  , keyword "break" >> semicolon >> return Break 
  , try (keyword "return" >> semicolon) >> return (Return Nothing)
  , keyword "return" >> expression >>= \e -> semicolon >> return (Return $ Just e)
  , keyword "discard" >> semicolon >> return Discard
  ]

translationUnit :: P TranslationUnit
translationUnit = TranslationUnit `fmap` many1 externalDeclaration

externalDeclaration :: P ExternalDeclaration
externalDeclaration = choice
  [ do p <- try functionPrototype
       choice
         [ semicolon >> return (FunctionDeclaration p)
         , compoundStatementNoNewScope >>= return . FunctionDefinition p
         ]
  , Declaration `fmap` declaration
  ]

-- inside externalDeclaration, used only in tests
functionDefinition :: P ExternalDeclaration
functionDefinition = do
  fp <- functionPrototype
  cs <- compoundStatementNoNewScope
  return $ FunctionDefinition fp cs

