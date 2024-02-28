module HRayLib3d.ScriptEngine.HRuby.Syntax where

-- TODO:
-- - add support for 'array of strings' ?
-- - add support for macro preprocessing
-- - add support for optional macro(s) include and require
-- - type checking
-- - check for constant expression where expected
-- - error reporting
-- - pretty-printing
-- - basic queries (inputs and outputs of script)
-- - proper testing (HUnit and QuickCheck)
-- - use hpc with the tests
-- - scoping
-- - clean module import/export
-- - order of Syntax data types and Pretty instances should be the same
-- - build with no warning
-- - use hlint
-- - use parsec 3
-- - handle all possible newlines (\n, \r, \r\n, \n\r)

----------------------------------------------------------------------
-- Abstract syntax tree
----------------------------------------------------------------------

data TranslationUnit = TranslationUnit [ExternalDeclaration] -- at least one
  deriving (Show, Eq)

data ExternalDeclaration =
    -- function declarations should be at top level (page 28)
    FunctionDeclaration FunctionPrototype
  | FunctionDefinition FunctionPrototype Compound
  | Declaration Declaration
  deriving (Show, Eq)

-- TODO clean
data Declaration =
-- e.g. layout (origin_upper_left) in vec4 gl_FragCoord;
--      struct name { ... };
--      struct name { ... } name;
    InitDeclaration InvariantOrType [InitDeclarator]
  | Precision PrecisionQualifier TypeSpecifierNoPrecision
  | Block TypeQualifier String [Field] (Maybe (String, Maybe (Maybe Expr))) -- constant expression
-- e.g. layout (origin_upper_left) in; TODO check if it is only used for default layout.
  | TQ TypeQualifier
  deriving (Show, Eq)

-- TODO regroup String (Maybe (Maybe Expr)) as Declarator and use it for
-- StructDeclarator.
data InitDeclarator = InitDecl String (Maybe (Maybe Expr)) (Maybe Expr) -- constant expression; assignment expression
  deriving (Show, Eq)

data InvariantOrType = InvariantDeclarator | TypeDeclarator FullType
  deriving (Show, Eq)

data FunctionPrototype = FuncProt FullType String [ParameterDeclaration]
  deriving (Show, Eq)

data ParameterDeclaration =
  ParameterDeclaration (Maybe ParameterTypeQualifier)
                       (Maybe ParameterQualifier)
                       TypeSpecifier
                       (Maybe (String, Maybe Expr)) -- constant expression
  deriving (Show, Eq)

data FullType = FullType (Maybe TypeQualifier) TypeSpecifier
  deriving (Show, Eq)

-- sto
-- lay [sto]
-- int [sto]
-- inv [sto]
-- inv int sto
data TypeQualifier =
    TypeQualSto StorageQualifier
  | TypeQualLay LayoutQualifier (Maybe StorageQualifier)
  | TypeQualInt InterpolationQualifier (Maybe StorageQualifier)
  | TypeQualInv InvariantQualifier (Maybe StorageQualifier)
  | TypeQualInv3 InvariantQualifier InterpolationQualifier StorageQualifier
  deriving (Show, Eq)

data TypeSpecifier = TypeSpec (Maybe PrecisionQualifier) TypeSpecifierNoPrecision
  deriving (Show, Eq)

data InvariantQualifier = Invariant
  deriving (Show, Eq)

data InterpolationQualifier =
    Smooth
  | Flat
  | NoPerspective
  deriving (Show, Eq)

data LayoutQualifier = Layout [LayoutQualifierId]
  deriving (Show, Eq)

data LayoutQualifierId = LayoutQualId String (Maybe Expr) -- TODO Expr should be IntConstant
  deriving (Show, Eq)

data Statement =
  -- declaration statement
    DeclarationStatement Declaration
  -- jump statement
  | Continue
  | Break
  | Return (Maybe Expr)
  | Discard -- fragment shader only
  -- compound statement
  | CompoundStatement Compound
  -- expression statement
  | ExpressionStatement (Maybe Expr)
  -- selection statement
  | SelectionStatement Expr Statement (Maybe Statement)
  -- switch statement
  | SwitchStatement Expr [Statement]
  | CaseLabel CaseLabel
  -- iteration statement
  | While Condition Statement -- no new scope
  | DoWhile Statement Expr
  | For (Either (Maybe Expr) Declaration) (Maybe Condition) (Maybe Expr) Statement
    -- 1st stmt: expression or declaration, 2nd: no new scope
  deriving (Show, Eq)

data Compound = Compound [Statement]
  deriving (Show, Eq)

data Condition =
    Condition Expr
  | InitializedCondition FullType String Expr -- assignment expression
  deriving (Show, Eq)

data CaseLabel = Case Expr | Default
  deriving (Show, Eq)

data StorageQualifier =
    Const
  | Attribute -- vertex only
  | Varying
  | CentroidVarying
  | In
  | Out
  | CentroidIn
  | CentroidOut
  | Uniform
  deriving (Show, Eq)

data TypeSpecifierNoPrecision = TypeSpecNoPrecision TypeSpecifierNonArray (Maybe (Maybe Expr)) -- constant expression
  deriving (Show, Eq)

data TypeSpecifierNonArray =
    Void
  | Def
  | Alias 
  | Class 
  | RModule 
  | Undef
  | Float
  | Double
  | Int
  | UInt
  | Bool
  | List
  | Dictionary
  | StructSpecifier (Maybe String) [Field]
  | TypeName String -- TODO user-defined type, should verify if it is declared
  deriving (Show, Eq)

data PrecisionQualifier = HighP | MediumP | LowP
  deriving (Show, Eq)

-- TODO The type qualifier can be present only when there is one or more declarators.
-- There other restrictions, see 4.1.8.
data Field = Field (Maybe TypeQualifier) TypeSpecifier [StructDeclarator]
  deriving (Show, Eq)

data StructDeclarator = StructDeclarator String (Maybe (Maybe Expr)) -- constant expression
  deriving (Show, Eq)

data Expr =
  -- primaryExpression
    Variable String
  | IntConstant IntConstantKind Integer
  | FloatConstant Float
  | BoolConstant Bool
  -- postfixExpression
  | Bracket Expr Expr
  | FieldSelection Expr String
  | MethodCall Expr FunctionIdentifier Parameters
  | FunctionCall FunctionIdentifier Parameters
  | PostInc Expr
  | PostDec Expr
  | PreInc Expr
  | PreDec Expr
  -- unary expression
  | UnaryPlus Expr
  | UnaryNegate Expr
  | UnaryNot Expr
  | UnaryOneComplement Expr
  -- binary expression
  | Mul Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  | Add Expr Expr
  | Sub Expr Expr
  | LeftShift Expr Expr
  | RightShift Expr Expr
  | Lt Expr Expr
  | Gt Expr Expr
  | Lte Expr Expr
  | Gte Expr Expr
  | Cbe Expr Expr
  | Equ Expr Expr
  | Neq Expr Expr
  | BitAnd Expr Expr
  | BitXor Expr Expr
  | BitOr Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | Selection Expr Expr Expr -- ternary _ ? _ : _ operator
  -- assignment, the left Expr should be unary expression
  | Equal Expr Expr
  | MulAssign Expr Expr
  | DivAssign Expr Expr
  | ModAssign Expr Expr
  | AddAssign Expr Expr
  | SubAssign Expr Expr
  | LeftAssign Expr Expr
  | RightAssign Expr Expr
  | AndAssign Expr Expr
  | XorAssign Expr Expr
  | OrAssign Expr Expr
  -- sequence
  | Sequence Expr Expr
  deriving (Show, Eq)

data IntConstantKind = Hexadecimal | Octal | Decimal
  deriving (Show, Eq)

data Parameters = ParamVoid | Params [Expr]
  deriving (Show, Eq)

data ParameterQualifier = InParameter | OutParameter | InOutParameter
  deriving (Show, Eq)

data ParameterTypeQualifier = ConstParameter
  deriving (Show, Eq)

data FunctionIdentifier =
    -- TODO could be refine (I think a precision qualifier is not permitted,
    -- nor a complete struct definition)
    FuncIdTypeSpec TypeSpecifier
  | FuncId String
  deriving (Show, Eq)