module TSHM.TypeScript where

import           Prelude

data UnOp
  = UnOpReflection
  | UnOpKeys
  | UnOpReadonly
  deriving (Eq, Show)

data BinOp
  = BinOpIntersection
  | BinOpUnion
  deriving (Eq, Show)

data Mutant
  = Immut
  | Mut
  deriving (Eq, Show)

data Partial
  = Required
  | Optional
  deriving (Eq, Show)

data ObjectKey
  = OKeyIdent String
  | OKeyStr String
  | OKeyNum String
  | OKeyComputed Expr
  | OKeyIndex Expr
  deriving (Eq, Show)

data ObjectPair
  = ObjectPair Mutant Partial (ObjectKey, Expr)
  deriving (Eq, Show)

-- Represents the positive or negative nature of the readonly modifier in
-- a mapped type. See:
-- https://www.typescriptlang.org/docs/handbook/2/mapped-types.html#mapping-modifiers
data ModMut
  = AddMut
  | RemMut
  deriving (Eq, Show)

-- Represents the positive or negative nature of the optionality modifier in
-- a mapped type. See:
-- https://www.typescriptlang.org/docs/handbook/2/mapped-types.html#mapping-modifiers
data ModOpt
  = AddOpt
  | RemOpt
  deriving (Eq, Show)

data Object
  = ObjectLit [ObjectPair]
  | ObjectMapped (Maybe ModMut) (Maybe ModOpt) (String, Expr, Maybe Expr) Expr
  deriving (Eq, Show)

type TypeArg = (Expr, Maybe Expr)

data TemplateToken
  = TemplateStr String
  | TemplateExpr Expr
  deriving (Eq, Show)

data Expr
  = TAny
  | TUnknown
  | TNever
  | TVoid
  | TNull
  | TUndefined
  | TUniqueSymbol
  | TBoolean Bool
  | TMisc String
  | TString String
  | TTemplate [TemplateToken]
  -- This is represented as a string because numbers are hard and tend to
  -- differ a fair amount between languages. We don't actually need to do any
  -- arithmetic, so this keeps things simple!
  | TNumber String
  | TTuple [Expr]
  | TObject Object
  | TIndexedAccess Expr Expr
  | TDotAccess Expr String
  | TGeneric String (NonEmpty TypeArg)
  | TSubtype String Expr
  | TLambda Lambda
  | TUnOp UnOp Expr
  | TBinOp BinOp Expr Expr
  | TCond Expr Expr Expr Expr
  -- This is defined here as opposed to with the other unary operators as it
  -- can only precede a new identifier.
  | TInfer String
  | TGrouped Expr
  deriving (Eq, Show)

data ParamScope
  = Normal
  | Rest
  deriving (Eq, Show)

type Param = (Partial, ParamScope, Expr)

data Lambda = Lambda
  { lambdaTypeArgs :: Maybe (NonEmpty TypeArg)
  , lambdaParams   :: [Param]
  , lambdaReturn   :: Expr
  } deriving (Eq, Show)

data ConstDec = ConstDec
  { constDecName :: String
  , constDecType :: Expr
  } deriving (Eq, Show)

data FunctionDec = FunctionDec
  { functionDecName :: String
  , functionDecType :: Lambda
  } deriving (Eq, Show)

fromFunctionDec :: FunctionDec -> ConstDec
fromFunctionDec (FunctionDec x y) = ConstDec x (TLambda y)

data Alias = Alias
  { aliasName     :: String
  , aliasTypeArgs :: Maybe (NonEmpty TypeArg)
  , aliasType     :: Expr
  } deriving (Eq, Show)

data Interface = Interface
  { interfaceName     :: String
  , interfaceTypeArgs :: Maybe (NonEmpty TypeArg)
  , interfaceExtends  :: Maybe Expr
  , interfaceType     :: Object
  } deriving (Eq, Show)

data EnumKey
  = EKeyIdent String
  | EKeyStr String
  deriving (Eq, Show)

data EnumMember = EnumMember EnumKey (Maybe Expr)
  deriving (Eq, Show)

data SEnum = SEnum
  { enumName    :: String
  , enumMembers :: [EnumMember]
  } deriving (Eq, Show)

fromInterface :: Interface -> Alias
fromInterface x = Alias (interfaceName x) (interfaceTypeArgs x) t
  where obj = TObject $ interfaceType x
        t = case interfaceExtends x of
          Nothing -> obj
          Just st -> TBinOp BinOpIntersection obj st

data Signature
  = SignatureAlias Alias
  | SignatureInterface Interface
  | SignatureConstDec ConstDec
  | SignatureFunctionDec (NonEmpty FunctionDec)
  | SignatureEnum SEnum
  deriving (Eq, Show)

