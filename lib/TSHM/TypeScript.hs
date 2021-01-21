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
  | OKeyComputed TExpr
  | OKeyIndex TExpr
  deriving (Eq, Show)

data ObjectPair
  = ObjectPair Mutant Partial (ObjectKey, TExpr)
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
  | ObjectMapped (Maybe ModMut) (Maybe ModOpt) (String, TExpr, Maybe TExpr) TExpr
  deriving (Eq, Show)

type TypeArg = (TExpr, Maybe TExpr)

data TemplateToken
  = TemplateStr String
  | TemplateExpr TExpr
  deriving (Eq, Show)

data TExpr
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
  | TTuple [TExpr]
  | TObject Object
  | TIndexedAccess TExpr TExpr
  | TDotAccess TExpr String
  | TGeneric String (NonEmpty TypeArg)
  | TSubtype String TExpr
  | TLambda Lambda
  | TUnOp UnOp TExpr
  | TBinOp BinOp TExpr TExpr
  | TCond TExpr TExpr TExpr TExpr
  -- This is defined here as opposed to with the other unary operators as it
  -- can only precede a new identifier.
  | TInfer String
  | TGrouped TExpr
  deriving (Eq, Show)

data ParamScope
  = Normal
  | Rest
  deriving (Eq, Show)

type Param = (Partial, ParamScope, TExpr)

data Lambda = Lambda
  { lambdaTypeArgs :: Maybe (NonEmpty TypeArg)
  , lambdaParams   :: [Param]
  , lambdaReturn   :: TExpr
  } deriving (Eq, Show)

data Import
  = ImportDef String
  | ImportNamed (NonEmpty String)
  | ImportAll String
  | ImportDefAndNamed String (NonEmpty String)
  | ImportDefAndAll { defIdent :: String, allIdent :: String }
  deriving (Eq, Show)

data ImportDec = ImportDec
  { importDecFrom     :: String
  , importDecContents :: Import
  } deriving (Eq, Show)

data ExportDec
  = ExportDef TExpr
  deriving (Eq, Show)

data ConstDec = ConstDec
  { constDecName :: String
  , constDecType :: TExpr
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
  , aliasType     :: TExpr
  } deriving (Eq, Show)

data Interface = Interface
  { interfaceName     :: String
  , interfaceTypeArgs :: Maybe (NonEmpty TypeArg)
  , interfaceExtends  :: Maybe TExpr
  , interfaceType     :: Object
  } deriving (Eq, Show)

data EnumKey
  = EKeyIdent String
  | EKeyStr String
  deriving (Eq, Show)

data EnumMember = EnumMember EnumKey (Maybe TExpr)
  deriving (Eq, Show)

data SEnum = SEnum
  { enumName    :: String
  , enumMembers :: [EnumMember]
  } deriving (Eq, Show)

fromInterface :: Interface -> Alias
fromInterface x = Alias (interfaceName x) (interfaceTypeArgs x) t
  where t = let obj = TObject $ interfaceType x
             in case interfaceExtends x of
                Nothing -> obj
                Just st -> TBinOp BinOpIntersection st obj

data Signature
  = SignatureImportDec ImportDec
  -- Concerns exports not captured by any of the other signature variants below.
  | SignatureExportDec ExportDec
  | SignatureAlias Alias
  | SignatureInterface Interface
  | SignatureConstDec ConstDec
  | SignatureFunctionDec (NonEmpty FunctionDec)
  | SignatureEnum SEnum
  deriving (Eq, Show)

