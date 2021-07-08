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
  = OKeyIdent Text
  | OKeyStr Text
  | OKeyNum Text
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
  | ObjectMapped (Maybe ModMut) (Maybe ModOpt) (Text, TExpr, Maybe TExpr) TExpr
  deriving (Eq, Show)

type TypeArg = (TExpr, Maybe TExpr)

data TemplateToken
  = TemplateStr Text
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
  | TMisc Text
  | TString Text
  | TTemplate [TemplateToken]
  -- This is represented as a string because numbers are hard and tend to
  -- differ a fair amount between languages. We don't actually need to do any
  -- arithmetic, so this keeps things simple!
  | TNumber Text
  | TTuple [TExpr]
  | TObject Object
  | TIndexedAccess TExpr TExpr
  | TDotAccess TExpr Text
  | TGeneric Text (NonEmpty TypeArg)
  | TSubtype Text TExpr
  | TLambda Lambda
  | TUnOp UnOp TExpr
  | TBinOp BinOp TExpr TExpr
  | TCond TExpr TExpr TExpr TExpr
  -- This is defined here as opposed to with the other unary operators as it
  -- can only precede a new identifier.
  | TInfer Text
  | TGrouped TExpr
  deriving (Eq, Show)

-- | A named parameter is coherent in terms of reflection as TypeScript
-- disallows looking ahead.
data ParamName
  = ParamNamed Text
  | ParamDestructured
  deriving (Eq, Show)

data ParamScope
  = Normal
  | Rest
  deriving (Eq, Show)

data Param = Param
  { paramName  :: ParamName
  , paramValue :: (Partial, ParamScope, TExpr)
  } deriving (Eq, Show)

data Lambda = Lambda
  { lambdaTypeArgs :: Maybe (NonEmpty TypeArg)
  , lambdaParams   :: [Param]
  , lambdaReturn   :: TExpr
  } deriving (Eq, Show)

data Import
  = ImportDef Text
  | ImportNamed (NonEmpty Text)
  | ImportAll Text
  | ImportDefAndNamed Text (NonEmpty Text)
  | ImportDefAndAll { defIdent :: Text, allIdent :: Text }
  deriving (Eq, Show)

data ImportDec = ImportDec
  { importDecFrom     :: Text
  , importDecContents :: Import
  } deriving (Eq, Show)

data ExportNamedRef
  = ExportNamedRefUnchanged Text
  | ExportNamedRefRenamed Text Text
  deriving (Eq, Show)

data ExportDec
  = ExportDef TExpr
  | ExportNamedRefs [ExportNamedRef]
  deriving (Eq, Show)

data ConstDec = ConstDec
  { constDecName     :: Text
  , constDecType     :: TExpr
  , constDecExported :: Bool
  } deriving (Eq, Show)

data FunctionDec = FunctionDec
  { functionDecName :: Text
  , functionDecType :: Lambda
  , functionDecExported :: Bool
  } deriving (Eq, Show)

fromFunctionDec :: FunctionDec -> ConstDec
fromFunctionDec (FunctionDec x y z) = ConstDec x (TLambda y) z

data Alias = Alias
  { aliasName     :: Text
  , aliasTypeArgs :: Maybe (NonEmpty TypeArg)
  , aliasType     :: TExpr
  } deriving (Eq, Show)

data Interface = Interface
  { interfaceName     :: Text
  , interfaceTypeArgs :: Maybe (NonEmpty TypeArg)
  , interfaceExtends  :: Maybe TExpr
  , interfaceType     :: Object
  } deriving (Eq, Show)

data EnumKey
  = EKeyIdent Text
  | EKeyStr Text
  deriving (Eq, Show)

data EnumMember = EnumMember EnumKey (Maybe TExpr)
  deriving (Eq, Show)

data SEnum = SEnum
  { enumName    :: Text
  , enumMembers :: [EnumMember]
  } deriving (Eq, Show)

fromInterface :: Interface -> Alias
fromInterface x = Alias (interfaceName x) (interfaceTypeArgs x) t
  where t = let obj = TObject $ interfaceType x
             in case interfaceExtends x of
                Nothing -> obj
                Just st -> TBinOp BinOpIntersection st obj

data Statement
  = StatementImportDec ImportDec
  -- Concerns exports not captured by any of the other signature variants below.
  | StatementExportDec ExportDec
  | StatementAlias Alias
  | StatementInterface Interface
  | StatementConstDec ConstDec
  | StatementFunctionDec (NonEmpty FunctionDec)
  | StatementEnum SEnum
  deriving (Eq, Show)

type AST = NonEmpty ScopedStatement

