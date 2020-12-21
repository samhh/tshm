module TSHM.TypeScript where

import           Prelude

data TsOperator
  = TsOperatorIntersection
  | TsOperatorUnion
  deriving (Eq, Show)

data Partial a
  = Required a
  | Optional a
  deriving (Eq, Show)

type ObjectLiteral = [Partial (String, TsType)]

data TsType
  = TsTypeVoid
  | TsTypeNull
  | TsTypeUndefined
  | TsTypeBoolean Bool
  | TsTypeMisc String
  | TsTypeStringLiteral String
  -- This is represented as a string because numbers are hard and tend to
  -- differ a fair amount between languages. We don't actually need to do any
  -- arithmetic, so this keeps things simple!
  | TsTypeNumberLiteral String
  | TsTypeTuple [TsType]
  | TsTypeObject ObjectLiteral
  | TsTypeObjectReference TsType String
  | TsTypeGeneric String (NonEmpty TsType)
  | TsTypeSubtype String TsType
  | TsTypeReflection String
  | TsTypeKeysOf TsType
  | TsTypeFunction Function
  | TsTypeExpression TsOperator TsType TsType
  | TsTypeGrouped TsType
  deriving (Eq, Show)

data Param
  = Normal TsType
  | Rest TsType
  deriving (Eq, Show)

data Function = Function
  { functionTypeArgs :: Maybe (NonEmpty TsType)
  , functionParams   :: [Partial Param]
  , functionReturn   :: TsType
  } deriving (Eq, Show)

data ConstDeclaration = ConstDeclaration
  { constDeclarationName :: String
  , constDeclarationType :: TsType
  } deriving (Eq, Show)

data FunctionDeclaration = FunctionDeclaration
  { functionDeclarationName :: String
  , functionDeclarationType :: Function
  } deriving (Eq, Show)

fromFunctionDeclaration :: FunctionDeclaration -> ConstDeclaration
fromFunctionDeclaration (FunctionDeclaration x y) = ConstDeclaration x (TsTypeFunction y)

data Alias = Alias
  { aliasName     :: String
  , aliasTypeArgs :: Maybe (NonEmpty TsType)
  , aliasType     :: TsType
  } deriving (Eq, Show)

data Interface = Interface
  { interfaceName     :: String
  , interfaceTypeArgs :: Maybe (NonEmpty TsType)
  , interfaceExtends  :: Maybe TsType
  , interfaceType     :: ObjectLiteral
  } deriving (Eq, Show)

fromInterface :: Interface -> Alias
fromInterface x = Alias (interfaceName x) (interfaceTypeArgs x) t
  where obj = TsTypeObject $ interfaceType x
        t = case interfaceExtends x of
          Nothing -> obj
          Just st -> TsTypeExpression TsOperatorIntersection obj st

data Signature
  = SignatureAlias Alias
  | SignatureInterface Interface
  | SignatureConstDeclaration ConstDeclaration
  | SignatureFunctionDeclaration FunctionDeclaration
  deriving (Eq, Show)

