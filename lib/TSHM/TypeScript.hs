module TSHM.TypeScript where

import           Prelude

data TsOperator
  = TsOperatorIntersection
  | TsOperatorUnion
  deriving (Eq, Show)

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
  | TsTypeObject [(String, TsType)]
  | TsTypeObjectReference String String
  | TsTypeGeneric String [TsType]
  | TsTypeSubtype String TsType
  | TsTypeReflection String
  | TsTypeKeysOf TsType
  | TsTypeFunction Function
  | TsTypeExpression TsOperator TsType TsType
  | TsTypeGrouped TsType
  deriving (Eq, Show)

data Function = Function
  { functionTypeArgs :: Maybe [TsType]
  , functionParams   :: [TsType]
  , functionReturn   :: TsType
  } deriving (Eq, Show)

data Declaration = Declaration
  { declarationName :: String
  , declarationType :: TsType
  } deriving (Eq, Show)

