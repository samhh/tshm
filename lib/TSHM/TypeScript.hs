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
  | TsTypeTuple [TsType]
  | TsTypeObject [(String, TsType)]
  | TsTypeGeneric String [TsType]
  | TsTypeFunction Function
  | TsTypeExpression TsOperator TsType TsType
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

