module TSHM.TypeScript where

import           Prelude

data TsType
  = TsTypeVoid
  | TsTypePrimitive String
  | TsTypeStringLiteral String
  | TsTypeGeneric String [TsType]
  | TsTypeFunction Function
  deriving (Eq, Show)

data Function = Function
  { functionTypeArgs :: Maybe [TsType]
  , functionParams   :: [TsType]
  , functionReturn   :: TsType
  } deriving (Eq, Show)

data Declaration = Declaration
  { declarationName  :: String
  , declarationType :: TsType
  } deriving (Eq, Show)

