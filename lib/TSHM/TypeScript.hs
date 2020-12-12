module TSHM.TypeScript where

import           Prelude

data Value
  = ValueVoid
  | ValuePrimitive String
  | ValueStringLiteral String
  | ValueGeneric String [Value]
  | ValueFunction Function
  deriving (Eq, Show)

data Function = Function
  { functionTypeArgs :: Maybe [Value]
  , functionParams   :: [Value]
  , functionReturn   :: Value
  } deriving (Eq, Show)

data Declaration = Declaration
  { declarationName  :: String
  , declarationValue :: Value
  } deriving (Eq, Show)

