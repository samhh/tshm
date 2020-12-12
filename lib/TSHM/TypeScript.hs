module TSHM.TypeScript where

import           Prelude

data Value
  = ValueVoid
  | ValuePrimitive String
  | ValueStringLiteral String
  | ValueFunction Function
  deriving (Eq, Show)

data TypeArg
  = TypeArgPrimitive String
  | TypeArgHigherOrder String [TypeArg]
  deriving (Eq, Show)

data Function = Function
  { functionTypeArgs :: Maybe [TypeArg]
  , functionParams   :: [Value]
  , functionReturn   :: Value
  } deriving (Eq, Show)

data Declaration = Declaration
  { declarationName  :: String
  , declarationValue :: Value
  } deriving (Eq, Show)

