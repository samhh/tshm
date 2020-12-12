module TSHM.Printer (fDeclaration) where

import           Data.List       (intercalate)
import           Prelude
import           TSHM.TypeScript

fTypeArgs :: [TypeArg] -> String
fTypeArgs = const ""

fParams :: [Value] -> String
fParams []  = "()"
fParams [x] = fValue x
fParams xs  = "(" <> intercalate ", " (fmap fValue xs) <> ")"

fFunction :: Function -> String
fFunction x = maybe "" fTypeArgs (functionTypeArgs x) <> fParams (functionParams x) <> " -> " <> fValue (functionReturn x)

fValue :: Value -> String
fValue ValueVoid          = "()"
fValue (ValuePrimitive x) = x
fValue (ValueFunction x)  = fFunction x

fDeclaration :: Declaration -> String
fDeclaration x = declarationName x <> " :: " <> fValue (declarationValue x)

