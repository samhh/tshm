module TSHM.Printer (fDeclaration) where

import           Prelude
import           TSHM.TypeScript

fParams :: [Value] -> String
fParams []  = "()"
fParams [x] = fValue x
fParams xs  = "(" <> intercalate ", " (fmap fValue xs) <> ")"

fFunction :: Function -> String
fFunction x = fParams (functionParams x) <> " -> " <> fValue (functionReturn x)

fValue :: Value -> String
fValue ValueVoid              = "()"
fValue (ValuePrimitive x)     = x
fValue (ValueStringLiteral x) = x
fValue (ValueGeneric x ys)    = "(" <> x <> " todo:" <> show (length ys) <> ")"
fValue (ValueFunction x)      = fFunction x

fDeclaration :: Declaration -> String
fDeclaration x = declarationName x <> " :: " <> fValue (declarationValue x)

