module TSHM.Printer (fDeclaration) where

import           Prelude
import           TSHM.TypeScript

fParams :: [TsType] -> String
fParams []  = "()"
fParams [x] = fTsType x
fParams xs  = "(" <> intercalate ", " (fmap fTsType xs) <> ")"

fFunction :: Function -> String
fFunction x = fParams (functionParams x) <> " -> " <> fTsType (functionReturn x)

fObjectPair :: (String, TsType) -> String
fObjectPair (k, v) = k <> ": " <> fTsType v

fOperator :: TsOperator -> String
fOperator TsOperatorIntersection = "&"
fOperator TsOperatorUnion        = "|"

fTsType :: TsType -> String
fTsType TsTypeVoid               = "()"
fTsType TsTypeUndefined          = "()"
fTsType TsTypeNull               = "()"
fTsType (TsTypeBoolean x)        = if x then "true" else "false"
fTsType (TsTypeMisc x)           = x
fTsType (TsTypeStringLiteral x)  = x
fTsType (TsTypeGeneric x ys)     = "(" <> x <> " todo:" <> show (length ys) <> ")"
fTsType (TsTypeObject [])        = "{}"
fTsType (TsTypeObject xs)        = "{ " <> (intercalate ", " . fmap fObjectPair $ xs) <> " }"
fTsType (TsTypeFunction x)       = fFunction x
fTsType (TsTypeExpression x y z) = fTsType y <> " " <> fOperator x <> " " <> fTsType z

fDeclaration :: Declaration -> String
fDeclaration x = declarationName x <> " :: " <> fTsType (declarationType x)

