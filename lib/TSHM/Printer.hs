module TSHM.Printer (fDeclaration) where

import           Data.Char       (toLower)
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
fTsType TsTypeVoid                  = "void"
fTsType TsTypeUndefined             = "undefined"
fTsType TsTypeNull                  = "null"
fTsType (TsTypeBoolean x)           = if x then "true" else "false"
fTsType (TsTypeMisc [x])            = pure . toLower $ x
fTsType (TsTypeMisc x)              = x
fTsType (TsTypeStringLiteral x)     = x
fTsType (TsTypeNumberLiteral x)     = x
fTsType (TsTypeTuple xs)            = "[" <> (intercalate ", " . fmap fTsType $ xs) <> "]"
fTsType (TsTypeGeneric x ys)        = "(" <> x <> " " <> (intercalate " " . fmap fTsType $ ys) <> ")"
fTsType (TsTypeSubtype x y)         = x <> " extends " <> fTsType y
fTsType (TsTypeObject [])           = "{}"
fTsType (TsTypeObject xs)           = "{ " <> (intercalate ", " . fmap fObjectPair $ xs) <> " }"
fTsType (TsTypeObjectReference x k) = x <> "[\"" <> k <> "\"]"
fTsType (TsTypeFunction x)          = fFunction x
fTsType (TsTypeExpression x y z)    = fTsType y <> " " <> fOperator x <> " " <> fTsType z

fDeclaration :: Declaration -> String
fDeclaration x = declarationName x <> " :: " <> fTsType (declarationType x)

