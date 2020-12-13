module TSHM.Printer (fDeclaration) where

import           Data.Char       (toLower)
import           Prelude
import           TSHM.TypeScript

fParam :: Param -> String
fParam (Normal x) = fTsType' x
fParam (Rest x) = "..." <> fTsType' x

fParams :: [Param] -> String
fParams []  = "()"
fParams [x] = fParam x
fParams xs  = "(" <> intercalate ", " (fmap fParam xs) <> ")"

fFunction :: Function -> String
fFunction x = fParams (functionParams x) <> " -> " <> fTsType' (functionReturn x)

fGeneric :: (String, NonEmpty TsType) -> Location -> String
fGeneric (x, ys) NestedGeneric = "(" <> fGeneric (x, ys) Other <> ")"
fGeneric (x, ys) Other = x <> " " <> (intercalate " " . fmap (`fTsType` NestedGeneric) $ toList ys)

fObjectPair :: Partial (String, TsType) -> String
fObjectPair (Required (k, v)) = k <> ": " <> fTsType' v
fObjectPair (Optional (k, v)) = k <> "?: " <> fTsType' v

fOperator :: TsOperator -> String
fOperator TsOperatorIntersection = "&"
fOperator TsOperatorUnion        = "|"

data Location
  = NestedGeneric
  | Other

fTsType :: TsType -> Location -> String
fTsType TsTypeVoid                  = const "void"
fTsType TsTypeUndefined             = const "undefined"
fTsType TsTypeNull                  = const "null"
fTsType (TsTypeBoolean x)           = const $ if x then "true" else "false"
fTsType (TsTypeMisc [x])            = const $ pure . toLower $ x
fTsType (TsTypeMisc x)              = const x
fTsType (TsTypeStringLiteral x)     = const x
fTsType (TsTypeNumberLiteral x)     = const x
fTsType (TsTypeTuple xs)            = const $ "[" <> (intercalate ", " . fmap fTsType' $ xs) <> "]"
fTsType (TsTypeGeneric x ys)        = fGeneric (x, ys)
fTsType (TsTypeSubtype x y)         = const $ x <> " extends " <> fTsType' y
fTsType (TsTypeKeysOf x)            = const $ "keyof " <> fTsType' x
fTsType (TsTypeReflection x)        = const $ "typeof " <> x
fTsType (TsTypeObject [])           = const "{}"
fTsType (TsTypeObject xs)           = const $ "{ " <> (intercalate ", " . fmap fObjectPair $ xs) <> " }"
fTsType (TsTypeObjectReference x k) = const $ fTsType' x <> "[\"" <> k <> "\"]"
fTsType (TsTypeFunction x)          = const $ fFunction x
fTsType (TsTypeExpression x y z)    = const $ fTsType' y <> " " <> fOperator x <> " " <> fTsType' z
fTsType (TsTypeGrouped x)           = const $ "(" <> fTsType' x <> ")"

fTsType' :: TsType -> String
fTsType' = (`fTsType` Other)

fDeclaration :: Declaration -> String
fDeclaration x = declarationName x <> " :: " <> fTsType' (declarationType x)

