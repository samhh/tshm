module TSHM.Printer (fDeclaration) where

import           Prelude
import           TSHM.TypeScript

fParams :: [TsType] -> String
fParams []  = "()"
fParams [x] = fTsType x
fParams xs  = "(" <> intercalate ", " (fmap fTsType xs) <> ")"

fFunction :: Function -> String
fFunction x = fParams (functionParams x) <> " -> " <> fTsType (functionReturn x)

fTsType :: TsType -> String
fTsType TsTypeVoid              = "()"
fTsType (TsTypePrimitive x)     = x
fTsType (TsTypeStringLiteral x) = x
fTsType (TsTypeGeneric x ys)    = "(" <> x <> " todo:" <> show (length ys) <> ")"
fTsType (TsTypeFunction x)      = fFunction x

fDeclaration :: Declaration -> String
fDeclaration x = declarationName x <> " :: " <> fTsType (declarationType x)

