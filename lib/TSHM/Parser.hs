module TSHM.Parser where

import           Data.Void                  ()
import           Prelude
import           TSHM.TypeScript
import           Text.Megaparsec            hiding (many, some)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

pType :: Parser TsType
pType = try (pArraySpecial arrayables) <|> (TsTypeFunction <$> pFunction) <|> arrayables
  where arrayables = choice
          [ TsTypeVoid <$ string "void"
          , TsTypeStringLiteral <$> pStringLiteral
          , try pGeneric
          , TsTypePrimitive <$> some (choice [alphaNumChar, char ' ', char '&', char '|'])
          ]

pGeneric :: Parser TsType
pGeneric = TsTypeGeneric <$> some alphaNumChar <*> pTypeArgs

pArraySpecial :: Parser TsType -> Parser TsType
pArraySpecial p = TsTypeGeneric "Array" . pure <$> p <* string "[]"

pStringLiteral :: Parser String
pStringLiteral = stringLiteral '\'' <|> stringLiteral '"'
  where stringLiteral :: Char -> Parser String
        stringLiteral c = char c *> manyTill L.charLiteral (char c)

pFunction :: Parser Function
pFunction = Function <$> optional pTypeArgs <*> pParams <*> pReturn

pName :: Parser String
pName = optional (string "export ") *> string "declare const " *> some alphaNumChar <* string ": "

pTypeArgs :: Parser [TsType]
pTypeArgs = between (char '<') (char '>') (sepBy1 pTypeArg (string ", "))
  where pTypeArg :: Parser TsType
        pTypeArg = f <$> some (choice [alphaNumChar, char ' ', char '[', char ']']) <*> optional pTypeArgs
        f x = \case
          Just y  -> TsTypeGeneric x y
          Nothing -> TsTypePrimitive x

pParams :: Parser [TsType]
pParams = between (char '(') (char ')') pInnerParams
  where pInnerParams :: Parser [TsType]
        pInnerParams = sepBy pParam (string ", ")

        pParam :: Parser TsType
        pParam = optional (string "...") *> some alphaNumChar *> string ": " *> optional (string "new ") *> pType

pReturn :: Parser TsType
pReturn = string " => " *> pType

pDeclaration :: Parser Declaration
pDeclaration = Declaration <$> pName <*> pType <* eof

