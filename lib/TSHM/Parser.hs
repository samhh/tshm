module TSHM.Parser where

import           Data.Void                  ()
import           Prelude
import           TSHM.TypeScript
import           Text.Megaparsec            hiding (many, some)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

pValue :: Parser Value
pValue = try (pArraySpecial arrayables) <|> (ValueFunction <$> pFunction) <|> arrayables
  where arrayables = choice
          [ ValueVoid <$ string "void"
          , ValueStringLiteral <$> pStringLiteral
          , try pGeneric
          , ValuePrimitive <$> some (choice [alphaNumChar, char ' ', char '&', char '|'])
          ]

pGeneric :: Parser Value
pGeneric = ValueGeneric <$> some alphaNumChar <*> pTypeArgs

pArraySpecial :: Parser Value -> Parser Value
pArraySpecial p = ValueGeneric "Array" . pure <$> p <* string "[]"

pStringLiteral :: Parser String
pStringLiteral = stringLiteral '\'' <|> stringLiteral '"'
  where stringLiteral :: Char -> Parser String
        stringLiteral c = char c *> manyTill L.charLiteral (char c)

pFunction :: Parser Function
pFunction = Function <$> optional pTypeArgs <*> pParams <*> pReturn

pName :: Parser String
pName = optional (string "export ") *> string "declare const " *> some alphaNumChar <* string ": "

pTypeArgs :: Parser [Value]
pTypeArgs = between (char '<') (char '>') (sepBy1 pTypeArg (string ", "))
  where pTypeArg :: Parser Value
        pTypeArg = f <$> some (choice [alphaNumChar, char ' ', char '[', char ']']) <*> optional pTypeArgs
        f x = \case
          Just y  -> ValueGeneric x y
          Nothing -> ValuePrimitive x

pParams :: Parser [Value]
pParams = between (char '(') (char ')') pInnerParams
  where pInnerParams :: Parser [Value]
        pInnerParams = sepBy pParam (string ", ")

        pParam :: Parser Value
        pParam = optional (string "...") *> some alphaNumChar *> string ": " *> optional (string "new ") *> pValue

pReturn :: Parser Value
pReturn = string " => " *> pValue

pDeclaration :: Parser Declaration
pDeclaration = Declaration <$> pName <*> pValue <* eof

