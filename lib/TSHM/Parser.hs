module TSHM.Parser where

import           Data.Void                  ()
import           Prelude
import           TSHM.TypeScript
import           Text.Megaparsec            hiding (many, some)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

pValue :: Parser Value
pValue = choice
  [ ValueVoid <$ string "void"
  , ValueFunction <$> pFunction
  , ValueStringLiteral <$> pStringLiteral
  , ValuePrimitive <$> some (choice [alphaNumChar, char '<', char '>', char '[', char ']', char ' ', char '&', char '|'])
  ]

pStringLiteral :: Parser String
pStringLiteral = stringLiteral '\'' <|> stringLiteral '"'
  where stringLiteral :: Char -> Parser String
        stringLiteral c = char c *> manyTill L.charLiteral (char c)

pFunction :: Parser Function
pFunction = Function <$> optional pTypeArgs <*> pParams <*> pReturn

pName :: Parser String
pName = optional (string "export ") *> string "declare const " *> some alphaNumChar <* string ": "

pTypeArgs :: Parser [TypeArg]
pTypeArgs = between (char '<') (char '>') (sepBy1 pTypeArg (string ", "))
  where pTypeArg :: Parser TypeArg
        pTypeArg = f <$> some (choice [alphaNumChar, char ' ', char '[', char ']']) <*> optional pTypeArgs
        f x = \case
          Just y  -> TypeArgHigherOrder x y
          Nothing -> TypeArgPrimitive x

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

