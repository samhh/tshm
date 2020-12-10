module Main (main) where

import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void String

data Value
  = ValuePrimitive String
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

pFunction :: Parser Function
pFunction = Function <$> optional pTypeArgs <*> pParams <*> pReturn

pValue :: Parser Value
pValue = choice
  [ ValueFunction <$> pFunction
  , ValuePrimitive <$> some (choice [alphaNumChar, char '<', char '>', char '[', char ']', char ',', char ' '])
  ]

pDeclaration :: Parser Declaration
pDeclaration = Declaration <$> pName <*> pValue <* eof

main :: IO ()
main = putStrLn "WIP: REPL only for now."

