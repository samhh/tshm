module TSHM.Parser where

import           Control.Monad.Combinators.Expr     (Operator (..),
                                                     makeExprParser)
import qualified Control.Monad.Combinators.NonEmpty as NE
import           Data.List                          (foldr1)
import           Data.Void                          ()
import           Prelude
import           TSHM.TypeScript
import           Text.Megaparsec                    hiding (many, some)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer         as L

type Parser = Parsec Void String

operators :: [[Operator Parser TsType]]
operators =
  [ [ Postfix (multi
      (   (TsTypeGeneric "Array" . pure <$ string "[]")
      <|> (flip TsTypeObjectReference <$> between (char '[') (char ']') pStringLiteral)
      )
    )]
  , [ Prefix (TsTypeKeysOf <$ string "keyof ")
    ]
  , [ binary "&" (TsTypeExpression TsOperatorIntersection)
    , binary "|" (TsTypeExpression TsOperatorUnion)
    ]
  ]
    where multi :: Alternative f => f (a -> a) -> f (a -> a)
          multi f = foldr1 (flip (.)) <$> some f

          binary :: String -> (TsType -> TsType -> TsType) -> Operator Parser TsType
          binary x f = InfixR $ f <$ string (" " <> x <> " ")

pType :: Parser TsType
pType = (`makeExprParser` operators) $ optional (string "readonly ") *> choice
  [ try $ TsTypeGrouped <$> between (char '(') (char ')') pType
  , TsTypeVoid <$ string "void"
  , TsTypeNull <$ string "null"
  , TsTypeUndefined <$ string "undefined"
  , TsTypeBoolean <$> ((True <$ string "true") <|> (False <$ string "false"))
  , TsTypeStringLiteral <$> pStringLiteral
  , TsTypeNumberLiteral <$> pNumberLiteral
  , TsTypeTuple <$> pTuple
  , TsTypeObject <$> pObject
  , TsTypeFunction <$> pFunction
  , try pGeneric
  , TsTypeReflection <$> (string "typeof " *> some alphaNumChar)
  , TsTypeMisc <$> pTypeMisc
  ]

pTypeMisc :: Parser String
pTypeMisc = some alphaNumChar

pGeneric :: Parser TsType
pGeneric = TsTypeGeneric <$> pTypeMisc <*> pTypeArgs

pStringLiteral :: Parser String
pStringLiteral = stringLiteral '\'' <|> stringLiteral '"'
  where stringLiteral :: Char -> Parser String
        stringLiteral c =
          let p = char c
           in p *> manyTill L.charLiteral p

pNumberLiteral :: Parser String
pNumberLiteral = (<>) . maybe "" pure <$> optional (char '-') <*> choice
  [ try $ (\x y z -> x <> pure y <> z) <$> some numberChar <*> char '.' <*> some numberChar
  , (:) <$> char '.' <*> some numberChar
  , some numberChar
  ]

pFunction :: Parser Function
pFunction = Function <$> optional pTypeArgs <*> pParams <*> pReturn

pTuple :: Parser [TsType]
pTuple = between (char '[') (char ']') $ sepBy pType (string ", ")

pObject :: Parser ObjectLiteral
pObject = between (string "{ ") (string " }") (sepBy1 pPair (string ", " <|> string "; ")) <|> [] <$ string "{}"
  where pPair :: Parser (Partial (String, TsType))
        pPair = optional (string "readonly ") *> choice
          [ try $ flip fn <$> some alphaNumChar <*> (True <$ string ": " <|> False <$ string "?: ") <*> pType
          , method <$> some alphaNumChar <*> (isJust <$> optional (char '?')) <*> optional pTypeArgs <*> pParams <*> (string ": " *> pType)
          ]

        fn :: Bool -> String -> TsType -> Partial (String, TsType)
        fn True  = (Required .) . (,)
        fn False = (Optional .) . (,)

        method :: String -> Bool -> Maybe (NonEmpty TsType) -> [Partial Param] -> TsType -> Partial (String, TsType)
        method n o g p r = (if o then Optional else Required) (n, TsTypeFunction $ Function g p r)

pDeclarationName :: Parser String
pDeclarationName = optional (string "export ") *> string "declare const " *> some alphaNumChar <* string ": "

pTypeArgs :: Parser (NonEmpty TsType)
pTypeArgs = between (char '<') (char '>') (NE.sepBy1 pTypeArg (string ", "))
  where pTypeArg :: Parser TsType
        pTypeArg = choice
          [ try $ TsTypeSubtype <$> pTypeMisc <* string " extends " <*> pType
          , pType
          ]

pParams :: Parser [Partial Param]
pParams = between (char '(') (char ')') $ sepBy pParam (string ", ")
  where pParam :: Parser (Partial Param)
        pParam = f <$> rest <*> (some alphaNumChar *> sep) <*> (optional (string "new ") *> pType)

        rest :: Parser Bool
        rest = isJust <$> optional (string "...")

        sep :: Parser Bool
        sep = True <$ string ": " <|> False <$ string "?: "

        f :: Bool -> Bool -> TsType -> Partial Param
        f True True   = Required . Rest
        f True False  = Optional . Rest
        f False True  = Required . Normal
        f False False = Optional . Normal

pReturn :: Parser TsType
pReturn = string " => " *> pType

pDeclaration :: Parser Declaration
pDeclaration = Declaration <$> pDeclarationName <*> pType <* optional (char ';') <* eof

pAlias :: Parser Alias
pAlias = Alias <$> (optional (string "export ") *> string "type " *> some alphaNumChar) <*> (optional pTypeArgs <* string " = ") <*> pType <* optional (char ';') <* eof

pInterface :: Parser Interface
pInterface = Interface <$> (optional (string "export ") *> string "interface " *> some alphaNumChar) <*> (optional pTypeArgs <* string " ") <*> pObject <*> optional (string " extends " *> pType) <* eof

pSignature :: Parser Signature
pSignature = SignatureAlias <$> pAlias <|> SignatureInterface <$> pInterface <|> SignatureDeclaration <$> pDeclaration

parseSignature :: String -> ParseOutput
parseSignature = parse pSignature "input"

type ParseOutput = Either (ParseErrorBundle String Void) Signature

