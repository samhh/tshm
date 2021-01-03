module TSHM.Parser where

import           Control.Monad.Combinators.Expr     (Operator (..),
                                                     makeExprParser)
import qualified Control.Monad.Combinators.NonEmpty as NE
import           Data.List                          (foldr1)
import           Data.Void                          ()
import           Prelude
import           TSHM.TypeScript
import           Text.Megaparsec                    hiding (some)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer         as L

type Parser = Parsec Void String

operators :: [[Operator Parser TsType]]
operators =
  [ [ Postfix (multi
      (   (TsTypeGeneric "Array" . pure . (, Nothing) <$ string "[]")
      <|> (flip TsTypeObjectReference <$> between (char '[') (char ']') pStringLiteral)
      )
    )]
  , [ Prefix (TsTypeKeysOf <$ string "keyof" <* hspace1)
    ]
  , [ binary "&" (TsTypeExpression TsOperatorIntersection)
    , binary "|" (TsTypeExpression TsOperatorUnion)
    ]
  ]
    where multi :: Alternative f => f (a -> a) -> f (a -> a)
          multi f = foldr1 (flip (.)) <$> some f

          binary :: String -> (TsType -> TsType -> TsType) -> Operator Parser TsType
          binary x f = InfixR $ f <$ string (" " <> x <> " ")

pIdentifierHeadChar :: Parser Char
pIdentifierHeadChar = letterChar <|> char '$' <|> char '_'

pIdentifierTailChar :: Parser Char
pIdentifierTailChar = pIdentifierHeadChar <|> numberChar

-- https://developer.mozilla.org/en-US/docs/Glossary/identifier
-- I can't find any documentation for this, but TypeScript appears to follow
-- the same rules for type-level identifiers as JavaScript does for runtime
-- identifiers.
pIdentifier :: Parser String
pIdentifier = (:) <$> pIdentifierHeadChar <*> (maybeToMonoid <$> optional (some pIdentifierTailChar))

pPreciseIdentifier :: String -> Parser String
pPreciseIdentifier x = try $ string x <* notFollowedBy pIdentifierTailChar

pTypeMisc :: Parser String
pTypeMisc = some alphaNumChar

pType :: Parser TsType
pType = (`makeExprParser` operators) $ optional (string "readonly" <* hspace1) *> choice
  [ try $ TsTypeGrouped <$> between (char '(') (char ')') pType
  , TsTypeAny <$ pPreciseIdentifier "any"
  , TsTypeUnknown <$ pPreciseIdentifier "unknown"
  , TsTypeVoid <$ pPreciseIdentifier "void"
  , TsTypeNull <$ pPreciseIdentifier "null"
  , TsTypeUndefined <$ pPreciseIdentifier "undefined"
  , TsTypeUniqueSymbol <$ string "unique symbol"
  , TsTypeBoolean <$> ((True <$ pPreciseIdentifier "true") <|> (False <$ pPreciseIdentifier "false"))
  , TsTypeStringLiteral <$> pStringLiteral
  , TsTypeNumberLiteral <$> pNumberLiteral
  , TsTypeTuple <$> pTuple
  , TsTypeObject <$> pObject
  , TsTypeFunction <$> pFunctionLiteral
  , try pGeneric
  , TsTypeReflection <$> (string "typeof" *> hspace1 *> some alphaNumChar)
  , TsTypeMisc <$> pTypeMisc
  ]

pGeneric :: Parser TsType
pGeneric = TsTypeGeneric <$> pTypeMisc <*> pTypeArgs

pStringLiteral :: Parser String
pStringLiteral = stringLiteral '\'' <|> stringLiteral '"'
  where stringLiteral :: Char -> Parser String
        stringLiteral c =
          let p = char c
           in p *> manyTill L.charLiteral p

pNumberLiteral :: Parser String
pNumberLiteral = (<>) . foldMap pure <$> optional (char '-') <*> choice
  [ try $ (\x y z -> x <> pure y <> z) <$> some numberChar <*> char '.' <*> some numberChar
  , (:) <$> char '.' <*> some numberChar
  , some numberChar
  ]

pTuple :: Parser [TsType]
pTuple = between (char '[' <* space) (space *> char ']') $ sepEndBy pType (space *> char ',' <* space)

pObject :: Parser ObjectLiteral
pObject =
      [] <$ string "{}"
  <|> between (char '{' <* space) (space *> char '}') (sepEndBy1 pPair
        ((char ',' <|> char ';' <|> try (newline <* notFollowedBy (char '}'))) <* space))
  where pPair :: Parser (Partial (String, TsType))
        pPair = optional (string "readonly" <* hspace1) *> choice
          [ try $ flip fn <$> pIdentifier <*> (True <$ (char ':' <* hspace) <|> False <$ (string "?:" <* hspace)) <*> pType
          , method <$> pIdentifier <*> (isJust <$> optional (char '?')) <*> optional pTypeArgs <*> pParams <*> (char ':' *> hspace *> pType)
          ]

        fn :: Bool -> String -> TsType -> Partial (String, TsType)
        fn True  = (Required .) . (,)
        fn False = (Optional .) . (,)

        method :: String -> Bool -> Maybe (NonEmpty TypeArgument) -> [Partial Param] -> TsType -> Partial (String, TsType)
        method n o g p r = (if o then Optional else Required) (n, TsTypeFunction $ Function g p r)

pTypeArgs :: Parser (NonEmpty TypeArgument)
pTypeArgs = between (char '<') (char '>') (NE.sepEndBy1 pTypeArg (char ',' <* space))
  where pTypeArg :: Parser TypeArgument
        pTypeArg = (,)
          <$> choice
            [ try $ TsTypeSubtype <$> pTypeMisc <* hspace1 <* string "extends" <* hspace1 <*> pType
            , pType
            ]
          <*> pDefault

        pDefault :: Parser (Maybe TsType)
        pDefault = optional $ hspace1 *> char '=' *> hspace1 *> pType

pParams :: Parser [Partial Param]
pParams = between (char '(') (char ')') $ space *> sepEndBy pParam (char ',' <* space) <* space
  where pParam :: Parser (Partial Param)
        pParam = f <$> rest <*> (some alphaNumChar *> sep) <*> (optional (string "new" <* hspace1) *> pType)

        rest :: Parser Bool
        rest = isJust <$> optional (string "...")

        sep :: Parser Bool
        sep = True <$ (char ':' <* hspace) <|> False <$ (string "?:" <* hspace)

        f :: Bool -> Bool -> TsType -> Partial Param
        f True True   = Required . Rest
        f True False  = Optional . Rest
        f False True  = Required . Normal
        f False False = Optional . Normal

pFunctionLiteralReturn :: Parser TsType
pFunctionLiteralReturn = hspace1 *> string "=>" *> space1 *> pType

pFunctionLiteral :: Parser Function
pFunctionLiteral = Function <$> optional pTypeArgs <*> pParams <*> pFunctionLiteralReturn

pConstDeclarationName :: Parser String
pConstDeclarationName = optional (string "export" <* hspace1) *> (string "declare const" <* hspace1) *> pIdentifier <* char ':' <* hspace

pConstDeclaration :: Parser ConstDeclaration
pConstDeclaration = ConstDeclaration <$> pConstDeclarationName <*> pType <* optional (char ';')

pFunctionDeclarationName :: Parser String
pFunctionDeclarationName =
      optional (string "export" <* hspace1)
   *> (string "declare" <* hspace1)
   *> (string "function" <* hspace1)
   *> pIdentifier

pFunctionDeclaration :: Parser FunctionDeclaration
pFunctionDeclaration = FunctionDeclaration
  <$> pFunctionDeclarationName
  <*> (Function <$> optional pTypeArgs <*> pParams <* char ':' <* space1 <*> pType)

pAlias :: Parser Alias
pAlias = Alias
  <$> (optional (string "export" <* hspace1) *> (string "type" <* hspace1) *> some alphaNumChar)
  <*> (optional pTypeArgs <* (space1 *> char '=' <* space1))
  <*> pType <* optional (char ';')

pInterface :: Parser Interface
pInterface = Interface
  <$> (optional (string "export" <* hspace1) *> (string "interface" <* hspace1) *> some alphaNumChar)
  <*> optional pTypeArgs
  <*> optional (try $ (hspace1 *> string "extends" <* hspace1) *> pType)
  <*> (space1 *> pObject)

pSignature :: Parser Signature
pSignature = choice
  [ try $ SignatureAlias <$> pAlias
  , try $ SignatureInterface <$> pInterface
  , try $ SignatureConstDeclaration <$> pConstDeclaration
  , SignatureFunctionDeclaration <$> NE.sepBy1 pFunctionDeclaration newline
  ] <* eof

parseSignature :: String -> ParseOutput
parseSignature = parse pSignature "input"

type ParseOutput = Either (ParseErrorBundle String Void) Signature

