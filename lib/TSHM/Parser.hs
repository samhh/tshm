-- This module follows the following whitespace rules:
--   * Consume all whitespace after tokens where possible.
--   * Therefore, assume no whitespace before tokens.
--   * Always handle newlines explicitly, because they can potentially act as
--     delimiters in objects.

module TSHM.Parser where

import           Control.Monad.Combinators.Expr     (Operator (..),
                                                     makeExprParser)
import qualified Control.Monad.Combinators.NonEmpty as NE
import           Data.List                          (foldr1)
import           Data.Void                          ()
import           Prelude
import           TSHM.TypeScript
import           Text.Megaparsec                    hiding (some, many)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer         as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space hspace1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

symbol :: String -> Parser String
symbol = L.symbol sc

-- This is literally just `p <* sc`, but it slightly formalises how we're
-- trying to formally approach whitespace.
lexeme :: Parser String -> Parser String
lexeme = L.lexeme sc

-- Optionally parse any amount of interspersed newlines and whitespace. Use
-- this where whitespace is permitted and the usual space consumer is
-- insufficient.
nls :: Parser ()
nls = space

operators :: [[Operator Parser TsType]]
operators =
  [ [ Postfix (multi
      (   (TsTypeGeneric "Array" . pure . (, Nothing) <$ string "[]")
      <|> (flip TsTypeIndexedAccess <$> between (char '[') (char ']') pType)
      )
    )]
  , [ unaryPrefix "typeof" (TsTypeUnOp UnOpReflection)
    , unaryPrefix "keyof" (TsTypeUnOp UnOpKeys)
    ]
  , [ binaryInfix "&" (TsTypeBinOp BinOpIntersection)
    , binaryInfix "|" (TsTypeBinOp BinOpUnion)
    ]
  ]
    where multi :: Alternative f => f (a -> a) -> f (a -> a)
          multi f = foldr1 (flip (.)) <$> some f

          unaryPrefix :: String -> (TsType -> TsType) -> Operator Parser TsType
          unaryPrefix x f = Prefix $ f <$ symbol x

          binaryInfix :: String -> (TsType -> TsType -> TsType) -> Operator Parser TsType
          binaryInfix x f = InfixR $ f <$ symbol x

pIdentifierHeadChar :: Parser Char
pIdentifierHeadChar = letterChar <|> char '$' <|> char '_'

pIdentifierTailChar :: Parser Char
pIdentifierTailChar = pIdentifierHeadChar <|> numberChar

-- https://developer.mozilla.org/en-US/docs/Glossary/identifier
-- I can't find any documentation for this, but TypeScript appears to follow
-- the same rules for type-level identifiers as JavaScript does for runtime
-- identifiers.
pIdentifier :: Parser String
pIdentifier = lexeme $ (:) <$> pIdentifierHeadChar <*> (maybeToMonoid <$> optional (some pIdentifierTailChar))

pPreciseIdentifier :: String -> Parser String
pPreciseIdentifier x = try $ lexeme $ string x <* notFollowedBy pIdentifierTailChar

pType :: Parser TsType
pType = (`makeExprParser` operators) $ optional (symbol "readonly") *> choice
  [ try $ TsTypeGrouped <$> between (symbol "(") (symbol ")") pType
  , TsTypeAny <$ pPreciseIdentifier "any"
  , TsTypeUnknown <$ pPreciseIdentifier "unknown"
  , TsTypeNever <$ pPreciseIdentifier "never"
  , TsTypeVoid <$ pPreciseIdentifier "void"
  , TsTypeNull <$ pPreciseIdentifier "null"
  , TsTypeUndefined <$ pPreciseIdentifier "undefined"
  , TsTypeUniqueSymbol <$ symbol "unique symbol"
  , TsTypeBoolean <$> ((True <$ pPreciseIdentifier "true") <|> (False <$ pPreciseIdentifier "false"))
  , TsTypeString <$> pString
  , TsTypeNumber <$> pNumber
  , TsTypeTuple <$> pTuple
  , TsTypeObject <$> pObject
  , TsTypeFunction <$> pFunction
  , try pGeneric
  , TsTypeMisc <$> pIdentifier
  ]

pGeneric :: Parser TsType
pGeneric = TsTypeGeneric <$> pIdentifier <*> pTypeArgs

pString :: Parser String
pString = lexeme $ stringLit '\'' <|> stringLit '"'
  where stringLit :: Char -> Parser String
        stringLit c =
          let p = char c
           in p *> manyTill L.charLiteral p

pNumber :: Parser String
pNumber = lexeme $ (<>) . foldMap pure <$> optional (char '-') <*> choice
  [ try $ (\x y z -> x <> pure y <> z) <$> some numberChar <*> char '.' <*> some numberChar
  , (:) <$> char '.' <*> some numberChar
  , some numberChar
  ]

pTuple :: Parser [TsType]
pTuple = between (symbol "[") (symbol "]") $ sepEndBy pType (symbol ",")

pObject :: Parser Object
pObject = between (symbol "{" <* nls) (nls *> symbol "}") pInner
  where pInner :: Parser Object
        pInner = choice
          [ mapped <$>
                (symbol "[" *> pIdentifier)
            <*> (symbol "in" *> pType <* symbol "]")
            <*> stdPairDelim
            <*> (pType <* optional (symbol "," <|> symbol ";"))
          , ObjectLit <$> sepEndBy pPair litDelim
          ]

        pPair :: Parser ObjectPair
        pPair = optional (symbol "readonly") *> choice
          [ try $ flip norm <$> pIdentifier <*> stdPairDelim <*> pType
          , method <$> pIdentifier <*> (isJust <$> optional (char '?')) <*> optional pTypeArgs <*> pParams <*> (symbol ":" *> pType)
          ]

        litDelim :: Parser String
        litDelim = lexeme $ pure <$> (char ',' <|> char ';' <|> try (newline <* notFollowedBy (char '}')))

        stdPairDelim :: Parser Bool
        stdPairDelim = True <$ symbol ":" <|> False <$ symbol "?:"

        mapped :: String -> TsType -> Bool -> TsType -> Object
        mapped k x req v = let f = if req then Required else Optional
                            in ObjectMapped (f ((k, x), v))

        norm :: Bool -> String -> TsType -> ObjectPair
        norm True  = (Required .) . (,)
        norm False = (Optional .) . (,)

        method :: String -> Bool -> Maybe (NonEmpty TypeArgument) -> [Partial Param] -> TsType -> ObjectPair
        method n o g p r = (if o then Optional else Required) (n, TsTypeFunction $ Function g p r)

pTypeArgs :: Parser (NonEmpty TypeArgument)
pTypeArgs = between (symbol "<") (symbol ">") (NE.sepEndBy1 pTypeArg (symbol ","))
  where pTypeArg :: Parser TypeArgument
        pTypeArg = (,)
          <$> choice
            [ try $ TsTypeSubtype <$> pIdentifier <* symbol "extends" <*> pType
            , pType
            ]
          <*> pDefault

        pDefault :: Parser (Maybe TsType)
        pDefault = optional $ symbol "=" *> pType

pParams :: Parser [Partial Param]
pParams = between (symbol "(" <* nls) (nls *> symbol ")") $ sepEndBy pParam (symbol "," <* nls)
  where pParam :: Parser (Partial Param)
        pParam = f <$> rest <*> (pIdentifier *> sep) <*> (optional (symbol "new") *> pType)

        rest :: Parser Bool
        rest = isJust <$> optional (string "...")

        sep :: Parser Bool
        sep = True <$ symbol ":" <|> False <$ symbol "?:"

        f :: Bool -> Bool -> TsType -> Partial Param
        f True True   = Required . Rest
        f True False  = Optional . Rest
        f False True  = Required . Normal
        f False False = Optional . Normal

pFunctionReturn :: Parser TsType
pFunctionReturn = symbol "=>" *> pType

pFunction :: Parser Function
pFunction = Function <$> optional pTypeArgs <*> pParams <*> pFunctionReturn

pConstDeclarationName :: Parser String
pConstDeclarationName =
     optional (symbol "export")
  *> symbol "declare"
  *> symbol "const"
  *> pIdentifier
  <* symbol ":"

pConstDeclaration :: Parser ConstDeclaration
pConstDeclaration = ConstDeclaration <$> pConstDeclarationName <*> pType <* optional (char ';')

pFunctionDeclarationName :: Parser String
pFunctionDeclarationName =
      optional (symbol "export")
   *> symbol "declare"
   *> symbol "function"
   *> pIdentifier

pFunctionDeclaration :: Parser FunctionDeclaration
pFunctionDeclaration = FunctionDeclaration
  <$> pFunctionDeclarationName
  <*> (Function <$> optional pTypeArgs <*> pParams <* symbol ":" <*> pType)

pAlias :: Parser Alias
pAlias = Alias
  <$> (optional (symbol "export") *> symbol "type" *> pIdentifier)
  <*> (optional pTypeArgs <* symbol "=")
  <*> pType <* optional (char ';')

pInterface :: Parser Interface
pInterface = Interface
  <$> (optional (symbol "export") *> symbol "interface" *> pIdentifier)
  <*> optional pTypeArgs
  <*> optional (try $ symbol "extends" *> pType)
  <*> pObject

pSignature :: Parser Signature
pSignature = sc *> choice
  [ try $ SignatureAlias <$> pAlias
  , try $ SignatureInterface <$> pInterface
  , try $ SignatureConstDeclaration <$> pConstDeclaration
  , SignatureFunctionDeclaration <$> NE.sepBy1 pFunctionDeclaration (some newline)
  ] <* eof

parseSignature :: String -> ParseOutput
parseSignature = parse pSignature "input"

type ParseOutput = Either (ParseErrorBundle String Void) Signature

