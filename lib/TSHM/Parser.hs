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
          unaryPrefix x f = Prefix $ f <$ string x <* hspace1

          binaryInfix :: String -> (TsType -> TsType -> TsType) -> Operator Parser TsType
          binaryInfix x f = InfixR $ f <$ string (" " <> x <> " ")

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
  , TsTypeNever <$ pPreciseIdentifier "never"
  , TsTypeVoid <$ pPreciseIdentifier "void"
  , TsTypeNull <$ pPreciseIdentifier "null"
  , TsTypeUndefined <$ pPreciseIdentifier "undefined"
  , TsTypeUniqueSymbol <$ string "unique symbol"
  , TsTypeBoolean <$> ((True <$ pPreciseIdentifier "true") <|> (False <$ pPreciseIdentifier "false"))
  , TsTypeString <$> pString
  , TsTypeNumber <$> pNumber
  , TsTypeTuple <$> pTuple
  , TsTypeObject <$> pObject
  , TsTypeFunction <$> pFunction
  , try pGeneric
  , TsTypeMisc <$> pTypeMisc
  ]

pGeneric :: Parser TsType
pGeneric = TsTypeGeneric <$> pTypeMisc <*> pTypeArgs

pString :: Parser String
pString = stringLit '\'' <|> stringLit '"'
  where stringLit :: Char -> Parser String
        stringLit c =
          let p = char c
           in p *> manyTill L.charLiteral p

pNumber :: Parser String
pNumber = (<>) . foldMap pure <$> optional (char '-') <*> choice
  [ try $ (\x y z -> x <> pure y <> z) <$> some numberChar <*> char '.' <*> some numberChar
  , (:) <$> char '.' <*> some numberChar
  , some numberChar
  ]

pTuple :: Parser [TsType]
pTuple = between (char '[' <* space) (space *> char ']') $ sepEndBy pType (space *> char ',' <* space)

pObject :: Parser Object
pObject = between (char '{' <* space) (space *> char '}') pInner
  where pInner :: Parser Object
        pInner = choice
          [ mapped <$>
                (char '[' *> space *> pIdentifier)
            <*> (space1 *> string "in" *> space1 *> pType <* space <* char ']')
            <*> stdPairDelim
            <*> (pType <* optional (char ',' <|> char ';'))
          , ObjectLit <$> sepEndBy pPair litDelim
          ]

        pPair :: Parser ObjectPair
        pPair = optional (string "readonly" <* hspace1) *> choice
          [ try $ flip norm <$> pIdentifier <*> stdPairDelim <*> pType
          , method <$> pIdentifier <*> (isJust <$> optional (char '?')) <*> optional pTypeArgs <*> pParams <*> (char ':' *> hspace *> pType)
          ]

        litDelim :: Parser Char
        litDelim = (char ',' <|> char ';' <|> try (newline <* notFollowedBy (char '}'))) <* space

        stdPairDelim :: Parser Bool
        stdPairDelim = True <$ (char ':' <* hspace) <|> False <$ (string "?:" <* hspace)

        mapped :: String -> TsType -> Bool -> TsType -> Object
        mapped k x req v = let f = if req then Required else Optional
                            in ObjectMapped (f ((k, x), v))

        norm :: Bool -> String -> TsType -> ObjectPair
        norm True  = (Required .) . (,)
        norm False = (Optional .) . (,)

        method :: String -> Bool -> Maybe (NonEmpty TypeArgument) -> [Partial Param] -> TsType -> ObjectPair
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

pFunctionReturn :: Parser TsType
pFunctionReturn = hspace1 *> string "=>" *> space1 *> pType

pFunction :: Parser Function
pFunction = Function <$> optional pTypeArgs <*> pParams <*> pFunctionReturn

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

