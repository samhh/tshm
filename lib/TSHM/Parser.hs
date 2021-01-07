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
import           Text.Megaparsec                    hiding (many, some)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer         as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space hspace1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

sym :: String -> Parser String
sym = L.symbol sc

-- This is literally just `p <* sc`, but it slightly formalises how we're
-- trying to formally approach whitespace.
lex :: Parser String -> Parser String
lex = L.lexeme sc

-- Optionally parse any amount of interspersed newlines and whitespace. Use
-- this where whitespace is permitted and the usual space consumer is
-- insufficient.
nls :: Parser ()
nls = space

operators :: [[Operator Parser Expr]]
operators =
  [ [ Postfix (multi
      (   (TGeneric "Array" . pure . (, Nothing) <$ string "[]")
      <|> (flip TIndexedAccess <$> between (char '[') (char ']') expr)
      )
    )]
  , [ unaryPrefix "typeof" (TUnOp UnOpReflection)
    , unaryPrefix "keyof" (TUnOp UnOpKeys)
    ]
  , [ binaryInfix "&" (TBinOp BinOpIntersection)
    , binaryInfix "|" (TBinOp BinOpUnion)
    ]
  ]
    where multi :: Alternative f => f (a -> a) -> f (a -> a)
          multi f = foldr1 (flip (.)) <$> some f

          unaryPrefix :: String -> (Expr -> Expr) -> Operator Parser Expr
          unaryPrefix x f = Prefix $ f <$ sym x

          binaryInfix :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
          binaryInfix x f = InfixR $ f <$ sym x

identHeadChar :: Parser Char
identHeadChar = letterChar <|> char '$' <|> char '_'

identTailChar :: Parser Char
identTailChar = identHeadChar <|> numberChar

-- https://developer.mozilla.org/en-US/docs/Glossary/identifier
-- I can't find any documentation for this, but TypeScript appears to follow
-- the same rules for type-level identifiers as JavaScript does for runtime
-- identifiers.
ident :: Parser String
ident = lex $ (:) <$> identHeadChar <*> (maybeToMonoid <$> optional (some identTailChar))

keyword :: String -> Parser String
keyword x = try $ lex $ string x <* notFollowedBy identTailChar

expr :: Parser Expr
expr = (`makeExprParser` operators) $ optional (sym "readonly") *> choice
  [ try $ TGrouped <$> between (sym "(") (sym ")") expr
  , TAny <$ keyword "any"
  , TUnknown <$ keyword "unknown"
  , TNever <$ keyword "never"
  , TVoid <$ keyword "void"
  , TNull <$ keyword "null"
  , TUndefined <$ keyword "undefined"
  , TUniqueSymbol <$ sym "unique symbol"
  , TBoolean <$> ((True <$ keyword "true") <|> (False <$ keyword "false"))
  , TString <$> str
  , TNumber <$> num
  , TTuple <$> tuple
  , TObject <$> object
  , TLambda <$> lambda
  , try generic
  , TMisc <$> ident
  ]

generic :: Parser Expr
generic = TGeneric <$> ident <*> typeArgs

str :: Parser String
str = lex $ strOf '\'' <|> strOf '"'
  where strOf :: Char -> Parser String
        strOf c =
          let p = char c
           in p *> manyTill L.charLiteral p

num :: Parser String
num = lex $ (<>) . foldMap pure <$> optional (char '-') <*> choice
  [ try $ (\x y z -> x <> pure y <> z) <$> some numberChar <*> char '.' <*> some numberChar
  , (:) <$> char '.' <*> some numberChar
  , some numberChar
  ]

tuple :: Parser [Expr]
tuple = between (sym "[") (sym "]") $ sepEndBy expr (sym ",")

object :: Parser Object
object = between (sym "{" <* nls) (nls *> sym "}") pInner
  where pInner :: Parser Object
        pInner = choice
          [ mapped <$>
                (sym "[" *> ident)
            <*> (sym "in" *> expr <* sym "]")
            <*> stdPairDelim
            <*> (expr <* optional (sym "," <|> sym ";"))
          , ObjectLit <$> sepEndBy pair litDelim
          ]

        pair :: Parser ObjectPair
        pair = optional (sym "readonly") *> choice
          [ try $ flip norm <$> ident <*> stdPairDelim <*> expr
          , method <$> ident <*> (isJust <$> optional (char '?')) <*> optional typeArgs <*> params <*> (sym ":" *> expr)
          ]

        litDelim :: Parser String
        litDelim = lex $ pure <$> (char ',' <|> char ';' <|> try (newline <* notFollowedBy (char '}')))

        stdPairDelim :: Parser Bool
        stdPairDelim = True <$ sym ":" <|> False <$ sym "?:"

        mapped :: String -> Expr -> Bool -> Expr -> Object
        mapped k x req v = let f = if req then Required else Optional
                            in ObjectMapped (f ((k, x), v))

        norm :: Bool -> String -> Expr -> ObjectPair
        norm True  = (Required .) . (,)
        norm False = (Optional .) . (,)

        method :: String -> Bool -> Maybe (NonEmpty TypeArg) -> [Partial Param] -> Expr -> ObjectPair
        method n o g p r = (if o then Optional else Required) (n, TLambda $ Lambda g p r)

typeArgs :: Parser (NonEmpty TypeArg)
typeArgs = between (sym "<") (sym ">") (NE.sepEndBy1 typeArg (sym ","))
  where typeArg :: Parser TypeArg
        typeArg = (,)
          <$> choice
            [ try $ TSubtype <$> ident <* sym "extends" <*> expr
            , expr
            ]
          <*> pDefault

        pDefault :: Parser (Maybe Expr)
        pDefault = optional $ sym "=" *> expr

params :: Parser [Partial Param]
params = between (sym "(" <* nls) (nls *> sym ")") $ sepEndBy param (sym "," <* nls)
  where param :: Parser (Partial Param)
        param = f <$> rest <*> (ident *> sep) <*> (optional (sym "new") *> expr)

        rest :: Parser Bool
        rest = isJust <$> optional (string "...")

        sep :: Parser Bool
        sep = True <$ sym ":" <|> False <$ sym "?:"

        f :: Bool -> Bool -> Expr -> Partial Param
        f True True   = Required . Rest
        f True False  = Optional . Rest
        f False True  = Required . Normal
        f False False = Optional . Normal

lambda :: Parser Lambda
lambda = Lambda <$> optional typeArgs <*> params <*> (sym "=>" *> expr)

constDecIdent :: Parser String
constDecIdent =
     optional (sym "export")
  *> sym "declare"
  *> sym "const"
  *> ident
  <* sym ":"

constDec :: Parser ConstDec
constDec = ConstDec <$> constDecIdent <*> expr <* optional (char ';')

fnDecName :: Parser String
fnDecName =
      optional (sym "export")
   *> sym "declare"
   *> sym "function"
   *> ident

fnDec :: Parser FunctionDec
fnDec = FunctionDec
  <$> fnDecName
  <*> (Lambda <$> optional typeArgs <*> params <* sym ":" <*> expr)

alias :: Parser Alias
alias = Alias
  <$> (optional (sym "export") *> sym "type" *> ident)
  <*> (optional typeArgs <* sym "=")
  <*> expr <* optional (char ';')

interface :: Parser Interface
interface = Interface
  <$> (optional (sym "export") *> sym "interface" *> ident)
  <*> optional typeArgs
  <*> optional (try $ sym "extends" *> expr)
  <*> object

signature :: Parser Signature
signature = sc *> choice
  [ try $ SignatureAlias <$> alias
  , try $ SignatureInterface <$> interface
  , try $ SignatureConstDec <$> constDec
  , SignatureFunctionDec <$> NE.sepBy1 fnDec (some newline)
  ] <* eof

parseSignature :: String -> ParseOutput
parseSignature = parse signature "input"

type ParseOutput = Either (ParseErrorBundle String Void) Signature

