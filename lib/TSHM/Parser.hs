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

getSc :: Parser () -> Parser ()
getSc x = L.space x (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- The standard space consumer to be used in most circumstances, supporting
-- comments. Note that it does not parse newlines.
sc :: Parser ()
sc = getSc hspace1

-- An alternative space consumer that parses newlines.
scN :: Parser ()
scN = getSc space1

-- This can be thought of as just `string x <* sc`.
sym :: String -> Parser String
sym = L.symbol sc

-- An alternative symbol parser that parses newlines.
symN :: String -> Parser String
symN = L.symbol scN

-- This is literally just `p <* sc`, but it slightly formalises how we're
-- trying to formally approach whitespace.
lex :: Parser String -> Parser String
lex = L.lexeme sc

between' :: String -> String -> Parser a -> Parser a
between' l r = between (symN l) (scN *> sym r)

parens :: Parser a -> Parser a
parens = between' "(" ")"

braces :: Parser a -> Parser a
braces = between' "{" "}"

angles :: Parser a -> Parser a
angles = between' "<" ">"

bracks :: Parser a -> Parser a
bracks = between' "[" "]"

operators :: [[Operator Parser Expr]]
operators =
  [ [ Postfix $ multi
      (   try (flip TIndexedAccess <$> bracks expr)
      <|> TGeneric "Array" . pure . (, Nothing) <$ symN "[" <* sym "]"
      )
    ]
  , [ Prefix $ TUnOp UnOpReflection <$ sym "typeof "
    , Prefix $ TUnOp UnOpKeys       <$ sym "keyof "
    , Prefix $ TUnOp UnOpReadonly   <$ sym "readonly "
    ]
  , [ InfixR $ TBinOp BinOpIntersection <$ sym "&"
    , InfixR $ TBinOp BinOpUnion        <$ sym "|"
    ]
  ]
    where multi :: Alternative f => f (a -> a) -> f (a -> a)
          multi f = foldr1 (flip (.)) <$> some f

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
expr = (`makeExprParser` operators) $ choice
  [ try $ TGrouped <$> parens expr
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

ro :: Parser Mutant
ro = bool Mut Immut . isJust <$> optional (sym "readonly ")

roMod :: Parser (Maybe ModMut)
roMod = optional ((const RemMut <$ sym "-" <|> const AddMut <$ optional (sym "+")) <*> sym "readonly")

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
tuple = bracks $ sepEndBy expr (sym ",")

object :: Parser Object
object = braces inner
  where inner :: Parser Object
        inner = choice
          [ try $ mapped <$>
                roMod
            <*> (sym "[" *> ident)
            <*> (sym "in" *> expr <* sym "]")
            <*> mappedDelim
            <*> (expr <* optional (sym "," <|> sym ";"))
          , ObjectLit <$> sepEndBy pair litDelim
          ]

        pair :: Parser ObjectPair
        pair = choice
          [ try $ norm <$> ro <*> ident <*> stdPairDelim <*> expr
          , method <$> ro <*> ident <*> opt <*> optional typeArgs <*> params <*> (sym ":" *> expr)
          ]
          where opt :: Parser Partial
                opt = bool Required Optional . isJust <$> optional (char '?')

        litDelim :: Parser ()
        litDelim =
              () <$ symN ","
          <|> () <$ symN ";"
          -- Consume any leading whitespace, then ensure there's at least one
          -- newline, then consume all trailing whitespace including any
          -- further newlines, then finally check the next character is the
          -- closing brace.
          <|> try (sc <* newline <* scN <* notFollowedBy (char '}'))

        stdPairDelim :: Parser Partial
        stdPairDelim = Required <$ sym ":" <|> Optional <$ sym "?:"

        mappedDelim :: Parser (Maybe ModOpt)
        mappedDelim = optional ((const RemOpt <$ sym "-" <|> const AddOpt <$ optional (sym "+")) <*> sym "?") <* sym ":"

        mapped :: Maybe ModMut -> String -> Expr -> Maybe ModOpt -> Expr -> Object
        mapped m k x req v = ObjectMapped m req (k, x) v

        norm :: Mutant -> String -> Partial -> Expr -> ObjectPair
        norm m s p e = ObjectPair m p (s, e)

        method :: Mutant -> String -> Partial -> Maybe (NonEmpty TypeArg) -> [Param] -> Expr -> ObjectPair
        method m n q g p r = ObjectPair m q (n, TLambda $ Lambda g p r)

typeArgs :: Parser (NonEmpty TypeArg)
typeArgs = angles (NE.sepEndBy1 typeArg (sym ","))
  where typeArg :: Parser TypeArg
        typeArg = (,)
          <$> choice
            [ try $ TSubtype <$> ident <* sym "extends" <*> expr
            , expr
            ]
          <*> pDefault

        pDefault :: Parser (Maybe Expr)
        pDefault = optional $ sym "=" *> expr

params :: Parser [Param]
params = parens $ sepEndBy param (symN ",")
  where param :: Parser Param
        param = (\x y z -> (y, x, z)) <$> rest <*> (name *> sep) <*> (optional (sym "new") *> expr)

        name :: Parser ()
        name =
              () <$ ident
          <|> () <$ bracks (sepEndBy (rest *> name)                                (symN ","))
          <|> () <$ braces (sepEndBy (rest *> ident *> optional (sym ":" *> name)) (symN ","))

        rest :: Parser ParamScope
        rest = bool Normal Rest . isJust <$> optional (sym "...")

        sep :: Parser Partial
        sep = Required <$ sym ":" <|> Optional <$ sym "?:"

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
constDec = ConstDec <$> constDecIdent <*> expr <* optional (sym ";")

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
  <*> expr <* optional (sym ";")

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

