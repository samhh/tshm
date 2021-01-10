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

type ParseOutput = Either (ParseErrorBundle String Void) (NonEmpty Signature)

parseDeclaration :: String -> ParseOutput
parseDeclaration = parse declaration "input"

type Parser = Parsec Void String

declaration :: Parser (NonEmpty Signature)
declaration = scN *> NE.some signature <* scN <* eof

signature :: Parser Signature
signature = choice
  [ try $ SignatureAlias <$> alias
  , try $ SignatureInterface <$> interface
  , try $ SignatureConstDec <$> constDec
  , try $ SignatureFunctionDec <$> NE.sepBy1 fnDec (some newline)
  , SignatureEnum <$> enum
  ]

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
      <|> flip TDotAccess <$> (sym "." *> ident)
      )
    ]
  , [ Prefix $ TUnOp UnOpReflection <$ sym "typeof "
    , Prefix $ TUnOp UnOpKeys       <$ sym "keyof "
    , Prefix $ TUnOp UnOpReadonly   <$ sym "readonly "
    ]
  , [ InfixR $ TBinOp BinOpIntersection <$ sym "&"
    , InfixR $ TBinOp BinOpUnion        <$ sym "|"
    ]
  , [ TernR $ (\x -> (`TCond` x) <$ sym ":") <$> (sym "extends" *> expr <* sym "?")
    ]
  ]
    where multi :: Alternative f => f (a -> a) -> f (a -> a)
          multi f = foldr1 (flip (.)) <$> some f

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
  , TTemplate <$> template
  , TNumber <$> num
  , TTuple <$> tuple
  , TObject <$> object
  , TLambda <$> lambda
  , TInfer <$> (sym "infer" *> ident)
  , try generic
  , TMisc <$> ident
  ]

identHeadChar :: Parser Char
identHeadChar = letterChar <|> char '$' <|> char '_'

identTailChar :: Parser Char
identTailChar = identHeadChar <|> numberChar

-- https://developer.mozilla.org/en-US/docs/Glossary/identifier
-- I can't find any documentation for this, but TypeScript appears to follow
-- the same rules for type-level identifiers as JavaScript does for runtime
-- identifiers.
-- Note that JavaScript property names are technically a superset of ordinary
-- identifiers, behaving the same except also allowing reserved words. There's
-- currently no distinction in this module as we don't perform any manner of
-- correctness checking, making them for our purposes identical.
ident :: Parser String
ident = lex $ (:) <$> identHeadChar <*> (maybeToMonoid <$> optional (some identTailChar))

keyword :: String -> Parser String
keyword x = try $ lex $ string x <* notFollowedBy identTailChar

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

template :: Parser [TemplateToken]
template = char '`' *> go <* sym "`"
  where go :: Parser [TemplateToken]
        go = do
          s <- guard' <$> manyTill L.charLiteral (lookAhead $ string "${" <|> string "`")
          isInterp <- True <$ lookAhead (string "${") <|> False <$ lookAhead (string "`")

          if isInterp
             then ((s <>) .) . (:) <$> interp <*> go
             else pure s

        interp :: Parser TemplateToken
        interp = TemplateExpr <$> (sym "${" *> expr <* char '}')

        guard' :: String -> [TemplateToken]
        guard' = foldMap (pure . TemplateStr) . (guarded (not . null) :: String -> Maybe String)

num :: Parser String
num = lex $ (<>) . foldMap pure <$> optional (char '-') <*> choice
  [ try $ (\x y z -> x <> pure y <> z) <$> some numberChar <*> char '.' <*> some numberChar
  , (:) <$> char '.' <*> some numberChar
  , some numberChar
  ]

tuple :: Parser [Expr]
tuple = bracks $ sepEndBy expr (sym ",")

objLitProps :: Parser Object
objLitProps = ObjectLit <$> sepEndBy prop delim
  where key :: Parser ObjectKey
        key = choice
          [ OKeyIdent <$> ident
          , OKeyStr <$> str
          , OKeyNum <$> num
          , OKeyIndex <$> try (bracks (ident *> sym ":" *> expr))
          , OKeyComputed <$> bracks expr
          ]

        delim :: Parser ()
        delim =
              () <$ symN ","
          <|> () <$ symN ";"
          -- Consume any leading whitespace, then ensure there's at least one
          -- newline, then consume all trailing whitespace including any
          -- further newlines, then finally check the next character is the
          -- closing brace.
          <|> try (sc <* newline <* scN <* notFollowedBy (char '}'))

        prop :: Parser ObjectPair
        prop = choice
          [ try $ (\m s p e -> ObjectPair m p (s, e)) <$>
            ro <*> key <*> (opt <* sym ":") <*> expr
          , (\m n q g p r  -> ObjectPair m q (n, TLambda $ Lambda g p r)) <$>
            ro <*> key <*> opt <*> optional typeArgs <*> params <*> (sym ":" *> expr)
          ]
          where opt :: Parser Partial
                opt = bool Required Optional . isJust <$> optional (char '?')

objMappedProps :: Parser Object
objMappedProps = (\m k x a req v  -> ObjectMapped m req (k, x, a) v) <$>
      roMod
  <*> (sym "[" *> ident)
  <*> (sym "in" *> expr)
  <*> optional (sym "as" *> expr)
  <*> (sym "]" *> delim)
  <*> (expr <* optional (sym "," <|> sym ";"))
  where delim :: Parser (Maybe ModOpt)
        delim = optional ((const RemOpt <$ sym "-" <|> const AddOpt <$ optional (sym "+")) <*> sym "?") <* sym ":"

object :: Parser Object
object = braces $ try objMappedProps <|> objLitProps

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

enum :: Parser SEnum
enum = SEnum
  <$> (optional (sym "export") *> optional (sym "declare") *> optional (sym "const") *> sym "enum" *> ident)
  <*> braces (sepEndBy member (symN ","))
  where member :: Parser EnumMember
        member = EnumMember <$> key <*> optional (sym "=" *> expr)

        key :: Parser EnumKey
        key = (EKeyStr <$> str) <|> (EKeyIdent <$> ident)
