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
import qualified Data.Text                          as T
import           Data.Void                          ()
import           Prelude
import           TSHM.TypeScript
import           Text.Megaparsec                    hiding (many, some)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer         as L

type ParseOutput = Either (ParseErrorBundle Text Void) ParsedAST

parseDeclaration :: Text -> ParseOutput
parseDeclaration = parse declaration "input"

type Parser = Parsec Void Text

declaration :: Parser ParsedAST
declaration = scN *> many statement <* eof

statement :: Parser ScopedStatement
statement = choice
  [ ScopedStatementImportDec <$> importDec
  , try $ ScopedStatementExportDec <$> exportDec
  , try $ ScopedStatementMisc <$> scope <*> (fmap StatementAlias <$> alias)
  , try $ ScopedStatementMisc <$> scope <*> (fmap StatementInterface <$> interface)
  , try $ ScopedStatementMisc <$> scope <*> (fmap StatementConstDec <$> constDec)
  -- Overloaded function members don't have to be direct siblings, so we'll just
  -- parse them individually for now and reconcile later
  , try $ ScopedStatementMisc <$> scope <*> (fmap (StatementFunctionDec . pure) <$> fnDec)
  , ScopedStatementMisc <$> scope <*> (fmap StatementEnum <$> enum)
  ] <* scN <* optional (symN ";")

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
sym :: Text -> Parser Text
sym = L.symbol sc

-- An alternative symbol parser that parses newlines.
symN :: Text -> Parser Text
symN = L.symbol scN

-- This is literally just `p <* sc`, but it slightly formalises how we're
-- trying to formally approach whitespace.
lex :: Parser Text -> Parser Text
lex = L.lexeme sc

between' :: Text -> Text -> Parser a -> Parser a
between' l r = between (symN l) (scN *> sym r)

parens :: Parser a -> Parser a
parens = between' "(" ")"

braces :: Parser a -> Parser a
braces = between' "{" "}"

angles :: Parser a -> Parser a
angles = between' "<" ">"

bracks :: Parser a -> Parser a
bracks = between' "[" "]"

operators :: [[Operator Parser TExpr]]
operators =
  [ [ Postfix $ multi
      (   try (flip TIndexedAccess <$> bracks expr)
      <|> TGeneric "Array" . pure . (, Nothing) <$ symN "[" <* sym "]"
      <|> flip TDotAccess <$> (symN "." *> ident)
      )
    ]
  , [ Prefix $ TUnOp UnOpReflection <$ symN "typeof "
    , Prefix $ TUnOp UnOpKeys       <$ symN "keyof "
    , Prefix $ TUnOp UnOpReadonly   <$ symN "readonly "
    ]
  , [ InfixR $ TBinOp BinOpIntersection <$ symN "&"
    , InfixR $ TBinOp BinOpUnion        <$ symN "|"
    ]
  , [ TernR $ (\x -> (`TCond` x) <$ symN ":") <$> (symN "extends" *> expr <* symN "?")
    ]
  ]
    where multi :: Alternative f => f (a -> a) -> f (a -> a)
          multi f = foldr1 (flip (.)) <$> some f

expr :: Parser TExpr
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
ident :: Parser Text
ident = lex $ fmap T.pack . (:) <$> identHeadChar <*> (maybeToMonoid <$> optional (some identTailChar))

keyword :: Text -> Parser Text
keyword x = try . lex $ string x <* notFollowedBy identTailChar

scope :: Parser Scope
scope = toScope . isJust <$> optional (sym "export")
  where toScope True  = Exported
        toScope False = Local

ro :: Parser Mutant
ro = bool Mut Immut . isJust <$> optional (sym "readonly ")

roMod :: Parser (Maybe ModMut)
roMod = optional ((const RemMut <$ sym "-" <|> const AddMut <$ optional (sym "+")) <*> sym "readonly")

generic :: Parser TExpr
generic = TGeneric <$> ident <*> typeArgs

str :: Parser Text
str = lex $ strOf '\'' <|> strOf '"'
  where strOf :: Char -> Parser Text
        strOf c =
          let p = char c
           in T.pack <$> (p *> manyTill L.charLiteral p)

template :: Parser [TemplateToken]
template = char '`' *> go <* sym "`"
  where go :: Parser [TemplateToken]
        go = do
          s <- guard' . T.pack <$> manyTill L.charLiteral (lookAhead $ string "${" <|> string "`")
          isInterp <- True <$ lookAhead (string "${") <|> False <$ lookAhead (string "`")

          if isInterp
             then ((s <>) .) . (:) <$> interp <*> go
             else pure s

        interp :: Parser TemplateToken
        interp = TemplateExpr <$> (sym "${" *> expr <* char '}')

        guard' :: Text -> [TemplateToken]
        guard' = foldMap (pure . TemplateStr) . (guarded (not . T.null) :: Text -> Maybe Text)

num :: Parser Text
num = lex $ fmap T.pack . (<>) . foldMap pure <$> optional (char '-') <*> choice
  [ try $ (\x y z -> x <> pure y <> z) <$> some numberChar <*> char '.' <*> some numberChar
  , (:) <$> char '.' <*> some numberChar
  , some numberChar
  ]

tuple :: Parser [TExpr]
tuple = bracks $ sepEndBy expr (symN ",")

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

        pDefault :: Parser (Maybe TExpr)
        pDefault = optional $ sym "=" *> expr

params :: Parser [Param]
params = parens $ sepEndBy param (symN ",")
  where param :: Parser Param
        param = (\s n r v -> Param n (r, s, v)) <$> rest <*> name <*> sep <*> (optional (sym "new") *> expr)

        name :: Parser ParamName
        name =
              ParamNamed <$> ident
          <|> ParamDestructured <$ bracks (sepEndBy (rest *> name)                                (symN ","))
          <|> ParamDestructured <$ braces (sepEndBy (rest *> ident *> optional (sym ":" *> name)) (symN ","))

        rest :: Parser ParamScope
        rest = bool Normal Rest . isJust <$> optional (sym "...")

        sep :: Parser Partial
        sep = Required <$ sym ":" <|> Optional <$ sym "?:"

lambda :: Parser Lambda
lambda = Lambda <$> optional typeArgs <*> params <*> (sym "=>" *> expr)

importDec :: Parser ImportDec
importDec = flip ImportDec <$> (sym "import" *> optional (sym "type") *> imports) <*> (sym "from" *> str)
  where imports :: Parser Import
        imports = choice
          [ ImportNamed <$> named
          , ImportAll <$> allp
          , try $ ImportDefAndNamed <$> (def <* sym ",") <*> named
          , try $ ImportDefAndAll <$> (def <* sym ", ") <*> allp
          , ImportDef <$> def
          ]

        def :: Parser Text
        def = ident

        named :: Parser (NonEmpty Text)
        named = braces $ NE.sepBy1 ident (sym ",")

        allp :: Parser Text
        allp = sym "*" *> sym "as" *> ident

exportDec :: Parser ExportDec
exportDec =
      ExportDef <$> (symN "export default" *> ident)
  <|> ExportNamedRefs <$> try (symN "export" *> braces (sepEndBy (renamed <|> unchanged) (symN ",")))
  where unchanged = ExportNamedRefUnchanged <$> ident
        renamed = ExportNamedRefRenamed <$> try (ident <* sym "as") <*> ident

constDecIdent :: Parser Text
constDecIdent = sym "declare" *> sym "const" *> ident <* sym ":"

constDec :: Parser (Text, ConstDec)
constDec = (,) <$> constDecIdent <*> (ConstDec <$> expr)

fnDec :: Parser (Text, FunctionDec)
fnDec = (,)
  <$> (sym "declare" *> sym "function" *> ident)
  <*> (((FunctionDec .) .) . Lambda <$> optional typeArgs <*> params <* sym ":" <*> expr)

alias :: Parser (Text, Alias)
alias = (,)
  <$> (optional (sym "export") *> optional (sym "declare") *> sym "type" *> ident)
  <*> (Alias <$> (optional typeArgs <* sym "=") <*> expr)

interface :: Parser (Text, Interface)
interface = (,)
  <$> (optional (sym "export") *> sym "interface" *> ident)
  <*> (Interface <$> optional typeArgs <*> optional (try $ sym "extends" *> expr) <*> object)

enum :: Parser (Text, SEnum)
enum = (,)
  <$> (optional (sym "export") *> optional (sym "declare") *> optional (sym "const") *> sym "enum" *> ident)
  <*> (SEnum <$> braces (sepEndBy member (symN ",")))
  where member :: Parser EnumMember
        member = EnumMember <$> key <*> optional (sym "=" *> expr)

        key :: Parser EnumKey
        key = (EKeyStr <$> str) <|> (EKeyIdent <$> ident)
