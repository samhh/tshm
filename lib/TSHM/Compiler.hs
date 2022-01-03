module TSHM.Compiler (compileDeclaration, CompileConfig (CompileConfig)) where

import           Control.Monad.RWS   (RWS, evalRWS)
import           Data.List.NonEmpty  ((!!))
import           Data.Map            (insert, lookup)
import qualified Data.Text           as T
import           Data.Tuple.Sequence (sequenceT)
import           Prelude
import           TSHM.TypeScript
import           Utils               (applyWhen, (<>^))

type Compiler a = RWS CompileConfig () CompileState a
type Compiler' = Compiler Text

surround :: Semigroup a => a -> a -> a -> a
surround l r x = l <> x <> r

parens :: Text -> Text
parens = surround "(" ")"

bracks :: Text -> Text
bracks = surround "[" "]"

dblqts :: Text -> Text
dblqts = surround "\"" "\""

backticks :: Text -> Text
backticks = surround "`" "`"

withParensWhen :: Bool -> Text -> Text
withParensWhen = flip applyWhen parens

withParensStd :: TExpr -> Text -> Text
withParensStd = withParensWhen . needsParens
  where needsParens :: TExpr -> Bool
        needsParens TGeneric {} = True
        needsParens TUnOp {}    = True
        needsParens TBinOp {}   = True
        needsParens TCond {}    = True
        needsParens TLambda {}  = True
        needsParens TInfer {}   = True
        needsParens _           = False

data CompileConfig = CompileConfig
  { signatures :: ReconciledAST
  , forall     :: Maybe Text
  , readonly   :: Bool
  }

data CompileState = CompileState
  { immediateFunctionArg :: Bool
  , namedFunctionArgs    :: Map Text Text
  , explicitTypeArgs     :: [TypeArg]
  , implicitTypeArgs     :: [TypeArg]
  , mappedTypeKeys       :: [Text]
  , inferredTypes        :: [Text]
  }

initialState :: CompileState
initialState = CompileState False mempty mempty mempty mempty mempty

compileDeclaration :: CompileConfig -> Text
compileDeclaration x = fst $ evalRWS declaration x initialState

data RenderedCompileState = RenderedCompileState
  { renderedTypeArgs :: Text
  , renderedSubtypes :: Text
  }

renderCompileState :: Compiler RenderedCompileState
renderCompileState = do
  tas <- implicitTypeArgs <$> get
  fam <- forall <$> ask

  targsm :: Maybe [Text] <- fmap (guarded (not . null)) . mapMaybeM tryCompileTypeArg $ tas
  let targsSig = foldMap (uncurry f) (sequenceT (fam, targsm))
        where f fa targs = fa <> " " <> unwords targs <> ". "

  sts :: Maybe [Text] <- fmap (guarded (not . null)) . mapM (uncurry subtype) . mapMaybe matchSubtype $ tas
  let stsSig = foldMap ((<> " => ") . T.intercalate ", ") sts

  pure $ RenderedCompileState targsSig stsSig

  where tryCompileTypeArg :: TypeArg -> Compiler (Maybe Text)
        tryCompileTypeArg (TMisc y, _)      = Just <$> misc y
        tryCompileTypeArg (TSubtype y _, _) = Just <$> misc y
        tryCompileTypeArg _                 = pure Nothing

        matchSubtype :: TypeArg -> Maybe (Text, TExpr)
        matchSubtype (TSubtype y z, _) = Just (y, z)
        matchSubtype _                 = Nothing

unscopedStatement :: UnscopedStatement -> Compiler'
unscopedStatement (UnscopedStatementImportDec x) = importDec x
unscopedStatement (UnscopedStatementMisc y)      = statement y

statement :: Statement -> Compiler'
statement (n, StatementAlias x)        = alias n x
statement (n, StatementInterface x)    = interface n x
statement (n, StatementEnum x)         = enum n x
statement (n, StatementConstDec x)     = constDec n x
statement (n, StatementFunctionDec xs) = T.intercalate "\n" <$> mapM (clean (lambdaDec n)) (toList xs)
  where clean :: (a -> Compiler') -> a -> Compiler'
        clean p x = do
          modify $ \s -> s { implicitTypeArgs = [] }
          p x

-- | Compile an entire "declaration", which is zero or more statements.
declaration :: Compiler'
declaration = fmap (T.intercalate "\n\n") . mapM unscopedStatement . signatures =<< ask

expr :: TExpr -> Compiler'
expr t = do
  res <- f t
  modify $ \s -> s { immediateFunctionArg = False }
  pure res
  where f TAny                   = pure "any"
        f TUnknown               = pure "unknown"
        f TNever                 = pure "never"
        f TVoid                  = pure "void"
        f TUndefined             = pure "undefined"
        f TNull                  = pure "null"
        f TUniqueSymbol          = pure "unique symbol"
        f (TBoolean True)        = pure "true"
        f (TBoolean False)       = pure "false"
        f (TMisc x)              = misc x
        f (TString x)            = pure . dblqts $ x
        f (TTemplate xs)         = backticks . T.concat <$> mapM template xs
        f (TNumber x)            = pure x
        f (TTuple xs)            = bracks . T.intercalate ", " <$> mapM expr xs
        f (TGeneric x ys)        = generic (x, ys)
        f (TSubtype x y)         = subtype x y
        f (TObject xs)           = object xs
        f (TIndexedAccess x y)   = indexed x y
        f (TDotAccess x y)       = expr x <>^ pure ("." <> y)
        f (TLambda x)            = lambda x
        f (TInfer x)             = infer x
        f (TUnOp x y)            = unOp x y
        f (TBinOp x y z)         = binOp x y z
        f (TCond l r tt ff)      = cond l r tt ff
        f (TGrouped x)           = parens <$> expr x

indexed :: TExpr -> TExpr -> Compiler'
indexed v k = (withParensStd v <$> expr v) <>^ (bracks <$> expr k)

param :: Param -> Compiler'
param (Param n xs) = case n of
  ParamDestructured -> f xs
  ParamNamed n'     -> do
    compiled <- f xs
    modify $ \s -> s { namedFunctionArgs = insert n' compiled (namedFunctionArgs s) }
    pure compiled
  where f (Required, Normal, x) = annotatedExpr x
        f (Required, Rest, x)   = pure "..." <>^ annotatedExpr x
        f (Optional, Normal, x) = annotatedExpr x <>^ pure "?"
        f (Optional, Rest, x)   = pure "..." <>^ annotatedExpr x <>^ pure "?"

        annotatedExpr :: TExpr -> Compiler'
        annotatedExpr x = do
          modify $ \s -> s { immediateFunctionArg = True }
          expr x

params :: [Param] -> Compiler'
params []  = pure "()"
params [x] = withParensWhen (needsParens . getParamExpr $ x) <$> param x
  where needsParens TLambda {} = True
        needsParens TBinOp {}  = True
        needsParens _          = False
params xs  = parens . T.intercalate ", " <$> mapM param xs

misc :: Text -> Compiler'
-- If the type is a single character, and we know about it from a type argument
-- or mapped type, then lowercase it
misc (T.uncons -> Just (x', T.uncons -> Nothing)) = do
  let x = T.singleton x'
  tas <- (implicitTypeArgs <>^ explicitTypeArgs) <$> get
  ys <- T.concat . (mappedTypeKeys <>^ inferredTypes) <$> get
  pure $ applyWhen (x `T.isInfixOf` ys || any (isViable . fst) tas) T.toLower x
  where isViable :: TExpr -> Bool
        isViable (TMisc (T.uncons -> Just (y, T.uncons -> Nothing)))      = y == x'
        isViable (TSubtype (T.uncons -> Just (y, T.uncons -> Nothing)) _) = y == x'
        isViable _                                                        = False
misc x   = pure x

subtype :: Text -> TExpr -> Compiler'
subtype x y = misc x <>^ pure " extends " <>^ (withParensStd y <$> expr y)

lambda :: Lambda -> Compiler'
lambda x = do
  let tas = foldMap toList (lambdaTypeArgs x)
  modify $ \s -> s { implicitTypeArgs = implicitTypeArgs s <> tas }

  params (lambdaParams x) <>^ pure " -> " <>^ expr (lambdaReturn x)

fnewtype :: Text -> TExpr -> Compiler'
fnewtype x y = pure ("newtype " <> x <> " = ") <>^ expr y

isNewtype :: TExpr -> Maybe TExpr
isNewtype (TGeneric (TMisc "Newtype") xs) = fmap snd . guarded (isNewtypeObject . fst) =<< isNewtypeTypeArgs xs
  where isNewtypeTypeArgs :: NonEmpty TypeArg -> Maybe (Object, TExpr)
        isNewtypeTypeArgs ys
          | length ys == 2 = (, fst $ ys !! 1) <$> isObject (head ys)
          | otherwise = Nothing

        isObject :: TypeArg -> Maybe Object
        isObject (TObject x, Nothing) = Just x
        isObject _                    = Nothing

        isNewtypeObject :: Object -> Bool
        isNewtypeObject (ObjectLit [ObjectPair Immut Required (_, TUniqueSymbol)]) = True
        isNewtypeObject _                                                          = False
isNewtype _ = Nothing

generic :: (TExpr, NonEmpty TypeArg) -> Compiler'
generic (x, ys) = expr x <>^ pure " " <>^ (unwords <$> mapM expr' (toList ys))
  where expr' :: TypeArg -> Compiler'
        expr' (z, _) = withParensStd z <$> expr z

objectKey :: ObjectKey -> Compiler'
objectKey (OKeyIdent x)    = pure x
objectKey (OKeyStr x)      = pure . dblqts $ x
objectKey (OKeyNum x)      = pure x
objectKey (OKeyIndex x)    = pure "[index: " <>^ expr x <>^ pure "]"
objectKey (OKeyComputed x) = pure "[" <>^ expr x <>^ pure "]"

objectPair :: ObjectPair -> Compiler'
objectPair (ObjectPair m p (kt, vt)) = do
  cfgRO <- readonly <$> ask
  let ro = memptyIfFalse (cfgRO && m == Immut) "readonly "
  let delim = applyWhen (p == Optional) ("?" <>) ": "

  pure ro <>^ objectKey kt <>^ pure delim <>^ expr vt

infer :: Text -> Compiler'
infer x = do
  modify $ \s -> s { inferredTypes = x : inferredTypes s }
  pure "infer " <>^ misc x

unOp :: UnOp -> TExpr -> Compiler'
unOp o t = do
  cfgRO <- readonly <$> ask
  raw <- expr t
  case o of
    UnOpReadonly   -> pure $ applyWhen cfgRO ("readonly " <>) raw
    UnOpKeys       -> pure $ "keyof " <> raw
    UnOpReflection -> do
      args <- namedFunctionArgs <$> get
      pure $ fromMaybe ("typeof " <> raw) (lookup raw args)

binOp :: BinOp -> TExpr -> TExpr -> Compiler'
binOp o l r = expr l <>^ pure (" " <> op o <> " ") <>^ expr r
  where op :: BinOp -> Text
        op BinOpIntersection = "&"
        op BinOpUnion        = "|"

template :: TemplateToken -> Compiler'
template (TemplateStr x)  = pure x
template (TemplateExpr x) = pure "${" <>^ expr x <>^ pure "}"

cond :: TExpr -> TExpr -> TExpr -> TExpr -> Compiler'
cond l r t f = l' <>^ pure " extends " <>^ r' <>^ pure " ? " <>^ expr t <>^ pure " : " <>^ expr f
    where l' = withParensStd l <$> expr l
          r' = withParensStd r <$> expr r

modMut :: ModMut -> Text
modMut AddMut = "readonly"
modMut RemMut = "-readonly"

modOpt :: ModOpt -> Text
modOpt AddOpt = "?:"
modOpt RemOpt = "-?:"

object :: Object -> Compiler'
object (ObjectLit []) = pure "{}"
object (ObjectLit xs) = pure "{ " <>^ items <>^ pure " }"
  where items = T.intercalate ", " <$> mapM objectPair xs
object (ObjectMapped m p (kt, xt, asm) vt) = do
  modify $ \s -> s { mappedTypeKeys = kt : mappedTypeKeys s }
  cfgRO <- readonly <$> ask
  let ro = foldMap ((<> " ") . modMut) . mfilter (const cfgRO) $ m
  let sep = maybe ":" modOpt p
  as <- foldMapM (fmap (" as " <>) . expr) asm

  k <- misc kt
  x <- expr xt
  v <- expr vt
  pure $ "{ " <> ro <> "[" <> k <> " in " <> x <> as <> "]" <> sep <> " " <> v <> " }"

importDec :: ImportDec -> Compiler'
importDec x = pure $ "import " <> dblqts (importDecFrom x) <> " " <> imp (importDecContents x)
  where imp :: Import -> Text
        imp (ImportDef d)            = named $ defToNamed d
        imp (ImportNamed ns)         = named ns
        imp (ImportAll a)            = allp a
        imp (ImportDefAndNamed d ns) = named $ defToNamed d <> ns
        imp (ImportDefAndAll d a)    = allp a <> " " <> named (defToNamed d)

        defToNamed :: Text -> NonEmpty Text
        defToNamed = pure . ("default as " <>)

        named :: NonEmpty Text -> Text
        named = parens . T.intercalate ", " . toList

        allp :: Text -> Text
        allp = ("as " <>)

constDec :: Text -> ConstDec -> Compiler'
constDec n (ConstDec t) = f <$> expr t <*> renderCompileState
  where f t' ps = n <> " :: " <> renderedTypeArgs ps <> renderedSubtypes ps <> t'

lambdaDec :: Text -> FunctionDec -> Compiler'
lambdaDec n (FunctionDec t) = f <$> lambda t <*> renderCompileState
  where f t' ps = n <> " :: " <> renderedTypeArgs ps <> renderedSubtypes ps <> t'

alias :: Text -> Alias -> Compiler'
alias n x = case isNewtype (aliasType x) of
  Just y -> fnewtype n y
  Nothing -> do
    let explicitTargs = foldMap toList (aliasTypeArgs x)
    modify $ \s -> s { explicitTypeArgs = explicitTypeArgs s <> explicitTargs }
    explicitTargsP <- unwords <$> mapMaybeM tryCompileTypeArg explicitTargs
    ttype <- expr (aliasType x)
    ps <- renderCompileState
    pure $ "type " <> n <> (if T.null explicitTargsP then "" else " ") <> explicitTargsP <> " = " <> renderedTypeArgs ps <> renderedSubtypes ps <> ttype
      where tryCompileTypeArg :: TypeArg -> Compiler (Maybe Text)
            tryCompileTypeArg (TMisc y, _)      = Just <$> misc y
            tryCompileTypeArg (TSubtype y z, _) = Just . parens <$> subtype y z
            tryCompileTypeArg _                   = pure Nothing

interface :: Text -> Interface -> Compiler'
interface n x = case isNewtype =<< interfaceExtends x of
  Just y  -> fnewtype n y
  Nothing -> alias n (fromInterface x)

enum :: Text -> SEnum -> Compiler'
enum n (SEnum xs) = f <$> mapM enumMember xs
  where f ys = "enum " <> n <> " {" <> (if null ys then "" else " ") <> T.intercalate ", " ys <> " }"

        enumMember :: EnumMember -> Compiler'
        enumMember (EnumMember k Nothing)  = pure $ enumKey k
        enumMember (EnumMember k (Just v)) = pure (enumKey k <> " = ") <>^ expr v

        enumKey :: EnumKey -> Text
        enumKey (EKeyIdent k) = k
        enumKey (EKeyStr k)   = dblqts k
