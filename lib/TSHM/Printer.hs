module TSHM.Printer (printDeclaration, PrintConfig (PrintConfig)) where

import           Control.Monad.RWS   (RWS, evalRWS)
import           Data.List.NonEmpty  ((!!))
import qualified Data.Text           as T
import           Data.Tuple.Sequence (sequenceT)
import           Prelude
import           TSHM.TypeScript

type Printer a = RWS PrintConfig () PrintState a
type Printer' = Printer Text

surround :: Semigroup a => a -> a -> a -> a
surround l r x = l <> x <> r

surrounding :: Semigroup a => a -> a -> a -> a
surrounding x l r = l <> x <> r

doIf :: (a -> a) -> Bool -> a -> a
doIf f True  = f
doIf _ False = id

data PrintConfig = PrintConfig
  { signatures :: NonEmpty Statement
  , forall     :: Maybe Text
  , readonly   :: Bool
  }

data PrintState = PrintState
  { ambiguouslyNested    :: Bool
  , immediateFunctionArg :: Bool
  , explicitTypeArgs     :: [TypeArg]
  , implicitTypeArgs     :: [TypeArg]
  , mappedTypeKeys       :: [Text]
  , inferredTypes        :: [Text]
  }

instance Semigroup PrintState where
  a <> b = PrintState
    (ambiguouslyNested a || ambiguouslyNested b)
    (immediateFunctionArg a || immediateFunctionArg b)
    (explicitTypeArgs a <> explicitTypeArgs b)
    (implicitTypeArgs a <> implicitTypeArgs b)
    (mappedTypeKeys a <> mappedTypeKeys b)
    (inferredTypes a <> inferredTypes b)

instance Monoid PrintState where
  mempty = PrintState False False mempty mempty mempty mempty

printDeclaration :: PrintConfig -> Text
printDeclaration x = fst $ evalRWS declaration x mempty

data RenderedPrintState = RenderedPrintState
  { renderedTypeArgs :: Text
  , renderedSubtypes :: Text
  }

renderPrintState :: Printer RenderedPrintState
renderPrintState = do
  tas <- implicitTypeArgs <$> get
  fam <- forall <$> ask

  targsm :: Maybe [Text] <- fmap (guarded (not . null)) . mapMaybeM printableTypeArg $ tas
  let targsSig = case sequenceT (fam, targsm) of
        Just (fa, ts) -> fa <> " " <> unwords ts <> ". "
        Nothing       -> ""

  sts :: Maybe [Text] <- fmap (guarded (not . null)) . mapM (uncurry subtype) . mapMaybe matchSubtype $ tas
  let stsSig = foldMap ((<> " => ") . T.intercalate ", ") sts

  pure $ RenderedPrintState targsSig stsSig

  where printableTypeArg :: TypeArg -> Printer (Maybe Text)
        printableTypeArg (TMisc y, _)      = Just <$> misc y
        printableTypeArg (TSubtype y _, _) = Just <$> misc y
        printableTypeArg _                 = pure Nothing

        matchSubtype :: TypeArg -> Maybe (Text, TExpr)
        matchSubtype (TSubtype y z, _) = Just (y, z)
        matchSubtype _                 = Nothing

statement :: Statement -> Printer'
statement (StatementImportDec x)    = importDec x
statement (StatementExportDec x)    = exportDec x
statement (StatementAlias x)        = alias x
statement (StatementInterface x)    = interface x
statement (StatementEnum x)         = enum x
statement (StatementConstDec x)     = constDec x
statement (StatementFunctionDec xs) = T.intercalate "\n" <$> mapM (clean lambdaDec) (toList xs)
  where clean :: (a -> Printer') -> a -> Printer'
        clean p x = do
          modify $ \s -> s { implicitTypeArgs = [] }
          p x

declaration :: Printer'
declaration = fmap (T.intercalate "\n\n" . toList) . mapM statement . signatures =<< ask

expr :: TExpr -> Printer'
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
        f (TBoolean x)           = pure $ if x then "true" else "false"
        f (TMisc x)              = misc x
        f (TString x)            = pure $ "\"" <> x <> "\""
        f (TTemplate xs)         = surround "`" "`" . T.concat <$> mapM template xs
        f (TNumber x)            = pure x
        f (TTuple xs)            = surround "[" "]" . T.intercalate ", " <$> mapM expr xs
        f (TGeneric x ys)        = generic (x, ys)
        f (TSubtype x y)         = subtype x y
        f (TObject xs)           = object xs
        f (TIndexedAccess tv tk) = (\v k -> v <> "[" <> k <> "]") <$> ambiguouslyNestedExpr tv <*> expr tk
        f (TDotAccess x y)       = (<> "." <> y) <$> expr x
        f (TLambda x)            = lambda x
        f (TInfer x)             = infer x
        f (TUnOp x y)            = unOp x y
        f (TBinOp x y z)         = binOp x y z
        f (TCond l r tt ff)      = cond l r tt ff
        f (TGrouped x)           = do
          modify $ \s -> s { ambiguouslyNested = False }
          surround "(" ")" <$> expr x

ambiguouslyNestedExpr :: TExpr -> Printer'
ambiguouslyNestedExpr t = do
  modify $ \s -> s { ambiguouslyNested = True }
  expr t

param :: Param -> Printer'
param = f
  where f (Required, Normal, x) = annotatedExpr x
        f (Required, Rest, x)   = ("..." <>) <$> annotatedExpr x
        f (Optional, Normal, x) = (<> "?") <$> annotatedExpr x
        f (Optional, Rest, x)   = ("..." <>) . (<> "?") <$> annotatedExpr x

        annotatedExpr :: TExpr -> Printer'
        annotatedExpr x = do
          modify $ \s -> s { immediateFunctionArg = True }
          expr x

params :: [Param] -> Printer'
params []  = pure "()"
params [x] = param x
params xs  = surround "(" ")" . T.intercalate ", " <$> mapM param xs

misc :: Text -> Printer'
-- If the type is a single character, and we know about it from a type argument
-- or mapped type, then lowercase it
misc (T.uncons -> Just (x', T.uncons -> Nothing)) = do
  let x = T.singleton x'
  tas <- ((<>) <$> implicitTypeArgs <*> explicitTypeArgs) <$> get
  ys <- (fmap T.concat . (<>) <$> mappedTypeKeys <*> inferredTypes) <$> get
  pure $ if x `T.isInfixOf` ys || any (isViable . fst) tas then T.toLower x else x
  where isViable :: TExpr -> Bool
        isViable (TMisc (T.uncons -> Just (y, T.uncons -> Nothing)))      = y == x'
        isViable (TSubtype (T.uncons -> Just (y, T.uncons -> Nothing)) _) = y == x'
        isViable _                                                        = False
misc x   = pure x

subtype :: Text -> TExpr -> Printer'
subtype x y = surrounding " extends " <$> misc x <*> ambiguouslyNestedExpr y

lambda :: Lambda -> Printer'
lambda x = do
  nested <- uncurry (||) . (immediateFunctionArg &&& ambiguouslyNested) <$> get
  let tas = foldMap toList (lambdaTypeArgs x)
  modify $ \s -> s { implicitTypeArgs = implicitTypeArgs s <> tas }

  (doIf (surround "(" ")") nested .) . surrounding " -> " <$> params (lambdaParams x) <*> expr (lambdaReturn x)

fnewtype :: Text -> TExpr -> Printer'
fnewtype x y = (("newtype " <> x <> " = ") <>) <$> expr y

isNewtype :: TExpr -> Maybe TExpr
isNewtype (TGeneric "Newtype" xs) = fmap snd . guarded (isNewtypeObject . fst) =<< isNewtypeTypeArgs xs
  where isNewtypeTypeArgs :: NonEmpty TypeArg -> Maybe (Object, TExpr)
        isNewtypeTypeArgs ys
          | length ys == 2 = (, fst $ ys !! 1) <$> isObject (head ys)
          | otherwise = Nothing

        isObject :: TypeArg -> Maybe Object
        isObject (TObject x, Nothing) = Just x
        isObject _                    = Nothing

        isNewtypeObject :: Object -> Bool
        isNewtypeObject (ObjectLit [ObjectPair Immut Required (_, TUniqueSymbol)]) = True
        isNewtypeObject _                                              = False
isNewtype _ = Nothing

generic :: (Text, NonEmpty TypeArg) -> Printer'
generic (x, ys) = do
  nested <- ambiguouslyNested <$> get
  if nested then do
    modify $ \s -> s { ambiguouslyNested = False }
    surround "(" ")" <$> generic (x, ys)
  else ((x <> " ") <>) . unwords <$> mapM expr' (toList ys)
  where expr' :: TypeArg -> Printer'
        expr' z = do
          modify $ \s -> s { ambiguouslyNested = True }
          res <- expr $ fst z
          modify $ \s -> s { ambiguouslyNested = False }
          pure res

objectKey :: ObjectKey -> Printer'
objectKey (OKeyIdent x)    = pure x
objectKey (OKeyStr x)      = pure $ "\"" <> x <> "\""
objectKey (OKeyNum x)      = pure x
objectKey (OKeyIndex x)    = surround "[index: " "]" <$> expr x
objectKey (OKeyComputed x) = surround "[" "]" <$> expr x

objectPair :: ObjectPair -> Printer'
objectPair (ObjectPair m p (kt, vt)) = do
  cfgRO <- readonly <$> ask
  let ro = if cfgRO && m == Immut then "readonly " else ""
  let delim = if p == Required then ": " else "?: "

  (\k v -> ro <> k <> delim <> v) <$> objectKey kt <*> expr vt

infer :: Text -> Printer'
infer x = do
  modify (\s -> s { inferredTypes = x : inferredTypes s })
  nested <- ambiguouslyNested <$> get
  doIf (surround "(" ")") nested . ("infer " <>) <$> misc x

unOp :: UnOp -> TExpr -> Printer'
unOp o t = do
  cfgRO <- readonly <$> ask
  if o == UnOpReadonly && not cfgRO then expr t else do
    nested <- ambiguouslyNested <$> get
    doIf (surround "(" ")") nested . ((op o <> " ") <>) <$> expr t
    where op :: UnOp -> Text
          op UnOpReflection = "typeof"
          op UnOpKeys       = "keyof"
          op UnOpReadonly   = "readonly"

binOp :: BinOp -> TExpr -> TExpr -> Printer'
binOp o l r = do
  nested <- ambiguouslyNested <$> get
  (doIf (surround "(" ")") nested .) . surrounding (" " <> op o <> " ")
    <$> expr l <*> expr r

  where op :: BinOp -> Text
        op BinOpIntersection = "&"
        op BinOpUnion        = "|"

template :: TemplateToken -> Printer'
template (TemplateStr x)  = pure x
template (TemplateExpr x) = surround "${" "}" <$> expr x

cond :: TExpr -> TExpr -> TExpr -> TExpr -> Printer'
cond lt rt tt ft = (\l r t f -> l <> " extends " <> r <> " ? " <> t <> " : " <> f) <$>
  expr lt <*> expr rt <*> expr tt <*> expr ft

modMut :: ModMut -> Text
modMut AddMut = "readonly"
modMut RemMut = "-readonly"

modOpt :: ModOpt -> Text
modOpt AddOpt = "?:"
modOpt RemOpt = "-?:"

object :: Object -> Printer'
object (ObjectLit []) = pure "{}"
object (ObjectLit xs) = surround "{ " " }" . T.intercalate ", " <$> mapM objectPair xs
object (ObjectMapped m p (kt, xt, asm) vt) = do
  modify $ \s -> s { mappedTypeKeys = kt : mappedTypeKeys s }
  cfgRO <- readonly <$> ask
  let ro = foldMap ((<> " ") . modMut) . mfilter (const cfgRO) $ m
  let sep = maybe ":" modOpt p
  as <- case asm of
    Nothing -> pure ""
    Just x  -> (" as " <>) <$> expr x

  k <- misc kt
  x <- expr xt
  v <- expr vt
  pure $ "{ " <> ro <> "[" <> k <> " in " <> x <> as <> "]" <> sep <> " " <> v <> " }"

importDec :: ImportDec -> Printer'
importDec x = pure $ "import \"" <> importDecFrom x <> "\" " <> imp (importDecContents x)
  where imp :: Import -> Text
        imp (ImportDef d)            = named $ defToNamed d
        imp (ImportNamed ns)         = named ns
        imp (ImportAll a)            = allp a
        imp (ImportDefAndNamed d ns) = named $ defToNamed d <> ns
        imp (ImportDefAndAll d a)    = allp a <> " " <> named (defToNamed d)

        defToNamed :: Text -> NonEmpty Text
        defToNamed = pure . ("default as " <>)

        named :: NonEmpty Text -> Text
        named = surround "(" ")" . T.intercalate ", " . toList

        allp :: Text -> Text
        allp = ("as " <>)

exportDec :: ExportDec -> Printer'
exportDec (ExportDef x) = ("default :: " <>) <$> expr x

constDec :: ConstDec -> Printer'
constDec x = (\t ps -> constDecName x <> " :: " <> renderedTypeArgs ps <> renderedSubtypes ps <> t)
  <$> expr (constDecType x) <*> renderPrintState

lambdaDec :: FunctionDec -> Printer'
lambdaDec x = (\t ps -> functionDecName x <> " :: " <> renderedTypeArgs ps <> renderedSubtypes ps <> t)
  <$> lambda (functionDecType x) <*> renderPrintState

alias :: Alias -> Printer'
alias x = case isNewtype (aliasType x) of
  Just y -> fnewtype (aliasName x) y
  Nothing -> do
    let explicitTargs = foldMap toList (aliasTypeArgs x)
    modify $ \s -> s { explicitTypeArgs = explicitTypeArgs s <> explicitTargs }
    explicitTargsP <- unwords <$> mapMaybeM printableTypeArg explicitTargs
    ttype <- expr (aliasType x)
    ps <- renderPrintState
    pure $ "type " <> aliasName x <> (if T.null explicitTargsP then "" else " ") <> explicitTargsP <> " = " <> renderedTypeArgs ps <> renderedSubtypes ps <> ttype
      where printableTypeArg :: TypeArg -> Printer (Maybe Text)
            printableTypeArg (TMisc y, _)      = Just <$> misc y
            printableTypeArg (TSubtype y z, _) = Just . surround "(" ")" <$> subtype y z
            printableTypeArg _                   = pure Nothing

interface :: Interface -> Printer'
interface x = case isNewtype =<< interfaceExtends x of
  Just y  -> fnewtype (interfaceName x) y
  Nothing -> alias $ fromInterface x

enum :: SEnum -> Printer'
enum x = (\ys -> "enum " <> enumName x <> " {" <> (if null ys then "" else " ") <> T.intercalate ", " ys <> " }") <$>
  mapM enumMember (enumMembers x)
  where enumMember :: EnumMember -> Printer'
        enumMember (EnumMember k Nothing)  = pure $ enumKey k
        enumMember (EnumMember k (Just v)) = ((enumKey k <> " = ") <>) <$> expr v

        enumKey :: EnumKey -> Text
        enumKey (EKeyIdent k) = k
        enumKey (EKeyStr k)   = "\"" <> k <> "\""
