module TSHM.Printer (printSignature, PrintConfig (PrintConfig)) where

import           Control.Monad.RWS   (RWS, evalRWS)
import           Data.Char           (toLower)
import           Data.List.NonEmpty  ((!!))
import           Data.Tuple.Sequence (sequenceT)
import           Prelude
import           TSHM.TypeScript

type Printer a = RWS PrintConfig () PrintState a
type Printer' = Printer String

surround :: Semigroup a => a -> a -> a -> a
surround l r x = l <> x <> r

surrounding :: Semigroup a => a -> a -> a -> a
surrounding x l r = l <> x <> r

doIf :: (a -> a) -> Bool -> a -> a
doIf f True  = f
doIf _ False = id

param :: Param -> Printer'
param = f
  where f (Required, Normal, x) = annotatedExpr x
        f (Required, Rest, x)   = ("..." <>) <$> annotatedExpr x
        f (Optional, Normal, x) = (<> "?") <$> annotatedExpr x
        f (Optional, Rest, x)   = ("..." <>) . (<> "?") <$> annotatedExpr x

        annotatedExpr :: Expr -> Printer'
        annotatedExpr x = do
          modify $ \s -> s { immediateFunctionArg = True }
          expr x

params :: [Param] -> Printer'
params []  = pure "()"
params [x] = param x
params xs  = surround "(" ")" . intercalate ", " <$> mapM param xs

misc :: String -> Printer'
-- If the type is a single character, and we know about it from a type argument
-- somewhere, then lowercase it
misc [x] = trans x . any (isViable . fst) . ((<>) <$> implicitTypeArgs <*> explicitTypeArgs) <$> get
  where isViable :: Expr -> Bool
        isViable (TMisc [y])      = y == x
        isViable (TSubtype [y] _) = y == x
        isViable _                = False

        trans :: Char -> Bool -> String
        trans c b = pure $ if b then toLower c else c
misc x   = pure x

subtype :: String -> Expr -> Printer'
subtype x y = surrounding " extends " <$> misc x <*> ambiguouslyNestedExpr y

lambda :: Lambda -> Printer'
lambda x = do
  nested <- uncurry (||) . (immediateFunctionArg &&& ambiguouslyNested) <$> get
  let tas = foldMap toList (lambdaTypeArgs x)
  modify $ \s -> s { implicitTypeArgs = implicitTypeArgs s <> tas }

  (doIf (surround "(" ")") nested .) . surrounding " -> " <$> params (lambdaParams x) <*> expr (lambdaReturn x)

fnewtype :: String -> Expr -> Printer'
fnewtype x y = (("newtype " <> x <> " = ") <>) <$> expr y

isNewtype :: Expr -> Maybe Expr
isNewtype (TGeneric "Newtype" xs) = fmap snd . guarded (isNewtypeObject . fst) =<< isNewtypeTypeArgs xs
  where isNewtypeTypeArgs :: NonEmpty TypeArg -> Maybe (Object, Expr)
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

generic :: (String, NonEmpty TypeArg) -> Printer'
generic (x, ys) = do
  nested <- ambiguouslyNested <$> get
  if nested then do
    modify $ \s -> s { ambiguouslyNested = False }
    surround "(" ")" <$> generic (x, ys)
  else ((x <> " ") <>) . intercalate " " <$> mapM expr' (toList ys)
  where expr' :: TypeArg -> Printer'
        expr' z = do
          modify $ \s -> s { ambiguouslyNested = True }
          res <- expr $ fst z
          modify $ \s -> s { ambiguouslyNested = False }
          pure res

objectPair :: ObjectPair -> Printer'
objectPair (ObjectPair m p (k, v)) = do
  cfgRO <- readonly <$> ask
  let ro = if cfgRO && m == Immut then "readonly " else ""
  let delim = if p == Required then ": " else "?: "

  surrounding delim (ro <> k) <$> expr v

unOp :: UnOp -> Expr -> Printer'
unOp o t = do
  cfgRO <- readonly <$> ask
  if o == UnOpReadonly && not cfgRO then expr t else do
    nested <- ambiguouslyNested <$> get
    doIf (surround "(" ")") nested . ((op o <> " ") <>) <$> expr t
    where op :: UnOp -> String
          op UnOpReflection = "typeof"
          op UnOpKeys       = "keyof"
          op UnOpReadonly   = "readonly"

binOp :: BinOp -> Expr -> Expr -> Printer'
binOp o l r = do
  nested <- ambiguouslyNested <$> get
  (doIf (surround "(" ")") nested .) . surrounding (" " <> op o <> " ")
    <$> expr l <*> expr r

  where op :: BinOp -> String
        op BinOpIntersection = "&"
        op BinOpUnion        = "|"

template :: TemplateToken -> Printer'
template (TemplateStr x)  = pure x
template (TemplateExpr x) = surround "${" "}" <$> expr x

expr :: Expr -> Printer'
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
        f (TTemplate xs)         = surround "`" "`" . intercalate "" <$> mapM template xs
        f (TNumber x)            = pure x
        f (TTuple xs)            = surround "[" "]" . intercalate ", " <$> mapM expr xs
        f (TGeneric x ys)        = generic (x, ys)
        f (TSubtype x y)         = subtype x y
        f (TObject xs)           = object xs
        f (TIndexedAccess tv tk) = (\v k -> v <> "[" <> k <> "]") <$> ambiguouslyNestedExpr tv <*> expr tk
        f (TDotAccess x y)       = (<> "." <> y) <$> expr x
        f (TLambda x)            = lambda x
        f (TUnOp x y)            = unOp x y
        f (TBinOp x y z)         = binOp x y z
        f (TGrouped x)           = do
          modify $ \s -> s { ambiguouslyNested = False }
          surround "(" ")" <$> expr x

ambiguouslyNestedExpr :: Expr -> Printer'
ambiguouslyNestedExpr t = do
  modify $ \s -> s { ambiguouslyNested = True }
  expr t

modMut :: ModMut -> String
modMut AddMut = "readonly"
modMut RemMut = "-readonly"

modOpt :: ModOpt -> String
modOpt AddOpt = "?:"
modOpt RemOpt = "-?:"

object :: Object -> Printer'
object (ObjectLit []) = pure "{}"
object (ObjectLit xs) = surround "{ " " }" . intercalate ", " <$> mapM objectPair xs
object (ObjectMapped m p (k, xt, asm) vt) = do
  cfgRO <- readonly <$> ask
  let ro = foldMap ((<> " ") . modMut) . mfilter (const cfgRO) $ m
  let sep = maybe ":" modOpt p
  as <- case asm of
    Nothing -> pure ""
    Just x  -> (" as " <>) <$> expr x

  x <- expr xt
  v <- expr vt
  pure $ "{ " <> ro <> "[" <> k <> " in " <> x <> as <> "]" <> sep <> " " <> v <> " }"

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
    explicitTargsP <- intercalate " " <$> mapMaybeM printableTypeArg explicitTargs
    ttype <- expr (aliasType x)
    ps <- renderPrintState
    pure $ "type " <> aliasName x <> (if null explicitTargsP then "" else " ") <> explicitTargsP <> " = " <> renderedTypeArgs ps <> renderedSubtypes ps <> ttype
      where printableTypeArg :: TypeArg -> Printer (Maybe String)
            printableTypeArg (TMisc y, _)      = Just <$> misc y
            printableTypeArg (TSubtype y z, _) = Just . surround "(" ")" <$> subtype y z
            printableTypeArg _                   = pure Nothing

interface :: Interface -> Printer'
interface x = case isNewtype =<< interfaceExtends x of
  Just y  -> fnewtype (interfaceName x) y
  Nothing -> alias $ fromInterface x

enum :: SEnum -> Printer'
enum x = (\ys -> "enum " <> enumName x <> " {" <> (if null ys then "" else " ") <> intercalate ", " ys <> " }") <$>
  mapM enumMember (enumMembers x)
  where enumMember :: EnumMember -> Printer'
        enumMember (EnumMember k Nothing)  = pure $ enumKey k
        enumMember (EnumMember k (Just v)) = ((enumKey k <> " = ") <>) <$> expr v

        enumKey :: EnumKey -> String
        enumKey (EKeyIdent k) = k
        enumKey (EKeyStr k)   = "\"" <> k <> "\""

data RenderedPrintState = RenderedPrintState
  { renderedTypeArgs :: String
  , renderedSubtypes :: String
  }

renderPrintState :: Printer RenderedPrintState
renderPrintState = do
  tas <- implicitTypeArgs <$> get
  fam <- forall <$> ask

  targsm :: Maybe [String] <- fmap (guarded (not . null)) . mapMaybeM printableTypeArg $ tas
  let targsSig = case sequenceT (fam, targsm) of
        Just (fa, ts) -> fa <> " " <> intercalate " " ts <> ". "
        Nothing       -> ""

  sts :: Maybe [String] <- fmap (guarded (not . null)) . mapM (uncurry subtype) . mapMaybe matchSubtype $ tas
  let stsSig = foldMap ((<> " => ") . intercalate ", ") sts

  pure $ RenderedPrintState targsSig stsSig

  where printableTypeArg :: TypeArg -> Printer (Maybe String)
        printableTypeArg (TMisc y, _)      = Just <$> misc y
        printableTypeArg (TSubtype y _, _) = Just <$> misc y
        printableTypeArg _                 = pure Nothing

        matchSubtype :: TypeArg -> Maybe (String, Expr)
        matchSubtype (TSubtype y z, _) = Just (y, z)
        matchSubtype _                 = Nothing

fsignature :: Printer'
fsignature = f . signature =<< ask
  where f (SignatureAlias x)        = alias x
        f (SignatureInterface x)    = interface x
        f (SignatureConstDec x)     = constDec x
        f (SignatureFunctionDec xs) = intercalate "\n" <$> mapM (clean lambdaDec) (toList xs)
        f (SignatureEnum x)         = enum x

        clean :: (a -> Printer') -> a -> Printer'
        clean p x = do
          modify $ \s -> s { implicitTypeArgs = [] }
          p x

data PrintState = PrintState
  { ambiguouslyNested    :: Bool
  , immediateFunctionArg :: Bool
  , explicitTypeArgs     :: [TypeArg]
  , implicitTypeArgs     :: [TypeArg]
  }

data PrintConfig = PrintConfig
  { signature :: Signature
  , forall    :: Maybe String
  , readonly  :: Bool
  }

printSignature :: PrintConfig -> String
printSignature x = fst $ evalRWS fsignature x (PrintState False False [] [])
