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

fParam :: Partial Param -> Printer'
fParam = f
  where f (Required (Normal x)) = fAnnotatedTsType x
        f (Required (Rest x))   = ("..." <>) <$> fAnnotatedTsType x
        f (Optional (Normal x)) = (<> "?") <$> fAnnotatedTsType x
        f (Optional (Rest x))   = ("..." <>) . (<> "?") <$> fAnnotatedTsType x

        fAnnotatedTsType :: TsType -> Printer'
        fAnnotatedTsType x = do
          modify $ \s -> s { immediateFunctionArg = True }
          fTsType x

fParams :: [Partial Param] -> Printer'
fParams []  = pure "()"
fParams [x] = fParam x
fParams xs  = surround "(" ")" . intercalate ", " <$> mapM fParam xs

fMisc :: String -> Printer'
-- If the type is a single character, and we know about it from a type argument
-- somewhere, then lowercase it
fMisc [x] = trans x . any (isViable . fst) . ((<>) <$> implicitTypeArgs <*> explicitTypeArgs) <$> get
  where isViable :: TsType -> Bool
        isViable (TsTypeMisc [y])      = y == x
        isViable (TsTypeSubtype [y] _) = y == x
        isViable _                     = False

        trans :: Char -> Bool -> String
        trans c b = pure $ if b then toLower c else c
fMisc x   = pure x

fSubtype :: String -> TsType -> Printer'
fSubtype x y = surrounding " extends " <$> fMisc x <*> fAmbiguouslyNestedTsType y

fFunction :: Function -> Printer'
fFunction x = do
  nested <- uncurry (||) . (immediateFunctionArg &&& ambiguouslyNested) <$> get
  let tas = foldMap toList (functionTypeArgs x)
  modify $ \s -> s { implicitTypeArgs = implicitTypeArgs s <> tas }

  (doIf (surround "(" ")") nested .) . surrounding " -> " <$> fParams (functionParams x) <*> fTsType (functionReturn x)

fNewtype :: String -> TsType -> Printer'
fNewtype x y = (("newtype " <> x <> " = ") <>) <$> fTsType y

isNewtype :: TsType -> Maybe TsType
isNewtype (TsTypeGeneric "Newtype" xs) = fmap snd . guarded (isNewtypeObject . fst) =<< isNewtypeTypeArgs xs
  where isNewtypeTypeArgs :: NonEmpty TypeArgument -> Maybe (Object, TsType)
        isNewtypeTypeArgs ys
          | length ys == 2 = (, fst $ ys !! 1) <$> isObject (head ys)
          | otherwise = Nothing

        isObject :: TypeArgument -> Maybe Object
        isObject (TsTypeObject x, Nothing) = Just x
        isObject _                         = Nothing

        isNewtypeObject :: Object -> Bool
        isNewtypeObject (ObjectLit [Required (_, TsTypeUniqueSymbol)]) = True
        isNewtypeObject _                                              = False
isNewtype _ = Nothing

fGeneric :: (String, NonEmpty TypeArgument) -> Printer'
fGeneric (x, ys) = do
  nested <- ambiguouslyNested <$> get
  if nested then do
    modify $ \s -> s { ambiguouslyNested = False }
    surround "(" ")" <$> fGeneric (x, ys)
  else ((x <> " ") <>) . intercalate " " <$> mapM fTsType' (toList ys)
  where fTsType' :: TypeArgument -> Printer'
        fTsType' z = do
          modify $ \s -> s { ambiguouslyNested = True }
          res <- fTsType $ fst z
          modify $ \s -> s { ambiguouslyNested = False }
          pure res

fObjectPair :: ObjectPair -> Printer'
fObjectPair (Required (k, v)) = surrounding ": " k <$> fTsType v
fObjectPair (Optional (k, v)) = surrounding "?: " k <$> fTsType v

fUnOp :: UnOp -> TsType -> Printer'
fUnOp o t = do
  nested <- ambiguouslyNested <$> get
  doIf (surround "(" ")") nested . ((op o <> " ") <>) <$> fTsType t
  where op :: UnOp -> String
        op UnOpReflection = "typeof"
        op UnOpKeys       = "keyof"

fBinOp :: BinOp -> TsType -> TsType -> Printer'
fBinOp o l r = do
  nested <- ambiguouslyNested <$> get
  (doIf (surround "(" ")") nested .) . surrounding (" " <> op o <> " ")
    <$> fTsType l <*> fTsType r

  where op :: BinOp -> String
        op BinOpIntersection = "&"
        op BinOpUnion        = "|"

fTsType :: TsType -> Printer'
fTsType t = do
  res <- f t
  modify $ \s -> s { immediateFunctionArg = False }
  pure res
  where f TsTypeAny                   = pure "any"
        f TsTypeUnknown               = pure "unknown"
        f TsTypeNever                 = pure "never"
        f TsTypeVoid                  = pure "void"
        f TsTypeUndefined             = pure "undefined"
        f TsTypeNull                  = pure "null"
        f TsTypeUniqueSymbol          = pure "unique symbol"
        f (TsTypeBoolean x)           = pure $ if x then "true" else "false"
        f (TsTypeMisc x)              = fMisc x
        f (TsTypeString x)     = pure $ "\"" <> x <> "\""
        f (TsTypeNumber x)     = pure x
        f (TsTypeTuple xs)            = surround "[" "]" . intercalate ", " <$> mapM fTsType xs
        f (TsTypeGeneric x ys)        = fGeneric (x, ys)
        f (TsTypeSubtype x y)         = fSubtype x y
        f (TsTypeObject xs)           = fObject xs
        f (TsTypeIndexedAccess tv tk) = (\v k -> v <> "[" <> k <> "]") <$> fAmbiguouslyNestedTsType tv <*> fTsType tk
        f (TsTypeFunction x)          = fFunction x
        f (TsTypeUnOp x y)            = fUnOp x y
        f (TsTypeBinOp x y z)         = fBinOp x y z
        f (TsTypeGrouped x)           = do
          modify $ \s -> s { ambiguouslyNested = False }
          surround "(" ")" <$> fTsType x

fAmbiguouslyNestedTsType :: TsType -> Printer'
fAmbiguouslyNestedTsType t = do
  modify $ \s -> s { ambiguouslyNested = True }
  fTsType t

fObject :: Object -> Printer'
fObject (ObjectLit []) = pure "{}"
fObject (ObjectLit xs) = surround "{ " " }" . intercalate ", " <$> mapM fObjectPair xs
fObject (ObjectMapped (Required ((k, xt), vt))) = fMappedObject True k <$> fTsType xt <*> fTsType vt
fObject (ObjectMapped (Optional ((k, xt), vt))) = fMappedObject False k <$> fTsType xt <*> fTsType vt

fMappedObject :: Bool -> String -> String -> String -> String
fMappedObject req k x v = let delim = if req then "]: " else "]?: "
                           in "{ [" <> k <> " in " <> x <> delim <> v <> " }"

fConstDeclaration :: ConstDeclaration -> Printer'
fConstDeclaration x = (\t ps -> constDeclarationName x <> " :: " <> renderedTypeArgs ps <> renderedSubtypes ps <> t)
  <$> fTsType (constDeclarationType x) <*> renderPrintState

fFunctionDeclaration :: FunctionDeclaration -> Printer'
fFunctionDeclaration x = (\t ps -> functionDeclarationName x <> " :: " <> renderedTypeArgs ps <> renderedSubtypes ps <> t)
  <$> fFunction (functionDeclarationType x) <*> renderPrintState

fAlias :: Alias -> Printer'
fAlias x = case isNewtype (aliasType x) of
  Just y -> fNewtype (aliasName x) y
  Nothing -> do
    let explicitTargs = foldMap toList (aliasTypeArgs x)
    modify $ \s -> s { explicitTypeArgs = explicitTypeArgs s <> explicitTargs }
    explicitTargsP <- intercalate " " <$> mapMaybeM printableTypeArg explicitTargs
    ttype <- fTsType (aliasType x)
    ps <- renderPrintState
    pure $ "type " <> aliasName x <> (if null explicitTargsP then "" else " ") <> explicitTargsP <> " = " <> renderedTypeArgs ps <> renderedSubtypes ps <> ttype
      where printableTypeArg :: TypeArgument -> Printer (Maybe String)
            printableTypeArg (TsTypeMisc y, _)      = Just <$> fMisc y
            printableTypeArg (TsTypeSubtype y z, _) = Just . surround "(" ")" <$> fSubtype y z
            printableTypeArg _                   = pure Nothing

fInterface :: Interface -> Printer'
fInterface x = case isNewtype =<< interfaceExtends x of
  Just y  -> fNewtype (interfaceName x) y
  Nothing -> fAlias $ fromInterface x

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

  sts :: Maybe [String] <- fmap (guarded (not . null)) . mapM (uncurry fSubtype) . mapMaybe matchSubtype $ tas
  let stsSig = foldMap ((<> " => ") . intercalate ", ") sts

  pure $ RenderedPrintState targsSig stsSig

  where printableTypeArg :: TypeArgument -> Printer (Maybe String)
        printableTypeArg (TsTypeMisc y, _)      = Just <$> fMisc y
        printableTypeArg (TsTypeSubtype y _, _) = Just <$> fMisc y
        printableTypeArg _                      = pure Nothing

        matchSubtype :: TypeArgument -> Maybe (String, TsType)
        matchSubtype (TsTypeSubtype y z, _) = Just (y, z)
        matchSubtype _                      = Nothing

fSignature :: Printer'
fSignature = f . signature =<< ask
  where f (SignatureAlias x)                          = fAlias x
        f (SignatureInterface x)                      = fInterface x
        f (SignatureConstDeclaration x)               = fConstDeclaration x
        f (SignatureFunctionDeclaration xs)            = intercalate "\n" <$> mapM (clean fFunctionDeclaration) (toList xs)

        clean :: (a -> Printer') -> a -> Printer'
        clean p x = do
          modify $ \s -> s { implicitTypeArgs = [] }
          p x

data PrintState = PrintState
  { ambiguouslyNested    :: Bool
  , immediateFunctionArg :: Bool
  , explicitTypeArgs     :: [TypeArgument]
  , implicitTypeArgs     :: [TypeArgument]
  }

data PrintConfig = PrintConfig
  { signature :: Signature
  , forall    :: Maybe String
  }

printSignature :: PrintConfig -> String
printSignature x = fst $ evalRWS fSignature x (PrintState False False [] [])
