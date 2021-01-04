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
  where isNewtypeTypeArgs :: NonEmpty TypeArgument -> Maybe (ObjectLiteral, TsType)
        isNewtypeTypeArgs ys
          | length ys == 2 = (, fst $ ys !! 1) <$> isObjectLiteral (head ys)
          | otherwise = Nothing

        isObjectLiteral :: TypeArgument -> Maybe ObjectLiteral
        isObjectLiteral (TsTypeObject x, Nothing) = Just x
        isObjectLiteral _                         = Nothing

        isNewtypeObject :: ObjectLiteral -> Bool
        isNewtypeObject [Required (_, TsTypeUniqueSymbol)] = True
        isNewtypeObject _                                  = False
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

fObjectPair :: Partial (String, TsType) -> Printer'
fObjectPair (Required (k, v)) = ((k <> ": ") <>) <$> fTsType v
fObjectPair (Optional (k, v)) = ((k <> "?: ") <>) <$> fTsType v

fOperator :: TsOperator -> String
fOperator TsOperatorIntersection = "&"
fOperator TsOperatorUnion        = "|"

fExpression :: TsOperator -> TsType -> TsType -> Printer'
fExpression o l r = do
  nested <- ambiguouslyNested <$> get
  (doIf (surround "(" ")") nested .) . surrounding (" " <> fOperator o <> " ")
    <$> fTsType l <*> fTsType r

fTsType :: TsType -> Printer'
fTsType t = do
  res <- f t
  modify $ \s -> s { immediateFunctionArg = False }
  pure res
  where f TsTypeAny                   = pure "any"
        f TsTypeUnknown               = pure "unknown"
        f TsTypeVoid                  = pure "void"
        f TsTypeUndefined             = pure "undefined"
        f TsTypeNull                  = pure "null"
        f TsTypeUniqueSymbol          = pure "unique symbol"
        f (TsTypeBoolean x)           = pure $ if x then "true" else "false"
        f (TsTypeMisc x)              = fMisc x
        f (TsTypeStringLiteral x)     = pure $ "\"" <> x <> "\""
        f (TsTypeNumberLiteral x)     = pure x
        f (TsTypeTuple xs)            = surround "[" "]" . intercalate ", " <$> mapM fTsType xs
        f (TsTypeGeneric x ys)        = fGeneric (x, ys)
        f (TsTypeSubtype x y)         = fSubtype x y
        f (TsTypeKeysOf x)            = ("keyof " <>) <$> fTsType x
        f (TsTypeReflection x)        = pure $ "typeof " <> x
        f (TsTypeObject xs)           = fObject xs
        f (TsTypeObjectReference x k) = (<> ("[\"" <> k <> "\"]")) <$> fAmbiguouslyNestedTsType x
        f (TsTypeFunction x)          = fFunction x
        f (TsTypeExpression x y z)    = fExpression x y z
        f (TsTypeGrouped x)           = do
          modify $ \s -> s { ambiguouslyNested = False }
          surround "(" ")" <$> fTsType x

fAmbiguouslyNestedTsType :: TsType -> Printer'
fAmbiguouslyNestedTsType t = do
  modify $ \s -> s { ambiguouslyNested = True }
  fTsType t

fObject :: ObjectLiteral -> Printer'
fObject [] = pure "{}"
fObject xs = surround "{ " " }" . intercalate ", " <$> mapM fObjectPair xs

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
