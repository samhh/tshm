module TSHM.Printer (printSignature) where

import           Data.Char          (toLower)
import           Data.List.NonEmpty ((!!))
import           Prelude
import           TSHM.TypeScript

surround :: Semigroup a => a -> a -> a -> a
surround l r x = l <> x <> r

surrounding :: Semigroup a => a -> a -> a -> a
surrounding x l r = l <> x <> r

doIf :: (a -> a) -> Bool -> a -> a
doIf f True  = f
doIf _ False = id

fParam :: Partial Param -> State PrintState String
fParam (Required (Normal x)) = fTsType x
fParam (Required (Rest x))   = ("..." <>) <$> fTsType x
fParam (Optional (Normal x)) = (<> "?") <$> fTsType x
fParam (Optional (Rest x))   = ("..." <>) . (<> "?") <$> fTsType x

fParams :: [Partial Param] -> State PrintState String
fParams []  = pure "()"
fParams [x] = do
  modify $ \s -> s { inParams = True }
  y <- fParam x
  modify $ \s -> s { inParams = False }
  pure y
fParams xs  = do
  modify $ \s -> s { inParams = True }
  y <- surround "(" ")" . intercalate ", " <$> mapM fParam xs
  modify $ \s -> s { inParams = False }
  pure y

fMisc :: String -> State PrintState String
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

fSubtype :: String -> TsType -> State PrintState String
fSubtype x y = surrounding " extends " <$> fMisc x <*> fTsType y

fFunction :: Function -> State PrintState String
fFunction x = do
  nested <- inParams <$> get
  let tas = foldMap toList (functionTypeArgs x)
  modify $ \s -> s { implicitTypeArgs = implicitTypeArgs s <> tas }

  (doIf (surround "(" ")") nested .) . surrounding " -> " <$> fParams (functionParams x) <*> fTsType (functionReturn x)

fNewtype :: String -> TsType -> State PrintState String
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

fGeneric :: (String, NonEmpty (TsType, Maybe TsType)) -> State PrintState String
fGeneric (x, ys) = do
  nested <- ambiguouslyNested <$> get
  if nested then do
    modify $ \s -> s { ambiguouslyNested = False }
    surround "(" ")" <$> fGeneric (x, ys)
  else ((x <> " ") <>) . intercalate " " <$> mapM fTsType' (toList ys)
  where fTsType' :: (TsType, Maybe TsType) -> State PrintState String
        fTsType' z = do
          modify $ \s -> s { ambiguouslyNested = True }
          res <- fTsType $ fst z
          modify $ \s -> s { ambiguouslyNested = False }
          pure res

fObjectPair :: Partial (String, TsType) -> State PrintState String
fObjectPair (Required (k, v)) = ((k <> ": ") <>) <$> fTsType v
fObjectPair (Optional (k, v)) = ((k <> "?: ") <>) <$> fTsType v

fOperator :: TsOperator -> String
fOperator TsOperatorIntersection = "&"
fOperator TsOperatorUnion        = "|"

fExpression :: TsOperator -> TsType -> TsType -> State PrintState String
fExpression o l r = do
  nested <- ambiguouslyNested <$> get
  (doIf (surround "(" ")") nested .) . surrounding (" " <> fOperator o <> " ")
    <$> fTsType l <*> fTsType r

fTsType :: TsType -> State PrintState String
fTsType TsTypeAny                   = pure "any"
fTsType TsTypeUnknown               = pure "unknown"
fTsType TsTypeVoid                  = pure "void"
fTsType TsTypeUndefined             = pure "undefined"
fTsType TsTypeNull                  = pure "null"
fTsType TsTypeUniqueSymbol          = pure "unique symbol"
fTsType (TsTypeBoolean x)           = pure $ if x then "true" else "false"
fTsType (TsTypeMisc x)              = fMisc x
fTsType (TsTypeStringLiteral x)     = pure $ "\"" <> x <> "\""
fTsType (TsTypeNumberLiteral x)     = pure x
fTsType (TsTypeTuple xs)            = surround "[" "]" . intercalate ", " <$> mapM fTsType xs
fTsType (TsTypeGeneric x ys)        = fGeneric (x, ys)
fTsType (TsTypeSubtype x y)         = fSubtype x y
fTsType (TsTypeKeysOf x)            = ("keyof " <>) <$> fTsType x
fTsType (TsTypeReflection x)        = pure $ "typeof " <> x
fTsType (TsTypeObject xs)           = fObject xs
fTsType (TsTypeObjectReference x k) = (<> ("[\"" <> k <> "\"]")) <$> fTsType x
fTsType (TsTypeFunction x)          = fFunction x
fTsType (TsTypeExpression x y z)    = fExpression x y z
fTsType (TsTypeGrouped x)           = do
  modify $ \s -> s { ambiguouslyNested = False }
  surround "(" ")" <$> fTsType x

fObject :: ObjectLiteral -> State PrintState String
fObject [] = pure "{}"
fObject xs = surround "{ " " }" . intercalate ", " <$> mapM fObjectPair xs

fConstDeclaration :: ConstDeclaration -> State PrintState String
fConstDeclaration x = (\t ps -> constDeclarationName x <> " :: " <> renderedTypeArgs ps <> renderedSubtypes ps <> t)
  <$> fTsType (constDeclarationType x) <*> renderPrintState

fFunctionDeclaration :: FunctionDeclaration -> State PrintState String
fFunctionDeclaration x = (\t ps -> functionDeclarationName x <> " :: " <> renderedTypeArgs ps <> renderedSubtypes ps <> t)
  <$> fFunction (functionDeclarationType x) <*> renderPrintState

fAlias :: Alias -> State PrintState String
fAlias x = case isNewtype (aliasType x) of
  Just y -> fNewtype (aliasName x) y
  Nothing -> do
    let explicitTargs = foldMap toList (aliasTypeArgs x)
    modify $ \s -> s { explicitTypeArgs = explicitTypeArgs s <> explicitTargs }
    explicitTargsP <- intercalate " " <$> mapMaybeM printableTypeArg explicitTargs
    ttype <- fTsType (aliasType x)
    ps <- renderPrintState
    pure $ "type " <> aliasName x <> (if null explicitTargsP then "" else " ") <> explicitTargsP <> " = " <> renderedTypeArgs ps <> renderedSubtypes ps <> ttype
      where printableTypeArg :: (TsType, Maybe TsType) -> State PrintState (Maybe String)
            printableTypeArg (TsTypeMisc y, _)      = Just <$> fMisc y
            printableTypeArg (TsTypeSubtype y z, _) = Just . surround "(" ")" <$> fSubtype y z
            printableTypeArg _                   = pure Nothing

fInterface :: Interface -> State PrintState String
fInterface x = case isNewtype =<< interfaceExtends x of
  Just y -> fNewtype (interfaceName x) y
  Nothing -> fAlias $ fromInterface x

data RenderedPrintState = RenderedPrintState
  { renderedTypeArgs :: String
  , renderedSubtypes :: String
  }

renderPrintState :: State PrintState RenderedPrintState
renderPrintState = do
  tas <- implicitTypeArgs <$> get

  targs :: Maybe [String] <- fmap (guarded (not . null)) . mapMaybeM printableTypeArg $ tas
  let targsSig = foldMap (("forall " <>) . (<> ". ") . intercalate " ") targs

  sts :: Maybe [String] <- fmap (guarded (not . null)) . mapM (uncurry fSubtype) . mapMaybe matchSubtype $ tas
  let stsSig = foldMap ((<> " => ") . intercalate ", ") sts

  pure $ RenderedPrintState targsSig stsSig

  where printableTypeArg :: (TsType, Maybe TsType) -> State PrintState (Maybe String)
        printableTypeArg (TsTypeMisc y, _)      = Just <$> fMisc y
        printableTypeArg (TsTypeSubtype y _, _) = Just <$> fMisc y
        printableTypeArg _                      = pure Nothing

        matchSubtype :: (TsType, Maybe TsType) -> Maybe (String, TsType)
        matchSubtype (TsTypeSubtype y z, _) = Just (y, z)
        matchSubtype _                      = Nothing

fSignature :: Signature -> State PrintState String
fSignature (SignatureAlias x)               = fAlias x
fSignature (SignatureInterface x)           = fInterface x
fSignature (SignatureConstDeclaration x)    = fConstDeclaration x
fSignature (SignatureFunctionDeclaration x) = fFunctionDeclaration x

data PrintState = PrintState
  { ambiguouslyNested :: Bool
  , inParams          :: Bool
  , explicitTypeArgs  :: [(TsType, Maybe TsType)]
  , implicitTypeArgs  :: [(TsType, Maybe TsType)]
  }

printSignature :: Signature -> String
printSignature x = evalState (fSignature x) (PrintState False False [] [])
