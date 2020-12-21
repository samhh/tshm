module TSHM.Printer (printSignature) where

import           Data.Char       (toLower)
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
fMisc [x] = pure . bool x (toLower x) . any (isTypeArg . fst) . ((<>) <$> implicitTypeArgs <*> explicitTypeArgs) <$> get
  where isTypeArg :: TsType -> Bool
        isTypeArg (TsTypeMisc [y])      = y == x
        isTypeArg (TsTypeSubtype [y] _) = y == x
        isTypeArg _                     = False
fMisc x   = pure x

fSubtype :: String -> TsType -> State PrintState String
fSubtype x y = surrounding " extends " <$> fMisc x <*> fTsType y

fFunction :: Function -> State PrintState String
fFunction x = do
  nested <- inParams <$> get
  let tas = maybe [] toList (functionTypeArgs x)
  modify $ \s -> s { implicitTypeArgs = implicitTypeArgs s <> tas }

  (doIf (surround "(" ")") nested .) . surrounding " -> " <$> fParams (functionParams x) <*> fTsType (functionReturn x)

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
fTsType TsTypeVoid                  = pure "void"
fTsType TsTypeUndefined             = pure "undefined"
fTsType TsTypeNull                  = pure "null"
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
fAlias x = do
  let explicitTargs = maybe [] toList (aliasTypeArgs x)
  modify $ \s -> s { explicitTypeArgs = explicitTypeArgs s <> explicitTargs }
  explicitTargsP <- intercalate " " <$> mapMaybeM printableTypeArg explicitTargs
  ttype <- fTsType (aliasType x)
  ps <- renderPrintState
  pure $ "type " <> aliasName x <> (if null explicitTargsP then "" else " ") <> explicitTargsP <> " = " <> renderedTypeArgs ps <> renderedSubtypes ps <> ttype
    where printableTypeArg :: (TsType, Maybe TsType) -> State PrintState (Maybe String)
          printableTypeArg (TsTypeMisc y, _)      = Just <$> fMisc y
          printableTypeArg (TsTypeSubtype y z, _) = Just . surround "(" ")" <$> fSubtype y z
          printableTypeArg _                   = pure Nothing

data RenderedPrintState = RenderedPrintState
  { renderedTypeArgs :: String
  , renderedSubtypes :: String
  }

renderPrintState :: State PrintState RenderedPrintState
renderPrintState = do
  tas <- implicitTypeArgs <$> get

  targs <- fmap (guarded (not . null)) . mapMaybeM printableTypeArg $ tas
  let targsSig = maybe "" (("forall " <>) . (<> ". ") . intercalate " ") targs

  sts <- fmap (guarded (not . null)) . mapM (uncurry fSubtype) . mapMaybe matchSubtype $ tas
  let stsSig = maybe "" ((<> " => ") . intercalate ", ") sts

  pure $ RenderedPrintState targsSig stsSig

  where printableTypeArg :: (TsType, Maybe TsType) -> State PrintState (Maybe String)
        printableTypeArg (TsTypeMisc y, _)      = Just <$> fMisc y
        printableTypeArg (TsTypeSubtype y _, _) = Just <$> fMisc y
        printableTypeArg _                   = pure Nothing

        matchSubtype :: (TsType, Maybe TsType) -> Maybe (String, TsType)
        matchSubtype (TsTypeSubtype y z, _) = Just (y, z)
        matchSubtype _                   = Nothing

fSignature :: Signature -> State PrintState String
fSignature (SignatureAlias y)               = fAlias y
fSignature (SignatureInterface y)           = fAlias $ fromInterface y
fSignature (SignatureConstDeclaration y)    = fConstDeclaration y
fSignature (SignatureFunctionDeclaration y) = fFunctionDeclaration y

data PrintState = PrintState
  { ambiguouslyNested :: Bool
  , inParams          :: Bool
  , explicitTypeArgs  :: [(TsType, Maybe TsType)]
  , implicitTypeArgs  :: [(TsType, Maybe TsType)]
  }

printSignature :: Signature -> String
printSignature x = evalState (fSignature x) (PrintState False False [] [])
