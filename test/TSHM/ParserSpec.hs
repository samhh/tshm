module TSHM.ParserSpec (spec) where

import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range
import           Prelude
import           TSHM.Parser
import           TSHM.TypeScript
import           Test.Hspec
import           Test.Hspec.Hedgehog   (PropertyT, forAll, hedgehog, (===))
import           Test.Hspec.Megaparsec
import           Text.Megaparsec       (ParseErrorBundle, Parsec, eof, parse)

unlines' :: [String] -> String
unlines' = intercalate "\n"

parse' :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse' = flip parse ""

(/=*) :: (Eq a, Show a) => (s -> Either e a) -> s -> PropertyT IO ()
f /=* a = first (const ()) (f a) === Left ()

(=*=) :: (Eq a, Eq e, Show a, Show e) => Either e a -> a -> PropertyT IO ()
a =*= b = a === Right b

typeArgs :: [TsType] -> NonEmpty TypeArgument
typeArgs = fmap (, Nothing) . fromList

spec :: Spec
spec = describe "TSHM.Parser" $ do
  describe "pType" $ do
    it "parses void" $ do
      parse' pType "void" `shouldParse` TsTypeVoid

    it "parses boolean" $ do
      parse' pType "true" `shouldParse` TsTypeBoolean True
      parse' pType "false" `shouldParse` TsTypeBoolean False

    it "parses primitive" $ do
      parse' pType "A" `shouldParse` TsTypeMisc "A"
      parse' pType "string" `shouldParse` TsTypeMisc "string"

    it "parses primitive with identifier starting with a known identifier" $ do
      parse' pType "void" `shouldParse` TsTypeVoid
      parse' pType "voidx" `shouldParse` TsTypeMisc "voidx"

    it "parses string literal" $ do
      parse' pType "'abc'" `shouldParse` TsTypeStringLiteral "abc"

    it "parses number literal" $ do
      parse' pType "-.123" `shouldParse` TsTypeNumberLiteral "-.123"

    it "parses special array syntax" $ do
      parse' pType "a[][]" `shouldParse`
        TsTypeGeneric "Array" (typeArgs [TsTypeGeneric "Array" $ typeArgs [TsTypeMisc "a"]])

    it "parses and discards readonly" $ do
      parse' pType "readonly [readonly A, { readonly x: readonly B }]" `shouldParse`
        TsTypeTuple [TsTypeMisc "A", TsTypeObject [Required ("x", TsTypeMisc "B")]]

    it "parses object reference" $ do
      parse' pType "A['key']" `shouldParse` TsTypeObjectReference (TsTypeMisc "A") "key"
      parse' pType "A[\"key\"]" `shouldParse` TsTypeObjectReference (TsTypeMisc "A") "key"
      parse' pType "true['k1']['k2']" `shouldParse`
        TsTypeObjectReference (TsTypeObjectReference (TsTypeBoolean True) "k1") "k2"

    it "postfix operators play nicely together" $ do
      parse' pType "a['k1']['k2'][][]['k3'][]" `shouldParse`
        TsTypeGeneric "Array" (typeArgs [TsTypeObjectReference (TsTypeGeneric "Array" (typeArgs [TsTypeGeneric "Array" (typeArgs [TsTypeObjectReference (TsTypeObjectReference (TsTypeMisc "a") "k1") "k2"])])) "k3"])

    it "parses function" $ do
      parse' pType "<A, B>(a: A) => B" `shouldParse`
         TsTypeFunction (Function (Just $ typeArgs [TsTypeMisc "A", TsTypeMisc "B"]) [Required $ Normal $ TsTypeMisc "A"] (TsTypeMisc "B"))

      parse' pType "(x: F<A, B>) => G<C, D>" `shouldParse`
         TsTypeFunction (Function Nothing [Required $ Normal $ TsTypeGeneric "F" $ typeArgs [TsTypeMisc "A", TsTypeMisc "B"]] (TsTypeGeneric "G" $ typeArgs [TsTypeMisc "C", TsTypeMisc "D"]))

    it "parses infix operators" $ do
      parse' pType "A & B | C" `shouldParse` TsTypeExpression TsOperatorIntersection (TsTypeMisc "A") (TsTypeExpression TsOperatorUnion (TsTypeMisc "B") (TsTypeMisc "C"))

    it "parses keyof" $ do
      parse' pType "keyof A<B>" `shouldParse` TsTypeKeysOf (TsTypeGeneric "A" $ typeArgs [TsTypeMisc "B"])

    it "parses typeof" $ do
      parse' pType "typeof x & y" `shouldParse` TsTypeExpression TsOperatorIntersection (TsTypeReflection "x") (TsTypeMisc "y")

    it "parses parentheses and changes precedence accordingly" $ do
      parse' pType "(A & B) | C" `shouldParse` TsTypeExpression TsOperatorUnion (TsTypeGrouped $ TsTypeExpression TsOperatorIntersection (TsTypeMisc "A") (TsTypeMisc "B")) (TsTypeMisc "C")

  describe "pFunctionLiteral" $ do
    it "parses minimal viable function" $ do
      parse' pFunctionLiteral "() => void" `shouldParse` Function Nothing [] TsTypeVoid

    it "parses type arguments" $ do
      parse' pFunctionLiteral "<A, Either<void, B>>() => void" `shouldParse`
        Function (Just $ typeArgs [TsTypeMisc "A", TsTypeGeneric "Either" $ typeArgs [TsTypeVoid, TsTypeMisc "B"]]) [] TsTypeVoid

    it "parses params" $ do
      parse' pFunctionLiteral "(x: number, y: string) => void" `shouldParse`
        Function Nothing [Required $ Normal $ TsTypeMisc "number", Required $ Normal $ TsTypeMisc "string"] TsTypeVoid

    it "parses return type" $ do
      parse' pFunctionLiteral "() => number" `shouldParse` Function Nothing [] (TsTypeMisc "number")

    it "parses curried function" $ do
      parse' pFunctionLiteral "() => () => void" `shouldParse` Function Nothing [] (TsTypeFunction (Function Nothing [] TsTypeVoid))

    it "supports extends clause in function generics" $ do
      parse' pFunctionLiteral "<B, A extends Array<B>>(f: <C extends number, D>(x: 'ciao') => string) => void" `shouldParse`
        Function
          ( Just $ typeArgs
            [ TsTypeMisc "B"
            , TsTypeSubtype "A" (TsTypeGeneric "Array" $ typeArgs [TsTypeMisc "B"])
            ]
          )
          [ Required $ Normal $ TsTypeFunction (Function
              (Just $ typeArgs [TsTypeSubtype "C" (TsTypeMisc "number"), TsTypeMisc "D"])
              [Required $ Normal $ TsTypeStringLiteral "ciao"]
              (TsTypeMisc "string"))
          ]
          TsTypeVoid

    it "parses complex function" $ do
      parse' pFunctionLiteral "<A, Array<Option<B>>>(x: number, y: <C>(z: C) => () => C) => () => void" `shouldParse`
        Function
          ( Just $ typeArgs
            [ TsTypeMisc "A"
            , TsTypeGeneric "Array" $ typeArgs
              [ TsTypeGeneric "Option" $ typeArgs
                [ TsTypeMisc "B"
                ]
              ]
            ]
          )
          [ Required $ Normal $ TsTypeMisc "number"
          , Required $ Normal $ TsTypeFunction (Function
              (Just $ typeArgs [TsTypeMisc "C"])
              [Required $ Normal $ TsTypeMisc "C"]
              (TsTypeFunction (Function Nothing [] (TsTypeMisc "C"))))
          ]
          ( TsTypeFunction (Function Nothing [] TsTypeVoid))

  describe "pObject" $ do
    it "parses empty object" $ do
      parse' pObject "{}" `shouldParse` []

    it "parses non-empty flat object" $ do
      parse' pObject "{ a: 1, b: 'two' }" `shouldParse` [Required ("a", TsTypeNumberLiteral "1"), Required ("b", TsTypeStringLiteral "two")]

    it "parses non-empty nested object" $ do
      parse' pObject "{ a: 1, b: { c: true }[] }" `shouldParse`
        [Required ("a", TsTypeNumberLiteral "1"), Required ("b", TsTypeGeneric "Array" $ typeArgs [TsTypeObject [Required ("c", TsTypeBoolean True)]])]

    it "supports mixed comma and semi-colon delimiters" $ do
      parse' pObject "{ a: number, b: string; c: boolean }" `shouldParse`
        [Required ("a", TsTypeMisc "number"), Required ("b", TsTypeMisc "string"), Required ("c", TsTypeMisc "boolean")]

    it "parses optional and required properties" $ do
      parse' pObject "{ a: A, b?: B, c: C }" `shouldParse`
        [Required ("a", TsTypeMisc "A"), Optional ("b", TsTypeMisc "B"), Required ("c", TsTypeMisc "C")]

    it "parses alternative method syntax" $ do
      parse' pObject "{ f?<A>(): void }" `shouldParse`
        [Optional ("f", TsTypeFunction $ Function (Just $ typeArgs [TsTypeMisc "A"]) [] TsTypeVoid)]

    it "parses trailing delimiter in non-empty object" $ do
      parse' pObject "{ a: number; b: string, }" `shouldParse`
        [Required ("a", TsTypeMisc "number"), Required ("b", TsTypeMisc "string")]

  describe "pTuple" $ do
    it "parses empty tuple" $ do
      parse' pTuple "[]" `shouldParse` []

    it "parses non-empty flat tuple" $ do
      parse' pTuple "[a, 'b']" `shouldParse` [TsTypeMisc "a", TsTypeStringLiteral "b"]

    it "parses non-empty nested tuple" $ do
      parse' pTuple "[a, ['b', 3]]" `shouldParse` [TsTypeMisc "a", TsTypeTuple [TsTypeStringLiteral "b", TsTypeNumberLiteral "3"]]

    it "parses non-empty tuple with trailing comma" $ do
      parse' pTuple "[a,]" `shouldParse` [TsTypeMisc "a"]
      parse' pTuple "[a, ]" `shouldParse` [TsTypeMisc "a"]
      parse' pTuple "[a, b,]" `shouldParse` [TsTypeMisc "a", TsTypeMisc "b"]
      parse' pTuple "[a, b, ]" `shouldParse` [TsTypeMisc "a", TsTypeMisc "b"]

  describe "pNumberLiteral" $ do
    let p = pNumberLiteral <* eof

    it "parses int" $ do
      parse' p "123" `shouldParse` "123"

    it "parses negative int" $ do
      parse' p "-123" `shouldParse` "-123"

    it "parses float" $ do
      parse' p ".123" `shouldParse` ".123"
      parse' p "123.456" `shouldParse` "123.456"

    it "parses negative float" $ do
      parse' p "-.123" `shouldParse` "-.123"
      parse' p "-123.456" `shouldParse` "-123.456"

    it "does not permit hyphen anywhere except start" $ do
      parse' p `shouldFailOn` "1-"
      parse' p `shouldFailOn` "1.2-"

    it "does not permit more than one period" $ do
      parse' p `shouldFailOn` ".1.2"
      parse' p `shouldFailOn` "1.2.3"

  describe "pConstDeclarationName" $ do
    let ident = Gen.list (Range.linear 1 99) Gen.alpha

    it "parses const declaration" $ hedgehog $ do
      x <- forAll ident
      parse' pConstDeclarationName ("declare const " <> x <> ": ") =*= x

    it "parses exported const declaration" $ hedgehog $ do
      x <- forAll ident
      parse' pConstDeclarationName ("export declare const " <> x <> ": ") =*= x

    it "requires declaration" $ hedgehog $ do
      x <- forAll ident
      parse' pConstDeclarationName /=* ("const " <> x <> ": ")

  describe "pTypeArgs" $ do
    it "parses single flat type argument" $ do
      parse' pTypeArgs "<A>" `shouldParse`
        typeArgs [ TsTypeMisc "A"
        ]

    it "parses multiple flat type arguments" $ do
      parse' pTypeArgs "<A, B, C>" `shouldParse`
        typeArgs [ TsTypeMisc "A"
        , TsTypeMisc "B"
        , TsTypeMisc "C"
        ]

    it "parses single nested type argument" $ do
      parse' pTypeArgs "<Array<Option<A>>>" `shouldParse`
        typeArgs [ TsTypeGeneric "Array" $ typeArgs
          [ TsTypeGeneric "Option" $ typeArgs
            [ TsTypeMisc "A"
            ]
          ]
        ]

    it "parses mixed type arguments" $ do
      parse' pTypeArgs "<A, Array<Option<B>, C>, Either<E, A>>" `shouldParse`
        typeArgs
          [ TsTypeMisc "A"
          , TsTypeGeneric "Array" $ typeArgs
            [ TsTypeGeneric "Option" $ typeArgs
              [ TsTypeMisc "B"
              ]
            , TsTypeMisc "C"
            ]
          , TsTypeGeneric "Either" $ typeArgs
            [ TsTypeMisc "E"
            , TsTypeMisc "A"
            ]
          ]

    it "parses default type arguments" $ do
      parse' pTypeArgs "<A, B = C, D extends E = F<G>>" `shouldParse`
        fromList
          [ (TsTypeMisc "A", Nothing)
          , (TsTypeMisc "B", Just $ TsTypeMisc "C")
          , (TsTypeSubtype "D" (TsTypeMisc "E"), Just $ TsTypeGeneric "F" $ typeArgs [TsTypeMisc "G"])
          ]

    it "requires at least one type argument" $ do
      parse' pTypeArgs `shouldFailOn` "<>"

  describe "pParams" $ do
    it "parses empty params" $ do
      parse' pParams "()" `shouldParse` []

    it "parses single param" $ do
      parse' pParams "(a: number)" `shouldParse` [ Required $ Normal $ TsTypeMisc "number"]

    it "parses multiple params" $ do
      parse' pParams "(a: number, b: <A>(x: string) => void, c: Array<boolean>)" `shouldParse`
        [ Required $ Normal $ TsTypeMisc "number"
        , Required $ Normal $ TsTypeFunction (Function (Just $ typeArgs [TsTypeMisc "A"]) [Required $ Normal $ TsTypeMisc "string"] TsTypeVoid)
        , Required $ Normal $ TsTypeGeneric "Array" $ typeArgs [TsTypeMisc "boolean"]
        ]

    it "parses rest params" $ do
      parse' pParams "(...a: number, b: string, ...c: boolean)" `shouldParse`
        [ Required $ Rest $ TsTypeMisc "number"
        , Required $ Normal $ TsTypeMisc "string"
        , Required $ Rest $ TsTypeMisc "boolean"
        ]

    it "parses optional params" $ do
      parse' pParams "(a?: A, ...b?: B, c: C)" `shouldParse`
        [ Optional $ Normal $ TsTypeMisc "A"
        , Optional $ Rest $ TsTypeMisc "B"
        , Required $ Normal $ TsTypeMisc "C"
        ]

    it "parses optional whitespace and newlines" $ do
      parse' pParams (unlines' ["(", ")"]) `shouldParse` []
      parse' pParams (unlines' ["(", "x: A", ")"]) `shouldParse` [Required $ Normal $ TsTypeMisc "A"]
      parse' pParams (unlines' ["(", "x: A,", "y: B", ")"]) `shouldParse` [Required $ Normal $ TsTypeMisc "A", Required $ Normal $ TsTypeMisc "B"]

    it "parses optional trailing comma in non-empty params" $ do
      parse' pParams "(a: number,)" `shouldParse` [ Required $ Normal $ TsTypeMisc "number"]

  describe "pFunctionLiteralReturn" $ do
    it "parses value after the lambda" $ do
      parse' pFunctionLiteralReturn " => void" `shouldParse` TsTypeVoid
      parse' pFunctionLiteralReturn " => string" `shouldParse` TsTypeMisc "string"
      parse' pFunctionLiteralReturn " => <A>(x: A) => A" `shouldParse` TsTypeFunction (Function (Just $ typeArgs [TsTypeMisc "A"]) [Required $ Normal $ TsTypeMisc "A"] (TsTypeMisc "A"))

    it "requires a value after the lambda" $ do
      parse' pFunctionLiteralReturn `shouldFailOn` " => "

  describe "pAlias" $ do
    let p = parse' $ pAlias <* eof

    it "optionally supports semicolons" $ do
      p "type X = Y" `shouldParse` Alias "X" Nothing (TsTypeMisc "Y")
      p "type X = Y;" `shouldParse` Alias "X" Nothing (TsTypeMisc "Y")

    it "parses type arguments" $ do
      p "type X<A, B extends string> = A | B" `shouldParse`
        Alias
          "X"
          (Just $ typeArgs [TsTypeMisc "A", TsTypeSubtype "B" (TsTypeMisc "string")])
          (TsTypeExpression TsOperatorUnion (TsTypeMisc "A") (TsTypeMisc "B"))

  describe "pInterface" $ do
    let p = parse' $ pInterface <* eof

    it "parses without extends" $ do
      p "interface X { a: A }" `shouldParse`
        Interface
          "X"
          Nothing
          Nothing
          [Required ("a", TsTypeMisc "A")]

      p "interface X<A, B extends Array<A>> { a: A }" `shouldParse`
        Interface
          "X"
          (Just $ typeArgs [TsTypeMisc "A", TsTypeSubtype "B" (TsTypeGeneric "Array" $ typeArgs [TsTypeMisc "A"])])
          Nothing
          [Required ("a", TsTypeMisc "A")]

    it "parses with extends" $ do
      p "interface X extends B { a: A }" `shouldParse`
        Interface
          "X"
          Nothing
          (Just $ TsTypeMisc "B")
          [Required ("a", TsTypeMisc "A")]

      p "interface X<A, B extends Array<A>> extends C { a: A }" `shouldParse`
        Interface
          "X"
          (Just $ typeArgs [TsTypeMisc "A", TsTypeSubtype "B" (TsTypeGeneric "Array" $ typeArgs [TsTypeMisc "A"])])
          (Just $ TsTypeMisc "C")
          [Required ("a", TsTypeMisc "A")]

  describe "pConstDeclaration" $ do
    let p = parse' $ pConstDeclaration <* eof

    it "optionally supports semicolons" $ do
      p "declare const x: string" `shouldParse` ConstDeclaration "x" (TsTypeMisc "string")
      p "declare const x: string;" `shouldParse` ConstDeclaration "x" (TsTypeMisc "string")

  describe "pFunctionDeclaration" $ do
    let p = parse' $ pFunctionDeclaration <* eof

    it "parses" $ do
      p "declare function f<A>(x: A): <B extends A>(y: B) => C" `shouldParse` FunctionDeclaration "f" (Function (Just $ typeArgs [TsTypeMisc "A"]) [Required $ Normal $ TsTypeMisc "A"] (TsTypeFunction $ Function (Just $ typeArgs [TsTypeSubtype "B" (TsTypeMisc "A")]) [Required $ Normal $ TsTypeMisc "B"] (TsTypeMisc "C")))

  describe "pSignature" $ do
    it "parses all variants" $ do
      parse' pSignature "declare const f: void" `shouldParse` SignatureConstDeclaration (ConstDeclaration "f" TsTypeVoid)
      parse' pSignature "export declare const f: void" `shouldParse` SignatureConstDeclaration (ConstDeclaration "f" TsTypeVoid)
      parse' pSignature "type X = void" `shouldParse` SignatureAlias (Alias "X" Nothing TsTypeVoid)
      parse' pSignature "export type X = void" `shouldParse` SignatureAlias (Alias "X" Nothing TsTypeVoid)
      parse' pSignature "interface X {}" `shouldParse` SignatureInterface (Interface "X" Nothing Nothing [])
      parse' pSignature "export interface X {}" `shouldParse` SignatureInterface (Interface "X" Nothing Nothing [])

    it "parses real signatures" $ do
      parse' pSignature "export declare const empty: ''" `shouldParse` SignatureConstDeclaration (ConstDeclaration "empty" (TsTypeStringLiteral ""))

      parse' pSignature "export declare const aperture: (n: number) => <A>(xs: A[]) => A[][]" `shouldParse`
        SignatureConstDeclaration (ConstDeclaration "aperture" (TsTypeFunction (Function Nothing [Required $ Normal $ TsTypeMisc "number"] (TsTypeFunction (Function (Just $ typeArgs [TsTypeMisc "A"]) [Required $ Normal $ TsTypeGeneric "Array" $ typeArgs [TsTypeMisc "A"]] (TsTypeGeneric "Array" $ typeArgs [TsTypeGeneric "Array" $ typeArgs [TsTypeMisc "A"]]))))))

      parse' pSignature "export declare const anyPass: <A>(fs: Predicate<A>[]) => Predicate<A>" `shouldParse`
        SignatureConstDeclaration (ConstDeclaration "anyPass" (TsTypeFunction (Function (Just $ typeArgs [TsTypeMisc "A"]) [Required $ Normal $ TsTypeGeneric "Array" $ typeArgs [TsTypeGeneric "Predicate" $ typeArgs [TsTypeMisc "A"]]] (TsTypeGeneric "Predicate" $ typeArgs [TsTypeMisc "A"]))))

      parse' pSignature "export declare const merge: <A>(x: A) => <B>(y: B) => A & B" `shouldParse`
        SignatureConstDeclaration (ConstDeclaration "merge" (TsTypeFunction (Function (Just $ typeArgs [TsTypeMisc "A"]) [Required $ Normal $ TsTypeMisc "A"] (TsTypeFunction (Function (Just $ typeArgs [TsTypeMisc "B"]) [Required $ Normal $ TsTypeMisc "B"] (TsTypeExpression TsOperatorIntersection (TsTypeMisc "A") (TsTypeMisc "B")))))))

      parse' pSignature "export declare const omit: <K extends string>(ks: K[]) => <V, A extends Record<K, V>>(x: Partial<A>) => Pick<A, Exclude<keyof A, K>>" `shouldParse`
        SignatureConstDeclaration (ConstDeclaration "omit" (TsTypeFunction (Function (Just $ typeArgs [TsTypeSubtype "K" (TsTypeMisc "string")]) [Required $ Normal $ TsTypeGeneric "Array" $ typeArgs [TsTypeMisc "K"]] (TsTypeFunction (Function (Just $ typeArgs [TsTypeMisc "V", TsTypeSubtype "A" (TsTypeGeneric "Record" $ typeArgs [TsTypeMisc "K", TsTypeMisc "V"])]) [Required $ Normal $ TsTypeGeneric "Partial" $ typeArgs [TsTypeMisc "A"]] (TsTypeGeneric "Pick" $ typeArgs [TsTypeMisc "A", TsTypeGeneric "Exclude" $ typeArgs [TsTypeKeysOf (TsTypeMisc "A"), TsTypeMisc "K"]]))))))

      parse' pSignature "export declare const unary: <A extends unknown[], B>(f: (...xs: A) => B) => (xs: A) => B" `shouldParse`
        SignatureConstDeclaration (ConstDeclaration "unary" (TsTypeFunction (Function (Just $ typeArgs [TsTypeSubtype "A" (TsTypeGeneric "Array" $ typeArgs [TsTypeUnknown]), TsTypeMisc "B"]) [Required $ Normal $ TsTypeFunction (Function Nothing [Required $ Rest $ TsTypeMisc "A"] (TsTypeMisc "B"))] (TsTypeFunction (Function Nothing [Required $ Normal $ TsTypeMisc "A"] (TsTypeMisc "B"))))))

      parse' pSignature "export interface Some<A> { readonly _tag: 'Some', readonly value: A }" `shouldParse`
        SignatureInterface (Interface "Some" (Just $ typeArgs [TsTypeMisc "A"]) Nothing [Required ("_tag", TsTypeStringLiteral "Some"), Required ("value", TsTypeMisc "A")])
