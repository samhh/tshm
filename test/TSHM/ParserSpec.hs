module TSHM.ParserSpec (spec) where

import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range
import           Prelude
import           TSHM.Parser
import           TSHM.TypeScript
import           Test.Hspec
import           Test.Hspec.Hedgehog   (PropertyT, forAll, hedgehog, (===))
import           Test.Hspec.Megaparsec
import           Text.Megaparsec       (ParseErrorBundle, Parsec, parse, eof)

unlines' :: [String] -> String
unlines' = concatMap (<> "\n")

parse' :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse' = flip parse ""

(/=*) :: (Eq a, Show a) => (s -> Either e a) -> s -> PropertyT IO ()
f /=* a = first (const ()) (f a) === Left ()

(=*=) :: (Eq a, Eq e, Show a, Show e) => Either e a -> a -> PropertyT IO ()
a =*= b = a === Right b

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

    it "parses string literal" $ do
      parse' pType "'abc'" `shouldParse` TsTypeStringLiteral "abc"

    it "parses number literal" $ do
      parse' pType "-.123" `shouldParse` TsTypeNumberLiteral "-.123"

    it "parses special array syntax" $ do
      parse' pType "a[][]" `shouldParse`
        TsTypeGeneric "Array" (fromList [TsTypeGeneric "Array" $ fromList [TsTypeMisc "a"]])

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
        TsTypeGeneric "Array" (fromList [TsTypeObjectReference (TsTypeGeneric "Array" (fromList [TsTypeGeneric "Array" (fromList [TsTypeObjectReference (TsTypeObjectReference (TsTypeMisc "a") "k1") "k2"])])) "k3"])

    it "parses function" $ do
      parse' pType "<A, B>(a: A) => B" `shouldParse`
         TsTypeFunction (Function (Just $ fromList [TsTypeMisc "A", TsTypeMisc "B"]) [Required $ Normal $ TsTypeMisc "A"] (TsTypeMisc "B"))

      parse' pType "(x: F<A, B>) => G<C, D>" `shouldParse`
         TsTypeFunction (Function Nothing [Required $ Normal $ TsTypeGeneric "F" $ fromList [TsTypeMisc "A", TsTypeMisc "B"]] (TsTypeGeneric "G" $ fromList [TsTypeMisc "C", TsTypeMisc "D"]))

    it "parses infix operators" $ do
      parse' pType "A & B | C" `shouldParse` TsTypeExpression TsOperatorIntersection (TsTypeMisc "A") (TsTypeExpression TsOperatorUnion (TsTypeMisc "B") (TsTypeMisc "C"))

    it "parses keyof" $ do
      parse' pType "keyof A<B>" `shouldParse` TsTypeKeysOf (TsTypeGeneric "A" $ fromList [TsTypeMisc "B"])

    it "parses typeof" $ do
      parse' pType "typeof x & y" `shouldParse` TsTypeExpression TsOperatorIntersection (TsTypeReflection "x") (TsTypeMisc "y")

    it "parses parentheses and changes precedence accordingly" $ do
      parse' pType "(A & B) | C" `shouldParse` TsTypeExpression TsOperatorUnion (TsTypeGrouped $ TsTypeExpression TsOperatorIntersection (TsTypeMisc "A") (TsTypeMisc "B")) (TsTypeMisc "C")

  describe "pFunction" $ do
    it "parses minimal viable function" $ do
      parse' pFunction "() => void" `shouldParse` Function Nothing [] TsTypeVoid

    it "parses type arguments" $ do
      parse' pFunction "<A, Either<void, B>>() => void" `shouldParse`
        Function (Just $ fromList [TsTypeMisc "A", TsTypeGeneric "Either" $ fromList [TsTypeVoid, TsTypeMisc "B"]]) [] TsTypeVoid

    it "parses params" $ do
      parse' pFunction "(x: number, y: string) => void" `shouldParse`
        Function Nothing [Required $ Normal $ TsTypeMisc "number", Required $ Normal $ TsTypeMisc "string"] TsTypeVoid

    it "parses return type" $ do
      parse' pFunction "() => number" `shouldParse` Function Nothing [] (TsTypeMisc "number")

    it "parses curried function" $ do
      parse' pFunction "() => () => void" `shouldParse` Function Nothing [] (TsTypeFunction (Function Nothing [] TsTypeVoid))

    it "supports extends clause in function generics" $ do
      parse' pFunction "<B, A extends Array<B>>(f: <C extends number, D>(x: 'ciao') => string) => void" `shouldParse`
        Function
          ( Just $ fromList
            [ TsTypeMisc "B"
            , TsTypeSubtype "A" (TsTypeGeneric "Array" $ fromList [TsTypeMisc "B"])
            ]
          )
          [ Required $ Normal $ TsTypeFunction (Function
              (Just $ fromList [TsTypeSubtype "C" (TsTypeMisc "number"), TsTypeMisc "D"])
              [Required $ Normal $ TsTypeStringLiteral "ciao"]
              (TsTypeMisc "string"))
          ]
          TsTypeVoid

    it "parses complex function" $ do
      parse' pFunction "<A, Array<Option<B>>>(x: number, y: <C>(z: C) => () => C) => () => void" `shouldParse`
        Function
          ( Just $ fromList
            [ TsTypeMisc "A"
            , TsTypeGeneric "Array" $ fromList
              [ TsTypeGeneric "Option" $ fromList
                [ TsTypeMisc "B"
                ]
              ]
            ]
          )
          [ Required $ Normal $ TsTypeMisc "number"
          , Required $ Normal $ TsTypeFunction (Function
              (Just $ fromList [TsTypeMisc "C"])
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
        [Required ("a", TsTypeNumberLiteral "1"), Required ("b", TsTypeGeneric "Array" $ fromList [TsTypeObject [Required ("c", TsTypeBoolean True)]])]

    it "supports mixed comma and semi-colon delimiters" $ do
      parse' pObject "{ a: number, b: string; c: boolean }" `shouldParse`
        [Required ("a", TsTypeMisc "number"), Required ("b", TsTypeMisc "string"), Required ("c", TsTypeMisc "boolean")]

    it "parses optional and required properties" $ do
      parse' pObject "{ a: A, b?: B, c: C }" `shouldParse`
        [Required ("a", TsTypeMisc "A"), Optional ("b", TsTypeMisc "B"), Required ("c", TsTypeMisc "C")]

    it "parses alternative method syntax" $ do
      parse' pObject "{ f?<A>(): void }" `shouldParse`
        [Optional ("f", TsTypeFunction $ Function (Just $ fromList [TsTypeMisc "A"]) [] TsTypeVoid)]

  describe "pTuple" $ do
    it "parses empty tuple" $ do
      parse' pTuple "[]" `shouldParse` []

    it "parses non-empty flat tuple" $ do
      parse' pTuple "[a, 'b']" `shouldParse` [TsTypeMisc "a", TsTypeStringLiteral "b"]

    it "parses non-empty nested tuple" $ do
      parse' pTuple "[a, ['b', 3]]" `shouldParse` [TsTypeMisc "a", TsTypeTuple [TsTypeStringLiteral "b", TsTypeNumberLiteral "3"]]

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

  describe "pDeclarationName" $ do
    let ident = Gen.list (Range.linear 1 99) Gen.alpha

    it "parses const declaration" $ hedgehog $ do
      x <- forAll ident
      parse' pDeclarationName ("declare const " <> x <> ": ") =*= x

    it "parses exported const declaration" $ hedgehog $ do
      x <- forAll ident
      parse' pDeclarationName ("export declare const " <> x <> ": ") =*= x

    it "requires declaration" $ hedgehog $ do
      x <- forAll ident
      parse' pDeclarationName /=* ("const " <> x <> ": ")

  describe "pTypeArgs" $ do
    it "parses single flat type argument" $ do
      parse' pTypeArgs "<A>" `shouldParse`
        fromList [ TsTypeMisc "A"
        ]

    it "parses multiple flat type arguments" $ do
      parse' pTypeArgs "<A, B, C>" `shouldParse`
        fromList [ TsTypeMisc "A"
        , TsTypeMisc "B"
        , TsTypeMisc "C"
        ]

    it "parses single nested type argument" $ do
      parse' pTypeArgs "<Array<Option<A>>>" `shouldParse`
        fromList [ TsTypeGeneric "Array" $ fromList
          [ TsTypeGeneric "Option" $ fromList
            [ TsTypeMisc "A"
            ]
          ]
        ]

    it "parses mixed type arguments" $ do
      parse' pTypeArgs "<A, Array<Option<B>, C>, Either<E, A>>" `shouldParse`
        fromList [ TsTypeMisc "A"
        , TsTypeGeneric "Array" $ fromList
          [ TsTypeGeneric "Option" $ fromList
            [ TsTypeMisc "B"
            ]
          , TsTypeMisc "C"
          ]
        , TsTypeGeneric "Either" $ fromList
          [ TsTypeMisc "E"
          , TsTypeMisc "A"
          ]
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
        , Required $ Normal $ TsTypeFunction (Function (Just $ fromList [TsTypeMisc "A"]) [Required $ Normal $ TsTypeMisc "string"] TsTypeVoid)
        , Required $ Normal $ TsTypeGeneric "Array" $ fromList [TsTypeMisc "boolean"]
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

  describe "pReturn" $ do
    it "parses value after the lambda" $ do
      parse' pReturn " => void" `shouldParse` TsTypeVoid
      parse' pReturn " => string" `shouldParse` TsTypeMisc "string"
      parse' pReturn " => <A>(x: A) => A" `shouldParse` TsTypeFunction (Function (Just $ fromList [TsTypeMisc "A"]) [Required $ Normal $ TsTypeMisc "A"] (TsTypeMisc "A"))

    it "requires a value after the lambda" $ do
      parse' pReturn `shouldFailOn` " => "

  describe "pAlias" $ do
    let p = parse' $ pAlias <* eof

    it "optionally supports semicolons" $ do
      p "type X = Y" `shouldParse` Alias "X" Nothing (TsTypeMisc "Y")
      p "type X = Y;" `shouldParse` Alias "X" Nothing (TsTypeMisc "Y")

    it "parses type arguments" $ do
      p "type X<A, B extends string> = A | B" `shouldParse`
        Alias
          "X"
          (Just $ fromList [TsTypeMisc "A", TsTypeSubtype "B" (TsTypeMisc "string")])
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
          (Just $ fromList [TsTypeMisc "A", TsTypeSubtype "B" (TsTypeGeneric "Array" $ fromList [TsTypeMisc "A"])])
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
          (Just $ fromList [TsTypeMisc "A", TsTypeSubtype "B" (TsTypeGeneric "Array" $ fromList [TsTypeMisc "A"])])
          (Just $ TsTypeMisc "C")
          [Required ("a", TsTypeMisc "A")]

  describe "pDeclaration" $ do
    let p = parse' $ pDeclaration <* eof

    it "optionally supports semicolons" $ do
      p "declare const x: string" `shouldParse` Declaration "x" (TsTypeMisc "string")
      p "declare const x: string;" `shouldParse` Declaration "x" (TsTypeMisc "string")

  describe "pSignature" $ do
    it "parses all variants" $ do
      parse' pSignature "declare const f: void" `shouldParse` SignatureDeclaration (Declaration "f" TsTypeVoid)
      parse' pSignature "export declare const f: void" `shouldParse` SignatureDeclaration (Declaration "f" TsTypeVoid)
      parse' pSignature "type X = void" `shouldParse` SignatureAlias (Alias "X" Nothing TsTypeVoid)
      parse' pSignature "export type X = void" `shouldParse` SignatureAlias (Alias "X" Nothing TsTypeVoid)
      parse' pSignature "interface X {}" `shouldParse` SignatureInterface (Interface "X" Nothing Nothing [])
      parse' pSignature "export interface X {}" `shouldParse` SignatureInterface (Interface "X" Nothing Nothing [])

    it "parses real signatures" $ do
      parse' pSignature "export declare const empty: ''" `shouldParse` SignatureDeclaration (Declaration "empty" (TsTypeStringLiteral ""))

      parse' pSignature "export declare const aperture: (n: number) => <A>(xs: A[]) => A[][]" `shouldParse`
        SignatureDeclaration (Declaration "aperture" (TsTypeFunction (Function Nothing [Required $ Normal $ TsTypeMisc "number"] (TsTypeFunction (Function (Just $ fromList [TsTypeMisc "A"]) [Required $ Normal $ TsTypeGeneric "Array" $ fromList [TsTypeMisc "A"]] (TsTypeGeneric "Array" $ fromList [TsTypeGeneric "Array" $ fromList [TsTypeMisc "A"]]))))))

      parse' pSignature "export declare const anyPass: <A>(fs: Predicate<A>[]) => Predicate<A>" `shouldParse`
        SignatureDeclaration (Declaration "anyPass" (TsTypeFunction (Function (Just $ fromList [TsTypeMisc "A"]) [Required $ Normal $ TsTypeGeneric "Array" $ fromList [TsTypeGeneric "Predicate" $ fromList [TsTypeMisc "A"]]] (TsTypeGeneric "Predicate" $ fromList [TsTypeMisc "A"]))))

      parse' pSignature "export declare const merge: <A>(x: A) => <B>(y: B) => A & B" `shouldParse`
        SignatureDeclaration (Declaration "merge" (TsTypeFunction (Function (Just $ fromList [TsTypeMisc "A"]) [Required $ Normal $ TsTypeMisc "A"] (TsTypeFunction (Function (Just $ fromList [TsTypeMisc "B"]) [Required $ Normal $ TsTypeMisc "B"] (TsTypeExpression TsOperatorIntersection (TsTypeMisc "A") (TsTypeMisc "B")))))))

      parse' pSignature "export declare const omit: <K extends string>(ks: K[]) => <V, A extends Record<K, V>>(x: Partial<A>) => Pick<A, Exclude<keyof A, K>>" `shouldParse`
        SignatureDeclaration (Declaration "omit" (TsTypeFunction (Function (Just $ fromList [TsTypeSubtype "K" (TsTypeMisc "string")]) [Required $ Normal $ TsTypeGeneric "Array" $ fromList [TsTypeMisc "K"]] (TsTypeFunction (Function (Just $ fromList [TsTypeMisc "V", TsTypeSubtype "A" (TsTypeGeneric "Record" $ fromList [TsTypeMisc "K", TsTypeMisc "V"])]) [Required $ Normal $ TsTypeGeneric "Partial" $ fromList [TsTypeMisc "A"]] (TsTypeGeneric "Pick" $ fromList [TsTypeMisc "A", TsTypeGeneric "Exclude" $ fromList [TsTypeKeysOf (TsTypeMisc "A"), TsTypeMisc "K"]]))))))

      parse' pSignature "export declare const unary: <A extends unknown[], B>(f: (...xs: A) => B) => (xs: A) => B" `shouldParse`
        SignatureDeclaration (Declaration "unary" (TsTypeFunction (Function (Just $ fromList [TsTypeSubtype "A" (TsTypeGeneric "Array" $ fromList [TsTypeMisc "unknown"]), TsTypeMisc "B"]) [Required $ Normal $ TsTypeFunction (Function Nothing [Required $ Rest $ TsTypeMisc "A"] (TsTypeMisc "B"))] (TsTypeFunction (Function Nothing [Required $ Normal $ TsTypeMisc "A"] (TsTypeMisc "B"))))))

      parse' pSignature "export interface Some<A> { readonly _tag: 'Some', readonly value: A }" `shouldParse`
        SignatureInterface (Interface "Some" (Just $ fromList [TsTypeMisc "A"]) Nothing [Required ("_tag", TsTypeStringLiteral "Some"), Required ("value", TsTypeMisc "A")])
