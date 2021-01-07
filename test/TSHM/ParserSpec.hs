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

typeArgs' :: [Expr] -> NonEmpty TypeArg
typeArgs' = fmap (, Nothing) . fromList

spec :: Spec
spec = describe "TSHM.Parser" $ do
  describe "expr" $ do
    it "parses void" $ do
      parse' expr "void" `shouldParse` TVoid

    it "parses boolean" $ do
      parse' expr "true" `shouldParse` TBoolean True
      parse' expr "false" `shouldParse` TBoolean False

    it "parses primitive" $ do
      parse' expr "A" `shouldParse` TMisc "A"
      parse' expr "string" `shouldParse` TMisc "string"

    it "parses primitive with identifier starting with a known identifier" $ do
      parse' expr "void" `shouldParse` TVoid
      parse' expr "voidx" `shouldParse` TMisc "voidx"

    it "parses string literal" $ do
      parse' expr "'abc'" `shouldParse` TString "abc"

    it "parses number literal" $ do
      parse' expr "-.123" `shouldParse` TNumber "-.123"

    it "parses special array syntax" $ do
      parse' expr "a[][]" `shouldParse`
        TGeneric "Array" (typeArgs' [TGeneric "Array" $ typeArgs' [TMisc "a"]])

    it "parses readonly" $ do
      parse' expr "readonly [readonly A, { readonly x: readonly B }]" `shouldParse`
        TUnOp UnOpReadonly (TTuple [TUnOp UnOpReadonly (TMisc "A"), TObject (ObjectLit [ObjectPair Immut Required ("x", TUnOp UnOpReadonly (TMisc "B"))])])

    it "parses object reference" $ do
      parse' expr "A['key']" `shouldParse` TIndexedAccess (TMisc "A") (TString "key")
      parse' expr "A[\"key\"]" `shouldParse` TIndexedAccess (TMisc "A") (TString "key")
      parse' expr "true['k1']['k2']" `shouldParse`
        TIndexedAccess (TIndexedAccess (TBoolean True) (TString "k1")) (TString "k2")
      parse' expr "A[K]" `shouldParse` TIndexedAccess (TMisc "A") (TMisc "K")

    it "postfix operators play nicely together" $ do
      parse' expr "a['k1']['k2'][][]['k3'][]" `shouldParse`
        TGeneric "Array" (typeArgs' [TIndexedAccess (TGeneric "Array" (typeArgs' [TGeneric "Array" (typeArgs' [TIndexedAccess (TIndexedAccess (TMisc "a") (TString "k1")) (TString "k2")])])) (TString "k3")])

    it "parses function" $ do
      parse' expr "<A, B>(a: A) => B" `shouldParse`
         TLambda (Lambda (Just $ typeArgs' [TMisc "A", TMisc "B"]) [(Required, Normal, TMisc "A")] (TMisc "B"))

      parse' expr "(x: F<A, B>) => G<C, D>" `shouldParse`
         TLambda (Lambda Nothing [(Required, Normal, TGeneric "F" $ typeArgs' [TMisc "A", TMisc "B"])] (TGeneric "G" $ typeArgs' [TMisc "C", TMisc "D"]))

    it "parses infix operators" $ do
      parse' expr "A & B | C" `shouldParse` TBinOp BinOpIntersection (TMisc "A") (TBinOp BinOpUnion (TMisc "B") (TMisc "C"))

    it "parses keyof" $ do
      parse' expr "keyof A<B>" `shouldParse` TUnOp UnOpKeys (TGeneric "A" $ typeArgs' [TMisc "B"])
      parse' expr "keyofx" `shouldParse` TMisc "keyofx"

    it "parses typeof" $ do
      parse' expr "typeof x & y" `shouldParse` TBinOp BinOpIntersection (TUnOp UnOpReflection (TMisc "x")) (TMisc "y")
      parse' expr "typeofx" `shouldParse` TMisc "typeofx"

    it "parses parentheses and changes precedence accordingly" $ do
      parse' expr "(A & B) | C" `shouldParse` TBinOp BinOpUnion (TGrouped $ TBinOp BinOpIntersection (TMisc "A") (TMisc "B")) (TMisc "C")

  describe "lambda" $ do
    it "parses minimal viable function" $ do
      parse' lambda "() => void" `shouldParse` Lambda Nothing [] TVoid

    it "parses type arguments" $ do
      parse' lambda "<A, Either<void, B>>() => void" `shouldParse`
        Lambda (Just $ typeArgs' [TMisc "A", TGeneric "Either" $ typeArgs' [TVoid, TMisc "B"]]) [] TVoid

    it "parses params" $ do
      parse' lambda "(x: number, y: string) => void" `shouldParse`
        Lambda Nothing [(Required, Normal, TMisc "number"), (Required, Normal, TMisc "string")] TVoid

    it "parses return type" $ do
      parse' lambda "() => number" `shouldParse` Lambda Nothing [] (TMisc "number")

    it "parses curried function" $ do
      parse' lambda "() => () => void" `shouldParse` Lambda Nothing [] (TLambda (Lambda Nothing [] TVoid))

    it "supports extends clause in function generics" $ do
      parse' lambda "<B, A extends Array<B>>(f: <C extends number, D>(x: 'ciao') => string) => void" `shouldParse`
        Lambda
          ( Just $ typeArgs'
            [ TMisc "B"
            , TSubtype "A" (TGeneric "Array" $ typeArgs' [TMisc "B"])
            ]
          )
          [ (Required, Normal, TLambda (Lambda
            (Just $ typeArgs' [TSubtype "C" (TMisc "number"), TMisc "D"])
            [(Required, Normal, TString "ciao")]
            (TMisc "string")))
          ]
          TVoid

    it "parses complex function" $ do
      parse' lambda "<A, Array<Option<B>>>(x: number, y: <C>(z: C) => () => C) => () => void" `shouldParse`
        Lambda
          ( Just $ typeArgs'
            [ TMisc "A"
            , TGeneric "Array" $ typeArgs'
              [ TGeneric "Option" $ typeArgs'
                [ TMisc "B"
                ]
              ]
            ]
          )
          [ (Required, Normal, TMisc "number")
          , (Required, Normal, TLambda (Lambda
              (Just $ typeArgs' [TMisc "C"])
              [(Required, Normal, TMisc "C")]
              (TLambda (Lambda Nothing [] (TMisc "C")))))
          ]
          ( TLambda (Lambda Nothing [] TVoid))

  describe "object" $ do
    it "parses empty object" $ do
      parse' object "{}" `shouldParse` ObjectLit []

    it "parses non-empty flat object" $ do
      parse' object "{ a: 1, b: 'two' }" `shouldParse` ObjectLit [ObjectPair Mut Required ("a", TNumber "1"), ObjectPair Mut Required ("b", TString "two")]

    it "parses non-empty nested object" $ do
      parse' object "{ a: 1, b: { c: true }[] }" `shouldParse`
        ObjectLit [ObjectPair Mut Required ("a", TNumber "1"), ObjectPair Mut Required ("b", TGeneric "Array" $ typeArgs' [TObject (ObjectLit [ObjectPair Mut Required ("c", TBoolean True)])])]

    it "supports mixed comma and semi-colon delimiters" $ do
      parse' object "{ a: number, b: string; c: boolean }" `shouldParse`
        ObjectLit [ObjectPair Mut Required ("a", TMisc "number"), ObjectPair Mut Required ("b", TMisc "string"), ObjectPair Mut Required ("c", TMisc "boolean")]

    it "parses optional and required properties" $ do
      parse' object "{ a: A, b?: B, c: C }" `shouldParse`
        ObjectLit [ObjectPair Mut Required ("a", TMisc "A"), ObjectPair Mut Optional ("b", TMisc "B"), ObjectPair Mut Required ("c", TMisc "C")]

    it "parses alternative method syntax" $ do
      parse' object "{ f?<A>(): void }" `shouldParse`
        ObjectLit [ObjectPair Mut Optional ("f", TLambda $ Lambda (Just $ typeArgs' [TMisc "A"]) [] TVoid)]

    it "parses trailing delimiter in non-empty object" $ do
      parse' object "{ a: number; b: string, }" `shouldParse`
        ObjectLit [ObjectPair Mut Required ("a", TMisc "number"), ObjectPair Mut Required ("b", TMisc "string")]

    it "parses mapped type" $ do
      parse' object "{ [K in A]: B }" `shouldParse`
        ObjectMapped Mut Required ("K", TMisc "A") (TMisc "B")

      parse' object "{ [K in A]?: B; }" `shouldParse`
        ObjectMapped Mut Optional ("K", TMisc "A") (TMisc "B")

  describe "tuple" $ do
    it "parses empty tuple" $ do
      parse' tuple "[]" `shouldParse` []

    it "parses non-empty flat tuple" $ do
      parse' tuple "[a, 'b']" `shouldParse` [TMisc "a", TString "b"]

    it "parses non-empty nested tuple" $ do
      parse' tuple "[a, ['b', 3]]" `shouldParse` [TMisc "a", TTuple [TString "b", TNumber "3"]]

    it "parses non-empty tuple with trailing comma" $ do
      parse' tuple "[a,]" `shouldParse` [TMisc "a"]
      parse' tuple "[a, ]" `shouldParse` [TMisc "a"]
      parse' tuple "[a, b,]" `shouldParse` [TMisc "a", TMisc "b"]
      parse' tuple "[a, b, ]" `shouldParse` [TMisc "a", TMisc "b"]

  describe "num" $ do
    let p = num <* eof

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

  describe "constDecIdent" $ do
    let ident' = Gen.list (Range.linear 1 99) Gen.alpha

    it "parses const declaration" $ hedgehog $ do
      x <- forAll ident'
      parse' constDecIdent ("declare const " <> x <> ": ") =*= x

    it "parses exported const declaration" $ hedgehog $ do
      x <- forAll ident'
      parse' constDecIdent ("export declare const " <> x <> ": ") =*= x

    it "requires declaration" $ hedgehog $ do
      x <- forAll ident'
      parse' constDecIdent /=* ("const " <> x <> ": ")

  describe "typeArgs'" $ do
    it "parses single flat type argument" $ do
      parse' typeArgs "<A>" `shouldParse`
        typeArgs' [ TMisc "A"
        ]

    it "parses multiple flat type arguments" $ do
      parse' typeArgs "<A, B, C>" `shouldParse`
        typeArgs' [ TMisc "A"
        , TMisc "B"
        , TMisc "C"
        ]

    it "parses single nested type argument" $ do
      parse' typeArgs "<Array<Option<A>>>" `shouldParse`
        typeArgs' [ TGeneric "Array" $ typeArgs'
          [ TGeneric "Option" $ typeArgs'
            [ TMisc "A"
            ]
          ]
        ]

    it "parses mixed type arguments" $ do
      parse' typeArgs "<A, Array<Option<B>, C>, Either<E, A>>" `shouldParse`
        typeArgs'
          [ TMisc "A"
          , TGeneric "Array" $ typeArgs'
            [ TGeneric "Option" $ typeArgs'
              [ TMisc "B"
              ]
            , TMisc "C"
            ]
          , TGeneric "Either" $ typeArgs'
            [ TMisc "E"
            , TMisc "A"
            ]
          ]

    it "parses default type arguments" $ do
      parse' typeArgs "<A, B = C, D extends E = F<G>>" `shouldParse`
        fromList
          [ (TMisc "A", Nothing)
          , (TMisc "B", Just $ TMisc "C")
          , (TSubtype "D" (TMisc "E"), Just $ TGeneric "F" $ typeArgs' [TMisc "G"])
          ]

    it "requires at least one type argument" $ do
      parse' typeArgs `shouldFailOn` "<>"

    it "parses trailing comma in non-empty arguments list" $ do
      parse' typeArgs "<A,>" `shouldParse`
        typeArgs' [ TMisc "A"
        ]

  describe "params" $ do
    it "parses empty params" $ do
      parse' params "()" `shouldParse` []

    it "parses single param" $ do
      parse' params "(a: number)" `shouldParse` [(Required, Normal, TMisc "number")]

    it "parses multiple params" $ do
      parse' params "(a: number, b: <A>(x: string) => void, c: Array<boolean>)" `shouldParse`
        [ (Required, Normal, TMisc "number")
        , (Required, Normal, TLambda (Lambda (Just $ typeArgs' [TMisc "A"]) [(Required, Normal, TMisc "string")] TVoid))
        , (Required, Normal, TGeneric "Array" $ typeArgs' [TMisc "boolean"])
        ]

    it "parses rest params" $ do
      parse' params "(...a: number, b: string, ...c: boolean)" `shouldParse`
        [ (Required, Rest, TMisc "number")
        , (Required, Normal, TMisc "string")
        , (Required, Rest, TMisc "boolean")
        ]

    it "parses optional params" $ do
      parse' params "(a?: A, ...b?: B, c: C)" `shouldParse`
        [ (Optional, Normal, TMisc "A")
        , (Optional, Rest, TMisc "B")
        , (Required, Normal, TMisc "C")
        ]

    it "parses optional whitespace and newlines" $ do
      parse' params (unlines' ["(", ")"]) `shouldParse` []
      parse' params (unlines' ["(", "x: A", ")"]) `shouldParse` [(Required, Normal, TMisc "A")]
      parse' params (unlines' ["(", "x: A,", "y: B", ")"]) `shouldParse` [(Required, Normal, TMisc "A"), (Required, Normal, TMisc "B")]

    it "parses optional trailing comma in non-empty params" $ do
      parse' params "(a: number,)" `shouldParse` [(Required, Normal, TMisc "number")]

    it "parses destructures" $ do
      parse' params "({ a: [a, { a, ...a }, ...a], a: a, ...a }: x)" `shouldParse` [(Required, Normal, TMisc "x")]
      parse' params "([a, { a, ...a }, ...a]: x)" `shouldParse` [(Required, Normal, TMisc "x")]

  describe "alias" $ do
    let p = parse' $ alias <* eof

    it "optionally supports semicolons" $ do
      p "type X = Y" `shouldParse` Alias "X" Nothing (TMisc "Y")
      p "type X = Y;" `shouldParse` Alias "X" Nothing (TMisc "Y")

    it "parses type arguments" $ do
      p "type X<A, B extends string> = A | B" `shouldParse`
        Alias
          "X"
          (Just $ typeArgs' [TMisc "A", TSubtype "B" (TMisc "string")])
          (TBinOp BinOpUnion (TMisc "A") (TMisc "B"))

  describe "interface" $ do
    let p = parse' $ interface <* eof

    it "parses without extends" $ do
      p "interface X { a: A }" `shouldParse`
        Interface
          "X"
          Nothing
          Nothing
          (ObjectLit [ObjectPair Mut Required ("a", TMisc "A")])

      p "interface X<A, B extends Array<A>> { a: A }" `shouldParse`
        Interface
          "X"
          (Just $ typeArgs' [TMisc "A", TSubtype "B" (TGeneric "Array" $ typeArgs' [TMisc "A"])])
          Nothing
          (ObjectLit [ObjectPair Mut Required ("a", TMisc "A")])

    it "parses with extends" $ do
      p "interface X extends B { a: A }" `shouldParse`
        Interface
          "X"
          Nothing
          (Just $ TMisc "B")
          (ObjectLit [ObjectPair Mut Required ("a", TMisc "A")])

      p "interface X<A, B extends Array<A>> extends C { a: A }" `shouldParse`
        Interface
          "X"
          (Just $ typeArgs' [TMisc "A", TSubtype "B" (TGeneric "Array" $ typeArgs' [TMisc "A"])])
          (Just $ TMisc "C")
          (ObjectLit [ObjectPair Mut Required ("a", TMisc "A")])

  describe "constDec" $ do
    let p = parse' $ constDec <* eof

    it "optionally supports semicolons" $ do
      p "declare const x: string" `shouldParse` ConstDec "x" (TMisc "string")
      p "declare const x: string;" `shouldParse` ConstDec "x" (TMisc "string")

  describe "fnDec" $ do
    let p = parse' $ fnDec <* eof

    -- type Param = (Partial, ParamScope, Expr)
    it "parses" $ do
      p "declare function f<A>(x: A): <B extends A>(y: B) => C" `shouldParse` FunctionDec "f" (Lambda (Just $ typeArgs' [TMisc "A"]) [(Required, Normal, TMisc "A")] (TLambda $ Lambda (Just $ typeArgs' [TSubtype "B" (TMisc "A")]) [(Required, Normal, TMisc "B")] (TMisc "C")))

  describe "signature" $ do
    it "parses and skips comments" $ do
      parse' signature "/**/declare /*x*/const/**/ x/* x xx xxx */: void/**///x" `shouldParse` SignatureConstDec (ConstDec "x" TVoid)

    it "parses all variants" $ do
      parse' signature "declare const f: void" `shouldParse` SignatureConstDec (ConstDec "f" TVoid)
      parse' signature "export declare const f: void" `shouldParse` SignatureConstDec (ConstDec "f" TVoid)
      parse' signature "type X = void" `shouldParse` SignatureAlias (Alias "X" Nothing TVoid)
      parse' signature "export type X = void" `shouldParse` SignatureAlias (Alias "X" Nothing TVoid)
      parse' signature "interface X {}" `shouldParse` SignatureInterface (Interface "X" Nothing Nothing (ObjectLit []))
      parse' signature "export interface X {}" `shouldParse` SignatureInterface (Interface "X" Nothing Nothing (ObjectLit []))
      parse' signature "declare function f(): void" `shouldParse` SignatureFunctionDec (fromList [FunctionDec "f" (Lambda Nothing [] TVoid)])
      parse' signature "export declare function f(): void" `shouldParse` SignatureFunctionDec (fromList [FunctionDec "f" (Lambda Nothing [] TVoid)])
      parse' signature (unlines' ["declare function f(): A", "declare function f(): B"]) `shouldParse`
        SignatureFunctionDec (fromList
          [ FunctionDec "f" (Lambda Nothing [] (TMisc "A"))
          , FunctionDec "f" (Lambda Nothing [] (TMisc "B"))
          ])
      parse' signature (unlines' ["export declare function f(): A", "export declare function f(): B"]) `shouldParse`
        SignatureFunctionDec (fromList
          [ FunctionDec "f" (Lambda Nothing [] (TMisc "A"))
          , FunctionDec "f" (Lambda Nothing [] (TMisc "B"))
          ])

    it "parses real signatures" $ do
      parse' signature "export declare const empty: ''" `shouldParse` SignatureConstDec (ConstDec "empty" (TString ""))

      parse' signature "export declare const aperture: (n: number) => <A>(xs: A[]) => A[][]" `shouldParse`
        SignatureConstDec (ConstDec "aperture" (TLambda (Lambda Nothing [(Required, Normal, TMisc "number")] (TLambda (Lambda (Just $ typeArgs' [TMisc "A"]) [(Required, Normal, TGeneric "Array" $ typeArgs' [TMisc "A"])] (TGeneric "Array" $ typeArgs' [TGeneric "Array" $ typeArgs' [TMisc "A"]]))))))

      parse' signature "export declare const anyPass: <A>(fs: Predicate<A>[]) => Predicate<A>" `shouldParse`
        SignatureConstDec (ConstDec "anyPass" (TLambda (Lambda (Just $ typeArgs' [TMisc "A"]) [(Required, Normal, TGeneric "Array" $ typeArgs' [TGeneric "Predicate" $ typeArgs' [TMisc "A"]])] (TGeneric "Predicate" $ typeArgs' [TMisc "A"]))))

      parse' signature "export declare const merge: <A>(x: A) => <B>(y: B) => A & B" `shouldParse`
        SignatureConstDec (ConstDec "merge" (TLambda (Lambda (Just $ typeArgs' [TMisc "A"]) [(Required, Normal, TMisc "A")] (TLambda (Lambda (Just $ typeArgs' [TMisc "B"]) [(Required, Normal, TMisc "B")] (TBinOp BinOpIntersection (TMisc "A") (TMisc "B")))))))

      parse' signature "export declare const omit: <K extends string>(ks: K[]) => <V, A extends Record<K, V>>(x: Partial<A>) => Pick<A, Exclude<keyof A, K>>" `shouldParse`
        SignatureConstDec (ConstDec "omit" (TLambda (Lambda (Just $ typeArgs' [TSubtype "K" (TMisc "string")]) [(Required, Normal, TGeneric "Array" $ typeArgs' [TMisc "K"])] (TLambda (Lambda (Just $ typeArgs' [TMisc "V", TSubtype "A" (TGeneric "Record" $ typeArgs' [TMisc "K", TMisc "V"])]) [(Required, Normal, TGeneric "Partial" $ typeArgs' [TMisc "A"])] (TGeneric "Pick" $ typeArgs' [TMisc "A", TGeneric "Exclude" $ typeArgs' [TUnOp UnOpKeys (TMisc "A"), TMisc "K"]]))))))

      parse' signature "export declare const unary: <A extends unknown[], B>(f: (...xs: A) => B) => (xs: A) => B" `shouldParse`
        SignatureConstDec (ConstDec "unary" (TLambda (Lambda (Just $ typeArgs' [TSubtype "A" (TGeneric "Array" $ typeArgs' [TUnknown]), TMisc "B"]) [(Required, Normal, TLambda (Lambda Nothing [(Required, Rest, TMisc "A")] (TMisc "B")))] (TLambda (Lambda Nothing [(Required, Normal, TMisc "A")] (TMisc "B"))))))

      parse' signature "export interface Some<A> { readonly _tag: 'Some', readonly value: A }" `shouldParse`
        SignatureInterface (Interface "Some" (Just $ typeArgs' [TMisc "A"]) Nothing (ObjectLit [ObjectPair Immut Required ("_tag", TString "Some"), ObjectPair Immut Required ("value", TMisc "A")]))
