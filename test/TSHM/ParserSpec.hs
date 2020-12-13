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

parse' :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse' = flip parse ""

(/=*) :: (Eq a, Show a) => (s -> Either e a) -> s -> PropertyT IO ()
f /=* a = let x = f a in first (const ()) x === Left ()

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
      parse' pType "a[][]" `shouldParse` TsTypeGeneric "Array" (fromList [TsTypeGeneric "Array" $ fromList [TsTypeMisc "a"]])

    it "parses object reference" $ do
      parse' pType "A['key']" `shouldParse` TsTypeObjectReference (TsTypeMisc "A") "key"
      parse' pType "A[\"key\"]" `shouldParse` TsTypeObjectReference (TsTypeMisc "A") "key"
      parse' pType "true['k1']['k2']" `shouldParse` TsTypeObjectReference (TsTypeObjectReference (TsTypeBoolean True) "k1") "k2"

    it "postfix operators play nicely together" $ do
      parse' pType "a['k1']['k2'][][]['k3'][]" `shouldParse`
        TsTypeGeneric "Array" (fromList [TsTypeObjectReference (TsTypeGeneric "Array" (fromList [TsTypeGeneric "Array" (fromList [TsTypeObjectReference (TsTypeObjectReference (TsTypeMisc "a") "k1") "k2"])])) "k3"])

    it "parses function" $ do
      parse' pType "<A, B>(a: A) => B" `shouldParse`
         TsTypeFunction (Function (Just $ fromList [TsTypeMisc "A", TsTypeMisc "B"]) [Normal $ TsTypeMisc "A"] (TsTypeMisc "B"))

      parse' pType "(x: F<A, B>) => G<C, D>" `shouldParse`
         TsTypeFunction (Function Nothing [Normal $ TsTypeGeneric "F" $ fromList [TsTypeMisc "A", TsTypeMisc "B"]] (TsTypeGeneric "G" $ fromList [TsTypeMisc "C", TsTypeMisc "D"]))

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
        Function Nothing [Normal $ TsTypeMisc "number", Normal $ TsTypeMisc "string"] TsTypeVoid

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
          [ Normal $ TsTypeFunction (Function
              (Just $ fromList [TsTypeSubtype "C" (TsTypeMisc "number"), TsTypeMisc "D"])
              [Normal $ TsTypeStringLiteral "ciao"]
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
          [ Normal $ TsTypeMisc "number"
          , Normal $ TsTypeFunction (Function
              (Just $ fromList [TsTypeMisc "C"])
              [Normal $ TsTypeMisc "C"]
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

  describe "pName" $ do
    let ident = Gen.list (Range.linear 1 99) Gen.alpha

    it "parses const declaration" $ hedgehog $ do
      x <- forAll ident
      parse' pName ("declare const " <> x <> ": ") =*= x

    it "parses exported const declaration" $ hedgehog $ do
      x <- forAll ident
      parse' pName ("export declare const " <> x <> ": ") =*= x

    it "requires declaration" $ hedgehog $ do
      x <- forAll ident
      parse' pName /=* ("const " <> x <> ": ")

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
      parse' pParams "(a: number)" `shouldParse` [ Normal $ TsTypeMisc "number"]

    it "parses multiple params" $ do
      parse' pParams "(a: number, b: <A>(x: string) => void, c: Array<boolean>)" `shouldParse`
        [ Normal $ TsTypeMisc "number"
        , Normal $ TsTypeFunction (Function (Just $ fromList [TsTypeMisc "A"]) [Normal $ TsTypeMisc "string"] TsTypeVoid)
        , Normal $ TsTypeGeneric "Array" $ fromList [TsTypeMisc "boolean"]
        ]

    it "parses rest params" $ do
      parse' pParams "(...a: number, b: string, ...c: boolean)" `shouldParse`
        [ Rest $ TsTypeMisc "number"
        , Normal $ TsTypeMisc "string"
        , Rest $ TsTypeMisc "boolean"
        ]

  describe "pReturn" $ do
    it "parses value after the lambda" $ do
      parse' pReturn " => void" `shouldParse` TsTypeVoid
      parse' pReturn " => string" `shouldParse` TsTypeMisc "string"
      parse' pReturn " => <A>(x: A) => A" `shouldParse` TsTypeFunction (Function (Just $ fromList [TsTypeMisc "A"]) [Normal $ TsTypeMisc "A"] (TsTypeMisc "A"))

    it "requires a value after the lambda" $ do
      parse' pReturn `shouldFailOn` " => "

  describe "pDeclaration" $ do
    let p = parse' $ pDeclaration <* eof

    it "optionally supports semicolons" $ do
      p "declare const x: string" `shouldParse` Declaration "x" (TsTypeMisc "string")
      p "declare const x: string;" `shouldParse` Declaration "x" (TsTypeMisc "string")

    it "parses real declarations" $ do
      p "export declare const empty: ''" `shouldParse` Declaration "empty" (TsTypeStringLiteral "")

      p "export declare const aperture: (n: number) => <A>(xs: A[]) => A[][]" `shouldParse`
        Declaration "aperture" (TsTypeFunction (Function Nothing [Normal $ TsTypeMisc "number"] (TsTypeFunction (Function (Just $ fromList [TsTypeMisc "A"]) [Normal $ TsTypeGeneric "Array" $ fromList [TsTypeMisc "A"]] (TsTypeGeneric "Array" $ fromList [TsTypeGeneric "Array" $ fromList [TsTypeMisc "A"]])))))

      p "export declare const anyPass: <A>(fs: Predicate<A>[]) => Predicate<A>" `shouldParse`
        Declaration "anyPass" (TsTypeFunction (Function (Just $ fromList [TsTypeMisc "A"]) [Normal $ TsTypeGeneric "Array" $ fromList [TsTypeGeneric "Predicate" $ fromList [TsTypeMisc "A"]]] (TsTypeGeneric "Predicate" $ fromList [TsTypeMisc "A"])))

      p "export declare const merge: <A>(x: A) => <B>(y: B) => A & B" `shouldParse`
        Declaration "merge" (TsTypeFunction (Function (Just $ fromList [TsTypeMisc "A"]) [Normal $ TsTypeMisc "A"] (TsTypeFunction (Function (Just $ fromList [TsTypeMisc "B"]) [Normal $ TsTypeMisc "B"] (TsTypeExpression TsOperatorIntersection (TsTypeMisc "A") (TsTypeMisc "B"))))))

      p "export declare const omit: <K extends string>(ks: K[]) => <V, A extends Record<K, V>>(x: Partial<A>) => Pick<A, Exclude<keyof A, K>>" `shouldParse`
        Declaration "omit" (TsTypeFunction (Function (Just $ fromList [TsTypeSubtype "K" (TsTypeMisc "string")]) [Normal $ TsTypeGeneric "Array" $ fromList [TsTypeMisc "K"]] (TsTypeFunction (Function (Just $ fromList [TsTypeMisc "V", TsTypeSubtype "A" (TsTypeGeneric "Record" $ fromList [TsTypeMisc "K", TsTypeMisc "V"])]) [Normal $ TsTypeGeneric "Partial" $ fromList [TsTypeMisc "A"]] (TsTypeGeneric "Pick" $ fromList [TsTypeMisc "A", TsTypeGeneric "Exclude" $ fromList [TsTypeKeysOf (TsTypeMisc "A"), TsTypeMisc "K"]])))))

      p "export declare const unary: <A extends unknown[], B>(f: (...xs: A) => B) => (xs: A) => B" `shouldParse`
        Declaration "unary" (TsTypeFunction (Function (Just $ fromList [TsTypeSubtype "A" (TsTypeGeneric "Array" $ fromList [TsTypeMisc "unknown"]), TsTypeMisc "B"]) [Normal $ TsTypeFunction (Function Nothing [Rest $ TsTypeMisc "A"] (TsTypeMisc "B"))] (TsTypeFunction (Function Nothing [Normal $ TsTypeMisc "A"] (TsTypeMisc "B")))))

