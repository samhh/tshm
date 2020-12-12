module TSHM.ParserSpec (spec) where

import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Prelude
import           TSHM.Parser
import           TSHM.TypeScript
import           Test.Hspec
import           Test.Hspec.Hedgehog (PropertyT, forAll, hedgehog, (===))
import           Test.Hspec.Megaparsec
import           Text.Megaparsec     (ParseErrorBundle, Parsec, parse)

parse' :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse' = flip parse ""

(/=*) :: (Eq a, Show a) => (s -> Either e a) -> s -> PropertyT IO ()
f /=* a = let x = f a in first (const ()) x === Left ()

(=*=) :: (Eq a, Eq e, Show a, Show e) => Either e a -> a -> PropertyT IO ()
a =*= b = a === Right b

spec :: Spec
spec = describe "TSHM.Parser" $ do
  describe "pValue" $ do
    it "parses void" $ do
      parse' pValue "void" `shouldParse` ValueVoid

    it "parses primitive" $ do
      parse' pValue "A" `shouldParse` ValuePrimitive "A"
      parse' pValue "string" `shouldParse` ValuePrimitive "string"
      parse' pValue "A & B" `shouldParse` ValuePrimitive "A & B"
      parse' pValue "A | B" `shouldParse` ValuePrimitive "A | B"

    it "parses string literal" $ do
      parse' pValue "'abc'" `shouldParse` ValueStringLiteral "abc"

    it "parses function" $ do
      parse' pValue "<A, B>(a: A) => B" `shouldParse`
        ValueFunction (Function (Just [TypeArgPrimitive "A", TypeArgPrimitive "B"]) [ValuePrimitive "A"] (ValuePrimitive "B"))

  describe "pFunction" $ do
    it "parses minimal viable function" $ do
      parse' pFunction "() => void" `shouldParse` Function Nothing [] ValueVoid

    it "parses type arguments" $ do
      parse' pFunction "<A, Array<B>>() => void" `shouldParse`
        Function (Just [TypeArgPrimitive "A", TypeArgHigherOrder "Array" [TypeArgPrimitive "B"]]) [] ValueVoid

    it "parses params" $ do
      parse' pFunction "(x: number, y: string) => void" `shouldParse`
        Function Nothing [ValuePrimitive "number", ValuePrimitive "string"] ValueVoid

    it "parses return type" $ do
      parse' pFunction "() => number" `shouldParse` Function Nothing [] (ValuePrimitive "number")

    it "parses curried function" $ do
      parse' pFunction "() => () => void" `shouldParse` Function Nothing [] (ValueFunction (Function Nothing [] ValueVoid))

    it "parses complex function" $ do
      parse' pFunction "<A, Array<Option<B>>>(x: number, y: <C>(z: C) => () => C) => () => void" `shouldParse`
        Function
          ( Just
            [ TypeArgPrimitive "A"
            , TypeArgHigherOrder "Array"
              [ TypeArgHigherOrder "Option"
                [ TypeArgPrimitive "B"
                ]
              ]
            ]
          )
          [ ValuePrimitive "number"
          , ValueFunction (Function
            (Just [TypeArgPrimitive "C"])
            [ValuePrimitive "C"]
            (ValueFunction (Function Nothing [] (ValuePrimitive "C"))))
          ]
          ( ValueFunction (Function Nothing [] ValueVoid))

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
        [ TypeArgPrimitive "A"
        ]

    it "parses multiple flat type arguments" $ do
      parse' pTypeArgs "<A, B, C>" `shouldParse`
        [ TypeArgPrimitive "A"
        , TypeArgPrimitive "B"
        , TypeArgPrimitive "C"
        ]

    it "parses single nested type argument" $ do
      parse' pTypeArgs "<Array<Option<A>>>" `shouldParse`
        [ TypeArgHigherOrder "Array"
          [ TypeArgHigherOrder "Option"
            [ TypeArgPrimitive "A"
            ]
          ]
        ]

    it "parses mixed type arguments" $ do
      parse' pTypeArgs "<A, Array<Option<B>, C>, Either<E, A>>" `shouldParse`
        [ TypeArgPrimitive "A"
        , TypeArgHigherOrder "Array"
          [ TypeArgHigherOrder "Option"
            [ TypeArgPrimitive "B"
            ]
          , TypeArgPrimitive "C"
          ]
        , TypeArgHigherOrder "Either"
          [ TypeArgPrimitive "E"
          , TypeArgPrimitive "A"
          ]
        ]

    it "requires at least one type argument" $ do
      parse' pTypeArgs `shouldFailOn` "<>"

  describe "pParams" $ do
    it "parses empty params" $ do
      parse' pParams "()" `shouldParse` []

    it "parses single param" $ do
      parse' pParams "(a: number)" `shouldParse` [ ValuePrimitive "number"]

    it "parses multiple params" $ do
      parse' pParams "(a: number, b: <A>(x: string) => void, c: Array<boolean>)" `shouldParse`
        [ ValuePrimitive "number"
        , ValueFunction (Function (Just [TypeArgPrimitive "A"]) [ValuePrimitive "string"] ValueVoid)
        , ValuePrimitive "Array<boolean>"
        ]

  describe "pReturn" $ do
    it "parses value after the lambda" $ do
      parse' pReturn " => void" `shouldParse` ValueVoid
      parse' pReturn " => string" `shouldParse` ValuePrimitive "string"
      parse' pReturn " => <A>(x: A) => A" `shouldParse` ValueFunction (Function (Just [TypeArgPrimitive "A"]) [ValuePrimitive "A"] (ValuePrimitive "A"))

    it "requires a value after the lambda" $ do
      parse' pReturn `shouldFailOn` " => "

  describe "pDeclaration" $ do
    it "parses an entire declaration" $ do
      parse' pDeclaration "export declare const empty: ''" `shouldParse` Declaration "empty" (ValueStringLiteral "")

      parse' pDeclaration "export declare const aperture: (n: number) => <A>(xs: A[]) => A[][]" `shouldParse`
        Declaration "aperture" (ValueFunction (Function Nothing [ValuePrimitive "number"] (ValueFunction (Function (Just [TypeArgPrimitive "A"]) [ValuePrimitive "A[]"] (ValuePrimitive "A[][]")))))

      parse' pDeclaration "export declare const anyPass: <A>(fs: Predicate<A>[]) => Predicate<A>" `shouldParse`
        Declaration "anyPass" (ValueFunction (Function (Just [TypeArgPrimitive "A"]) [ValuePrimitive "Predicate<A>[]"] (ValuePrimitive "Predicate<A>")))

      parse' pDeclaration "export declare const merge: <A>(x: A) => <B>(y: B) => A & B" `shouldParse`
        Declaration "merge" (ValueFunction (Function (Just [TypeArgPrimitive "A"]) [ValuePrimitive "A"] (ValueFunction (Function (Just [TypeArgPrimitive "B"]) [ValuePrimitive "B"] (ValuePrimitive "A & B")))))

