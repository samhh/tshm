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
  describe "pType" $ do
    it "parses void" $ do
      parse' pType "void" `shouldParse` TsTypeVoid

    it "parses primitive" $ do
      parse' pType "A" `shouldParse` TsTypePrimitive "A"
      parse' pType "string" `shouldParse` TsTypePrimitive "string"
      parse' pType "A & B" `shouldParse` TsTypePrimitive "A & B"
      parse' pType "A | B" `shouldParse` TsTypePrimitive "A | B"

    it "parses string literal" $ do
      parse' pType "'abc'" `shouldParse` TsTypeStringLiteral "abc"

    it "parses function" $ do
      parse' pType "<A, B>(a: A) => B" `shouldParse`
         TsTypeFunction (Function (Just [TsTypePrimitive "A", TsTypePrimitive "B"]) [TsTypePrimitive "A"] (TsTypePrimitive "B"))

  describe "pFunction" $ do
    it "parses minimal viable function" $ do
      parse' pFunction "() => void" `shouldParse` Function Nothing [] TsTypeVoid

    it "parses type arguments" $ do
      parse' pFunction "<A, Array<B>>() => void" `shouldParse`
        Function (Just [TsTypePrimitive "A", TsTypeGeneric "Array" [TsTypePrimitive "B"]]) [] TsTypeVoid

    it "parses params" $ do
      parse' pFunction "(x: number, y: string) => void" `shouldParse`
        Function Nothing [TsTypePrimitive "number", TsTypePrimitive "string"] TsTypeVoid

    it "parses return type" $ do
      parse' pFunction "() => number" `shouldParse` Function Nothing [] (TsTypePrimitive "number")

    it "parses curried function" $ do
      parse' pFunction "() => () => void" `shouldParse` Function Nothing [] (TsTypeFunction (Function Nothing [] TsTypeVoid))

    it "parses complex function" $ do
      parse' pFunction "<A, Array<Option<B>>>(x: number, y: <C>(z: C) => () => C) => () => void" `shouldParse`
        Function
          ( Just
            [ TsTypePrimitive "A"
            , TsTypeGeneric "Array"
              [ TsTypeGeneric "Option"
                [ TsTypePrimitive "B"
                ]
              ]
            ]
          )
          [ TsTypePrimitive "number"
          , TsTypeFunction (Function
            (Just [TsTypePrimitive "C"])
            [TsTypePrimitive "C"]
            (TsTypeFunction (Function Nothing [] (TsTypePrimitive "C"))))
          ]
          ( TsTypeFunction (Function Nothing [] TsTypeVoid))

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
        [ TsTypePrimitive "A"
        ]

    it "parses multiple flat type arguments" $ do
      parse' pTypeArgs "<A, B, C>" `shouldParse`
        [ TsTypePrimitive "A"
        , TsTypePrimitive "B"
        , TsTypePrimitive "C"
        ]

    it "parses single nested type argument" $ do
      parse' pTypeArgs "<Array<Option<A>>>" `shouldParse`
        [ TsTypeGeneric "Array"
          [ TsTypeGeneric "Option"
            [ TsTypePrimitive "A"
            ]
          ]
        ]

    it "parses mixed type arguments" $ do
      parse' pTypeArgs "<A, Array<Option<B>, C>, Either<E, A>>" `shouldParse`
        [ TsTypePrimitive "A"
        , TsTypeGeneric "Array"
          [ TsTypeGeneric "Option"
            [ TsTypePrimitive "B"
            ]
          , TsTypePrimitive "C"
          ]
        , TsTypeGeneric "Either"
          [ TsTypePrimitive "E"
          , TsTypePrimitive "A"
          ]
        ]

    it "requires at least one type argument" $ do
      parse' pTypeArgs `shouldFailOn` "<>"

  describe "pParams" $ do
    it "parses empty params" $ do
      parse' pParams "()" `shouldParse` []

    it "parses single param" $ do
      parse' pParams "(a: number)" `shouldParse` [ TsTypePrimitive "number"]

    it "parses multiple params" $ do
      parse' pParams "(a: number, b: <A>(x: string) => void, c: Array<boolean>)" `shouldParse`
        [ TsTypePrimitive "number"
        , TsTypeFunction (Function (Just [TsTypePrimitive "A"]) [TsTypePrimitive "string"] TsTypeVoid)
        , TsTypeGeneric "Array" [TsTypePrimitive "boolean"]
        ]

  describe "pReturn" $ do
    it "parses value after the lambda" $ do
      parse' pReturn " => void" `shouldParse` TsTypeVoid
      parse' pReturn " => string" `shouldParse` TsTypePrimitive "string"
      parse' pReturn " => <A>(x: A) => A" `shouldParse` TsTypeFunction (Function (Just [TsTypePrimitive "A"]) [TsTypePrimitive "A"] (TsTypePrimitive "A"))

    it "requires a value after the lambda" $ do
      parse' pReturn `shouldFailOn` " => "

  describe "pDeclaration" $ do
    it "parses an entire declaration" $ do
      parse' pDeclaration "export declare const empty: ''" `shouldParse` Declaration "empty" (TsTypeStringLiteral "")

      -- FAILS: requires nested []
      -- parse' pDeclaration "export declare const aperture: (n: number) => <A>(xs: A[]) => A[][]" `shouldParse`
      --   Declaration "aperture" (TsTypeFunction (Function Nothing [TsTypePrimitive "number"] (TsTypeFunction (Function (Just [TsTypePrimitive "A"]) [TsTypeGeneric "Array" [TsTypePrimitive "A"]] (TsTypeGeneric "Array" [TsTypeGeneric "Array" [TsTypePrimitive "A"]])))))

      parse' pDeclaration "export declare const anyPass: <A>(fs: Predicate<A>[]) => Predicate<A>" `shouldParse`
        Declaration "anyPass" (TsTypeFunction (Function (Just [TsTypePrimitive "A"]) [TsTypeGeneric "Array" [TsTypeGeneric "Predicate" [TsTypePrimitive "A"]]] (TsTypeGeneric "Predicate" [TsTypePrimitive "A"])))

      parse' pDeclaration "export declare const merge: <A>(x: A) => <B>(y: B) => A & B" `shouldParse`
        Declaration "merge" (TsTypeFunction (Function (Just [TsTypePrimitive "A"]) [TsTypePrimitive "A"] (TsTypeFunction (Function (Just [TsTypePrimitive "B"]) [TsTypePrimitive "B"] (TsTypePrimitive "A & B")))))

      -- FAILS
      -- parse' pDeclaration "export declare const omit: <K extends string>(ks: K[]) => <V, A extends Record<K, V>>(x: Partial<A>) => Pick<A, Exclude<keyof A, K>>" `shouldParse`
      --   Declaration "omit" (TsTypeFunction (Function (Just [TsTypePrimitive "K extends string"]) [TsTypePrimitive "K[]"] (TsTypeFunction (Function (Just [TsTypePrimitive "V", TsTypePrimitive "A extends Record<K, V>"]) [TsTypePrimitive "Partial<A>"] (TsTypePrimitive "Pick<A, Exclude<keyof A, K>>")))))

