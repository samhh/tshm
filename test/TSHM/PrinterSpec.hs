module TSHM.PrinterSpec (spec) where

import           Prelude
import           TSHM.Parser         (parseSignature)
import           TSHM.Printer        (printSignature)
import           Test.Hspec
import           Test.Hspec.Hedgehog (PropertyT, (===))
import           Text.Megaparsec     (ParseErrorBundle)

pp :: String -> Either (ParseErrorBundle String Void) String
pp = fmap printSignature . parseSignature

(=*=) :: (Eq a, Eq e, Show a, Show e) => Either e a -> a -> PropertyT IO ()
a =*= b = a === Right b

spec :: Spec
spec = describe "TSHM.Printer" $ do
  it "prints type aliases" $ do
    pp "type X = string" =*= "type X = string"
    pp "type X<A> = string" =*= "type X a = string"
    pp "type X<A, B extends Array<A>> = string" =*= "type X a (b extends Array a) = string"
    pp "type X = () => string" =*= "type X = () -> string"

  it "prints interfaces" $ do
    pp "interface X { a: B }" =*= "type X = { a: b }"
    -- TODO should only lowercase known type args i.e. B and Y should remain uppercase
    pp "interface X { a: B } extends Y" =*= "type X = { a: b } & y"

  it "prints declarations" $ do
    pp "declare const f: (x: A) => (y: B) => C" =*= "f :: a -> b -> c"
    pp "declare const f: <A>(x: A) => [A, A]" =*= "f :: forall a. a -> [a, a]"

  it "prints universal quantification and subtypes" $ do
    pp "type X = <A>(x: A) => <B>(y: B) => <C, D extends A, E>(c: [C, D, E]) => Either<E, C & D>" =*=
      "type X = forall a b c d e. d extends a => a -> b -> [c, d, e] -> Either e (c & d)"

  it "correctly wraps generics, expressions, and function arguments in parentheses" $ do
    pp "type X = <E, A>(x: Either<E, Option<A | E>>) => <B>(f: (x: A) => B) => (x: A | B) => Option<B>" =*=
      "type X = forall e a b. Either e (Option (a | e)) -> (a -> b) -> a | b -> Option b"

  describe "prints real signatures from" $ do
    it "fp-ts/Option" $ do
      pp "export type Option<A> = None | Some<A>" =*=
        "type Option a = None | Some a"

    it "fp-ts/Either" $ do
      pp "export type Either<E, A> = Left<E> | Right<A>" =*=
        "type Either e a = Left e | Right a"
      pp "export declare const Do: Either<never, {}>" =*=
        "Do :: Either never {}"
      pp "export type Json = boolean | number | string | null | JsonArray | JsonRecord" =*=
        "type Json = boolean | number | string | null | JsonArray | JsonRecord"
      -- pp "export interface JsonArray extends ReadonlyArray<Json> {}" =*=
      --   "type JsonArray = {} & ReadonlyArray Json"

    it "fp-ts/Array" $ do
      pp "export declare const zero: <A>() => A[]" =*=
        "zero :: forall a. () -> Array a"
      pp "export declare const map: <A, B>(f: (a: A) => B) => (fa: A[]) => B[]" =*=
        "map :: forall a b. (a -> b) -> Array a -> Array b"
      pp "export declare const mapWithIndex: <A, B>(f: (i: number, a: A) => B) => (fa: A[]) => B[]" =*=
        "mapWithIndex :: forall a b. ((number, a) -> b) -> Array a -> Array b"
      pp "export declare const chain: <A, B>(f: (a: A) => B[]) => (ma: A[]) => B[]" =*=
        "chain :: forall a b. (a -> Array b) -> Array a -> Array b"

