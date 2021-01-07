module TSHM.PrinterSpec (spec) where

import           Prelude
import           TSHM.Parser         (parseSignature)
import           TSHM.Printer        (PrintConfig (PrintConfig), printSignature)
import           Test.Hspec
import           Test.Hspec.Hedgehog (PropertyT, (===))
import           Text.Megaparsec     (ParseErrorBundle)

unlines' :: [String] -> String
unlines' = intercalate "\n"

ppWith :: Maybe String -> Bool -> String -> Either (ParseErrorBundle String Void) String
ppWith x y = fmap (printSignature . (\sig -> PrintConfig sig x y)) . parseSignature

pp :: String -> Either (ParseErrorBundle String Void) String
pp = ppWith (Just "forall") True

(=*=) :: (Eq a, Eq e, Show a, Show e) => Either e a -> a -> PropertyT IO ()
a =*= b = a === Right b

spec :: Spec
spec = describe "TSHM.Printer" $ do
  it "prints specified universal quantification" $ do
    ppWith Nothing True "declare const f: <A>() => A" =*= "f :: () -> a"
    ppWith (Just "forall") True "declare const f: <A>() => A" =*= "f :: forall a. () -> a"
    ppWith (Just "∀") True "declare const f: <A>() => A" =*= "f :: ∀ a. () -> a"


  it "conditionally prints readonly modifier" $ do
    ppWith Nothing True  "declare const x: { readonly k: v }" =*= "x :: { readonly k: v }"
    ppWith Nothing False "declare const x: { readonly k: v }" =*= "x :: { k: v }"

    ppWith Nothing True  "declare const x: { k: readonly v }" =*= "x :: { k: readonly v }"
    ppWith Nothing False "declare const x: { k: readonly v }" =*= "x :: { k: v }"

    ppWith Nothing True  "declare const x: { k: v }" =*= "x :: { k: v }"
    ppWith Nothing False "declare const x: { k: v }" =*= "x :: { k: v }"

  it "conditionally prints readonly mapped type modifier" $ do
    ppWith Nothing True  "declare const x: { readonly [k in x]: v }" =*= "x :: { readonly [k in x]: v }"
    ppWith Nothing False "declare const x: { readonly [k in x]: v }" =*= "x :: { [k in x]: v }"

    ppWith Nothing True  "declare const x: { +readonly [k in x]: v }" =*= "x :: { readonly [k in x]: v }"
    ppWith Nothing False "declare const x: { +readonly [k in x]: v }" =*= "x :: { [k in x]: v }"

    ppWith Nothing True  "declare const x: { -readonly [k in x]: v }" =*= "x :: { -readonly [k in x]: v }"
    ppWith Nothing False "declare const x: { -readonly [k in x]: v }" =*= "x :: { [k in x]: v }"

  it "prints optionality mapped type modifier" $ do
    pp "declare const x: { [K in A]: B }" =*= "x :: { [K in A]: B }"
    pp "declare const x: { [K in A]?: B }" =*= "x :: { [K in A]?: B }"
    pp "declare const x: { [K in A]+?: B }" =*= "x :: { [K in A]?: B }"
    pp "declare const x: { [K in A]-?: B }" =*= "x :: { [K in A]-?: B }"

  it "prints mapped type as clause" $ do
    pp "type Ageless<A> = { [K in keyof A as Exclude<K, 'age'>]: A[K] }" =*=
      "type Ageless a = { [K in keyof a as Exclude K \"age\"]: a[K] }"

  it "prints type aliases" $ do
    pp "type X = string" =*= "type X = string"
    pp "type X<A> = string" =*= "type X a = string"
    pp "type X<A, B extends Array<A>> = string" =*= "type X a (b extends (Array a)) = string"
    pp "type X = () => string" =*= "type X = () -> string"

  it "prints interfaces" $ do
    pp "interface X { a: B }" =*= "type X = { a: B }"
    pp "interface X extends Y { a: B }" =*= "type X = { a: B } & Y"

  it "prints const declarations" $ do
    pp "declare const x: number" =*= "x :: number"
    pp "declare const f: (x: A) => (y: B) => C" =*= "f :: A -> B -> C"
    pp "declare const f: <A>(x: A) => [A, A]" =*= "f :: forall a. a -> [a, a]"

  it "prints function declarations" $ do
    pp "declare function f(x: A): (y: B) => C" =*= "f :: A -> B -> C"
    pp "declare function f<A>(x: A): [A, A]" =*= "f :: forall a. a -> [a, a]"

  it "prints overloaded function declarations" $ do
    pp (unlines'
      [ "export declare function f(x: A): A"
      , "export declare function f(x: B): B"
      ]) =*= unlines'
      [ "f :: A -> A"
      , "f :: B -> B"
      ]

  it "prints universal quantification and subtypes" $ do
    pp "type X = <A>(x: A) => <B>(y: B) => <C, D extends A, E extends Partial<A>>(c: [C, D, E]) => Either<E, C & D>" =*=
      "type X = forall a b c d e. d extends a, e extends (Partial a) => a -> b -> [c, d, e] -> Either e (c & d)"

  it "prints mapped types" $ do
    pp "type X<A> = { [K in A]: A[K] }" =*= "type X a = { [K in a]: a[K] }"

  it "correctly wraps generics, expressions, object references, and function arguments in parentheses" $ do
    pp "type X = <E, A>(x: Either<E, Option<A | E>>) => <B>(f: (x: A) => B) => (x: A | B) => (x: A['k'], y: F<A>['k']) => Option<B>" =*=
      "type X = forall e a b. Either e (Option (a | e)) -> (a -> b) -> a | b -> (a[\"k\"], (F a)[\"k\"]) -> Option b"

  it "prints stylised newtype-ts newtypes" $ do
    pp "export type X = Newtype<{ readonly Y: unique symbol }, Z>" =*=
      "newtype X = Z"

    pp "export interface X extends Newtype<{ readonly Y: unique symbol }, Z> {}" =*=
      "newtype X = Z"

  describe "prints real signatures from" $ do
    it "fp-ts/Array" $ do
      pp "export declare const zero: <A>() => A[]" =*=
        "zero :: forall a. () -> Array a"
      pp "export declare const map: <A, B>(f: (a: A) => B) => (fa: A[]) => B[]" =*=
        "map :: forall a b. (a -> b) -> Array a -> Array b"
      pp "export declare const mapWithIndex: <A, B>(f: (i: number, a: A) => B) => (fa: A[]) => B[]" =*=
        "mapWithIndex :: forall a b. ((number, a) -> b) -> Array a -> Array b"
      pp "export declare const chain: <A, B>(f: (a: A) => B[]) => (ma: A[]) => B[]" =*=
        "chain :: forall a b. (a -> Array b) -> Array a -> Array b"
      pp "export declare const bindTo: <N extends string>(name: N) => <A>(fa: A[]) => { [K in N]: A }[]" =*=
        "bindTo :: forall n a. n extends string => n -> Array a -> Array { [K in n]: a }"

    it "fp-ts/Either" $ do
      pp "export type Either<E, A> = Left<E> | Right<A>" =*=
        "type Either e a = Left e | Right a"
      pp "export declare const Do: Either<never, {}>" =*=
        "Do :: Either never {}"
      pp "export type Json = boolean | number | string | null | JsonArray | JsonRecord" =*=
        "type Json = boolean | number | string | null | JsonArray | JsonRecord"
      pp "export interface JsonArray extends ReadonlyArray<Json> {}" =*=
        "type JsonArray = {} & ReadonlyArray Json"

    it "fp-ts/function" $ do
      -- the actual pipe function at time of writing goes on until T, but that
      -- wouldn't prove anything extra
      pp (unlines'
        [ "export declare function pipe<A>(a: A): A"
        , "export declare function pipe<A, B>(a: A, ab: (a: A) => B): B"
        , "export declare function pipe<A, B, C>(a: A, ab: (a: A) => B, bc: (b: B) => C): C"
        , "export declare function pipe<A, B, C, D>(a: A, ab: (a: A) => B, bc: (b: B) => C, cd: (c: C) => D): D"
        , "export declare function pipe<A, B, C, D, E>(a: A, ab: (a: A) => B, bc: (b: B) => C, cd: (c: C) => D, de: (d: D) => E): E"
        , "export declare function pipe<A, B, C, D, E, F>("
        , "a: A,"
        , "ab: (a: A) => B,"
        , "bc: (b: B) => C,"
        , "cd: (c: C) => D,"
        , "de: (d: D) => E,"
        , "ef: (e: E) => F"
        , "): F"
        ]) =*= unlines'
        [ "pipe :: forall a. a -> a"
        , "pipe :: forall a b. (a, (a -> b)) -> b"
        , "pipe :: forall a b c. (a, (a -> b), (b -> c)) -> c"
        , "pipe :: forall a b c d. (a, (a -> b), (b -> c), (c -> d)) -> d"
        , "pipe :: forall a b c d e. (a, (a -> b), (b -> c), (c -> d), (d -> e)) -> e"
        , "pipe :: forall a b c d e f. (a, (a -> b), (b -> c), (c -> d), (d -> e), (e -> f)) -> f"
        ]

    it "fp-ts/Option" $ do
      pp "export type Option<A> = None | Some<A>" =*=
        "type Option a = None | Some a"
      pp (unlines'
        [ "export interface Some<A> {"
        , "  readonly _tag: 'Some'"
        , "  readonly value: A"
        , "}"
        ]) =*=
        "type Some a = { readonly _tag: \"Some\", readonly value: a }"

    it "fp-ts/Ord" $ do
      pp "export declare const Contravariant: Contravariant1<'Ord'>" =*=
        "Contravariant :: Contravariant1 \"Ord\""

      pp (unlines'
        [ "export interface Ord<A> extends Eq<A> {"
        , "  readonly compare: (x: A, y: A) => Ordering"
        , "}"
        ]) =*=
        "type Ord a = { readonly compare: (a, a) -> Ordering } & Eq a"

      pp "export declare function getMonoid<A = never>(): Monoid<Ord<A>>" =*=
        "getMonoid :: forall a. () -> Monoid (Ord a)"

      pp "export declare function fromCompare<A>(compare: (x: A, y: A) => Ordering): Ord<A>" =*=
        "fromCompare :: forall a. ((a, a) -> Ordering) -> Ord a"

      -- Requires: conditional types
      -- pp (unlines'
      --   [ "export declare function getTupleOrd<T extends ReadonlyArray<Ord<any>>>("
      --   , "  ...ords: T"
      --   , "): Ord<{ [K in keyof T]: T[K] extends Ord<infer A> ? A : never }>"
      --   ]) =*=
      --   "getTupleOrd :: forall t. t extends ReadonlyArray (Ord any) => ...t -> Ord [MAPPED]"

    it "fp-ts/TaskEither" $ do
      pp (unlines'
        [ "export declare const apW: <D, A>("
        , "  fa: TaskEither<D, A>"
        , ") => <E, B>(fab: TaskEither<E, (a: A) => B>) => TaskEither<D | E, B>"
        ]) =*=
        "apW :: forall d a e b. TaskEither d a -> TaskEither e (a -> b) -> TaskEither (d | e) b"

    it "fp-ts-std/Array" $ do
      pp "export declare const aperture: (n: number) => <A>(xs: A[]) => A[][]" =*=
        "aperture :: forall a. number -> Array a -> Array (Array a)"

      pp "export declare const countBy: <A>(f: (x: A) => string) => (xs: A[]) => Record<string, number>" =*=
        "countBy :: forall a. (a -> string) -> Array a -> Record string number"

      pp "export declare const dropRepeats: <A>(eq: Eq<A>) => Endomorphism<A[]>" =*=
        "dropRepeats :: forall a. Eq a -> Endomorphism (Array a)"

      pp "export declare const insertMany: (i: number) => <A>(xs: NonEmptyArray<A>) => (ys: A[]) => Option<NonEmptyArray<A>>" =*=
        "insertMany :: forall a. number -> NonEmptyArray a -> Array a -> Option (NonEmptyArray a)"

      pp "export declare const pluckFirst: <A>(p: Predicate<A>) => (xs: A[]) => [Option<A>, A[]]" =*=
        "pluckFirst :: forall a. Predicate a -> Array a -> [Option a, Array a]"

    it "fp-ts-std/Date" $ do
      pp "export type Milliseconds = Newtype<{ readonly Milliseconds: unique symbol }, number>" =*=
        "newtype Milliseconds = number"

      pp "export declare const fieldMilliseconds: Field<Milliseconds>" =*=
        "fieldMilliseconds :: Field Milliseconds"

      pp "export declare const isDate: Refinement<unknown, Date>" =*=
        "isDate :: Refinement unknown Date"

      pp "export declare const parseDate: (ts: string | number) => Option<Date>" =*=
        "parseDate :: string | number -> Option Date"

    it "fp-ts-std/Function" $ do
      pp "export declare const applyTo: <A>(x: A) => <B>(f: (x: A) => B) => B" =*=
        "applyTo :: forall a b. a -> (a -> b) -> b"

      pp "export declare const construct: <A extends unknown[], B>(x: new (...xs: A) => B) => (xs: A) => B" =*=
        "construct :: forall a b. a extends (Array unknown) => (...a -> b) -> a -> b"

      pp (unlines'
        [ "export declare const curry5: <A, B, C, D, E, F>("
        , "  f: (a: A, b: B, c: C, d: D, e: E) => F"
        , ") => (a: A) => (b: B) => (c: C) => (d: D) => (e: E) => F"
        ]) =*=
        "curry5 :: forall a b c d e f. ((a, b, c, d, e) -> f) -> a -> b -> c -> d -> e -> f"

      pp (unlines'
        [ "export declare const curry5T: <A, B, C, D, E, F>("
        , "  f: (xs: [A, B, C, D, E]) => F"
        , ") => (a: A) => (b: B) => (c: C) => (d: D) => (e: E) => F"
        ]) =*=
        "curry5T :: forall a b c d e f. ([a, b, c, d, e] -> f) -> a -> b -> c -> d -> e -> f"

      pp (unlines'
        [ "export declare const flip: <A extends unknown[], B extends unknown[], C>("
        , "  f: (...a: A) => (...b: B) => C"
        , ") => (...b: B) => (...a: A) => C"
        ]) =*=
        "flip :: forall a b c. a extends (Array unknown), b extends (Array unknown) => (...a -> ...b -> c) -> ...b -> ...a -> c"

      pp (unlines'
        [ "export declare const guard: <A, B>("
        , "  branches: [Predicate<A>, (x: A) => B][]"
        , ") => (fallback: (x: A) => B) => (input: A) => B"
        ]) =*=
        "guard :: forall a b. Array [(Predicate a), a -> b] -> (a -> b) -> a -> b"

      pp (unlines'
        [ "export declare const withIndex: <A, B, C>("
        , "  f: (g: (x: A) => B) => (ys: A[]) => C[]"
        , ") => (g: (i: number) => (x: A) => B) => (ys: A[]) => C[]"
        ]) =*=
        "withIndex :: forall a b c. ((a -> b) -> Array a -> Array c) -> (number -> a -> b) -> Array a -> Array c"

    it "fp-ts-std/IO" $ do
      pp "export declare const tap: <A>(f: (x: A) => IO<void>) => (x: A) => IO<A>" =*=
        "tap :: forall a. (a -> IO void) -> a -> IO a"

    it "fp-ts-std/JSON" $ do
      pp "export declare const parse: <E>(f: (e: SyntaxError) => E) => (x: string) => Either<E, unknown>" =*=
        "parse :: forall e. (SyntaxError -> e) -> string -> Either e unknown"

      pp "export declare const stringifyPrimitive: (x: string | number | boolean) => JSONString" =*=
        "stringifyPrimitive :: string | number | boolean -> JSONString"

    it "fp-ts-std/Record" $ do
      pp "export declare const invertAll: <A>(f: (x: A) => string) => (x: Record<string, A>) => Record<string, string[]>" =*=
        "invertAll :: forall a. (a -> string) -> Record string a -> Record string (Array string)"

      pp "export declare const merge: <A>(x: A) => <B>(y: B) => A & B" =*=
        "merge :: forall a b. a -> b -> a & b"

      pp (unlines'
        [ "export declare const omit: <K extends string>("
        , "  ks: K[]"
        , ") => <V, A extends Record<K, V>>(x: Partial<A>) => Pick<A, Exclude<keyof A, K>>"
        ]) =*=
        "omit :: forall k v a. k extends string, a extends (Record k v) => Array k -> Partial a -> Pick a (Exclude (keyof a) k)"

      pp "export declare const pick: <A>() => <K extends keyof A>(ks: K[]) => (x: A) => Pick<A, K>" =*=
        "pick :: forall a k. k extends (keyof a) => () -> Array k -> a -> Pick a k"

    it "fp-ts-std/String" $ do
      pp "export declare const empty: ''" =*=
        "empty :: \"\""

      pp "export declare const matchAll: (r: RegExp) => (x: string) => Option<NonEmptyArray<RegExpMatchArray>>" =*=
        "matchAll :: RegExp -> string -> Option (NonEmptyArray RegExpMatchArray)"

    it "fp-ts-std/Task" $ do
      pp "export declare const elapsed: (f: (n: Milliseconds) => IO<void>) => <A>(x: Task<A>) => Task<A>" =*=
        "elapsed :: forall a. (Milliseconds -> IO void) -> Task a -> Task a"

    it "fp-ts-std/URL" $ do
      pp "export declare const isURL: Refinement<unknown, URL>" =*=
        "isURL :: Refinement unknown URL"

    it "fp-ts-std/URLSearchParams" $ do
      pp "export declare const fromTuples: (x: [string, string][]) => URLSearchParams" =*=
        "fromTuples :: Array [string, string] -> URLSearchParams"

