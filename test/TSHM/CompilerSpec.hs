module TSHM.CompilerSpec (spec) where

import qualified Data.Text           as T
import           Prelude
import           TSHM.Parser         (parseDeclaration)
import           TSHM.Compiler        (CompileConfig (CompileConfig),
                                      compileDeclaration)
import           TSHM.Reconciler     (reconcile)
import           Test.Hspec
import           Text.Megaparsec     (ParseErrorBundle)

unlines' :: [Text] -> Text
unlines' = T.intercalate "\n"

ppWith :: Maybe Text -> Bool -> Text -> Either (ParseErrorBundle Text Void) Text
ppWith x y = fmap (compileDeclaration . (\ast -> CompileConfig ast x y) . reconcile) . parseDeclaration

pp :: Text -> Either (ParseErrorBundle Text Void) Text
pp = ppWith (Just "forall") True

(=*=) :: (Eq a, Eq e, Show a, Show e) => Either e a -> a -> IO ()
a =*= b = a `shouldBe` Right b

spec :: Spec
spec = describe "TSHM.Compiler" $ do
  it "compiles specified universal quantification" $ do
    ppWith Nothing True "export declare const f: <A>() => A" =*= "f :: () -> a"
    ppWith (Just "forall") True "export declare const f: <A>() => A" =*= "f :: forall a. () -> a"
    ppWith (Just "∀") True "export declare const f: <A>() => A" =*= "f :: ∀ a. () -> a"


  it "conditionally compiles readonly modifier" $ do
    ppWith Nothing True  "export declare const x: { readonly k: v }" =*= "x :: { readonly k: v }"
    ppWith Nothing False "export declare const x: { readonly k: v }" =*= "x :: { k: v }"

    ppWith Nothing True  "export declare const x: { k: readonly v }" =*= "x :: { k: readonly v }"
    ppWith Nothing False "export declare const x: { k: readonly v }" =*= "x :: { k: v }"

    ppWith Nothing True  "export declare const x: { k: v }" =*= "x :: { k: v }"
    ppWith Nothing False "export declare const x: { k: v }" =*= "x :: { k: v }"

  it "conditionally compiles readonly mapped type modifier" $ do
    ppWith Nothing True  "export declare const x: { readonly [k in x]: v }" =*= "x :: { readonly [k in x]: v }"
    ppWith Nothing False "export declare const x: { readonly [k in x]: v }" =*= "x :: { [k in x]: v }"

    ppWith Nothing True  "export declare const x: { +readonly [k in x]: v }" =*= "x :: { readonly [k in x]: v }"
    ppWith Nothing False "export declare const x: { +readonly [k in x]: v }" =*= "x :: { [k in x]: v }"

    ppWith Nothing True  "export declare const x: { -readonly [k in x]: v }" =*= "x :: { -readonly [k in x]: v }"
    ppWith Nothing False "export declare const x: { -readonly [k in x]: v }" =*= "x :: { [k in x]: v }"

  it "compiles optionality mapped type modifier" $ do
    pp "export declare const x: { [K in A]: B }" =*= "x :: { [k in A]: B }"
    pp "export declare const x: { [K in A]?: B }" =*= "x :: { [k in A]?: B }"
    pp "export declare const x: { [K in A]+?: B }" =*= "x :: { [k in A]?: B }"
    pp "export declare const x: { [K in A]-?: B }" =*= "x :: { [k in A]-?: B }"

  it "compiles mapped type as clause" $ do
    pp "export type Ageless<A> = { [K in keyof A as Exclude<K, 'age'>]: A[K] }" =*=
      "type Ageless a = { [k in keyof a as Exclude k \"age\"]: a[k] }"

  it "compiles template literals" $ do
    pp "export type X = `${Quantity | Color} fish`" =*=
      "type X = `${Quantity | Color} fish`"

    pp (unlines'
      [ "export type Evt<A> = {"
      , "  on(evt: `${string & keyof A}Changed`, cb: () => void): void"
      , "}"
      ]) =*= "type Evt a = { on: (`${string & keyof a}Changed`, (() -> void)) -> void }"

  it "compiles dot access" $ do
    pp "export type X = { k: MyEnum.Member }" =*= "type X = { k: MyEnum.Member }"

  it "compiles type aliases" $ do
    pp "export type X = string" =*= "type X = string"
    pp "export type X<A> = string" =*= "type X a = string"
    pp "export type X<A, B extends Array<A>> = string" =*= "type X a (b extends (Array a)) = string"
    pp "export type X = () => string" =*= "type X = () -> string"

  it "compiles interfaces" $ do
    pp "export interface X { a: B }" =*= "type X = { a: B }"
    pp "export interface X extends Y { a: B }" =*= "type X = Y & { a: B }"

  it "compiles import declarations" $ do
    pp "import x from 'y'" =*= "import \"y\" (default as x)"
    pp "import type x from 'y'" =*= "import \"y\" (default as x)"
    pp "import { x, y } from 'z'" =*= "import \"z\" (x, y)"
    pp "import type { x, y } from 'z'" =*= "import \"z\" (x, y)"
    pp "import def, { x, y } from 'z'" =*= "import \"z\" (default as def, x, y)"
    pp "import type def, { x, y } from 'z'" =*= "import \"z\" (default as def, x, y)"

  it "compiles export declarations" $ do
    pp "declare const x: 'x'; export default x" =*= "default :: \"x\""
    pp "export default x; declare const x: 'x'" =*= "default :: \"x\""
    pp "export default x; export declare const x: 'x'" =*= unlines'
      [ "default :: \"x\""
      , ""
      , "x :: \"x\""
      ]
    pp "export {}" =*= ""
    pp "export { x, y }" =*= ""
    pp "declare const x: number; export { x }" =*= "x :: number"
    pp "export { x }; declare const x: number" =*= "x :: number"
    pp "export { x as y }" =*= ""
    pp "declare const x: number; export { x as y }" =*= "y :: number"
    pp "export { x as y }; declare const x: number" =*= "y :: number"
    pp "export { x as y }; export declare const x: number" =*= unlines'
      [ "y :: number"
      , ""
      , "x :: number"
      ]

  it "doesn't compile non-exported const declarations" $ do
    pp "declare const x: number" =*= ""

  it "compiles const declarations" $ do
    pp "export declare const x: number" =*= "x :: number"
    pp "export declare const f: (x: A) => (y: B) => C" =*= "f :: A -> B -> C"
    pp "export declare const f: <A>(x: A) => [A, A]" =*= "f :: forall a. a -> [a, a]"

  it "doesn't compile non-exported function declarations" $ do
    pp "declare function f(x: A): (y: B) => C" =*= ""

  it "compiles exported function declarations" $ do
    pp "export declare function f(x: A): (y: B) => C" =*= "f :: A -> B -> C"
    pp "export declare function f<A>(x: A): [A, A]" =*= "f :: forall a. a -> [a, a]"

  it "compiles grouped overloaded function declarations" $ do
    pp (unlines'
      [ "export declare function f(x: A): A"
      , "export declare function f(x: B): B"
      ]) =*= unlines'
      [ "f :: A -> A"
      , "f :: B -> B"
      ]

  it "compiles ungrouped overloaded function declarations" $ do
    pp (unlines'
      [ "export declare function f(x: A): A"
      , "type Irrelevant = Irrelevant"
      , "export declare function g(x: C): C"
      , "type Irrelevant = Irrelevant"
      , "export declare function f(x: B): B"
      ]) =*= unlines'
      [ "f :: A -> A"
      , "f :: B -> B"
      , ""
      , "g :: C -> C"
      ]

  it "compiles default exported overloaded function declarations" $ do
    pp (unlines'
      [ "declare function f(x: A): A"
      , "type Irrelevant = Irrelevant"
      , "export declare function g(x: C): C"
      , "type Irrelevant = Irrelevant"
      , "export default f"
      , "declare function f(x: B): B"
      ]) =*= unlines'
      [ "g :: C -> C"
      , ""
      , "default :: A -> A"
      , "default :: B -> B"
      ]

  it "compiles enums" $ do
    pp "export enum X {}" =*= "enum X { }"
    pp "export enum X { A = 0, B, 'C' = '1' }" =*= "enum X { A = 0, B, \"C\" = \"1\" }"
    pp "export const enum X { A = 0, B, 'C' = '1' }" =*= "enum X { A = 0, B, \"C\" = \"1\" }"
    pp "export declare enum X { A = 0, B, 'C' = '1' }" =*= "enum X { A = 0, B, \"C\" = \"1\" }"
    pp "export declare const enum X { A = 0, B, 'C' = '1' }" =*= "enum X { A = 0, B, \"C\" = \"1\" }"

  it "compiles universal quantification and subtypes" $ do
    pp "export type X = <A>(x: A) => <B>(y: B) => <C, D extends A, E extends Partial<A>>(c: [C, D, E]) => Either<E, C & D>" =*=
      "type X = forall a b c d e. d extends a, e extends (Partial a) => a -> b -> [c, d, e] -> Either e (c & d)"

  it "compiles mapped types" $ do
    pp "export type X<A> = { [K in A]: A[K] }" =*= "type X a = { [k in a]: a[k] }"

  it "compiles different object key types" $ do
    pp "export type X = { a: a, 'b': b, 3.3: c, ['d']: d, [e]: e, [f: number]: f }" =*=
      "type X = { a: a, \"b\": b, 3.3: c, [\"d\"]: d, [e]: e, [index: number]: f }"

  it "correctly wraps generics, expressions, object references, and function arguments in parentheses" $ do
    pp "export type X = <E, A>(x: Either<E, Option<A | E>>) => <B>(f: (x: A) => B) => (x: A | B) => (x: A['k'], y: F<A>['k']) => Option<B>" =*=
      "type X = forall e a b. Either e (Option (a | e)) -> (a -> b) -> a | b -> (a[\"k\"], (F a)[\"k\"]) -> Option b"

  it "compiles stylised newtype-ts newtypes" $ do
    pp "export type X = Newtype<{ readonly Y: unique symbol }, Z>" =*=
      "newtype X = Z"

    pp "export interface X extends Newtype<{ readonly Y: unique symbol }, Z> {}" =*=
      "newtype X = Z"

  it "compiles type of known referenced reflection type" $ do
    pp "export declare const f: (x: { y: number; z: [string] }) => [typeof x, x]" =*=
      "f :: { y: number, z: [string] } -> [{ y: number, z: [string] }, x]"

  it "compiles most recently discovered reflection type" $ do
    pp "export declare const f: (x: number) => (x: string) => typeof x" =*=
      "f :: number -> string -> string"

  it "compiles dumb typeof for unknown referenced reflection type" $ do
    pp "export declare const f: typeof x" =*=
      "f :: typeof x"

  describe "compiles real signatures from" $ do
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
        "bindTo :: forall n a. n extends string => n -> Array a -> Array { [k in n]: a }"

    it "fp-ts/Either" $ do
      pp "export type Either<E, A> = Left<E> | Right<A>" =*=
        "type Either e a = Left e | Right a"
      pp "export declare const Do: Either<never, {}>" =*=
        "Do :: Either never {}"
      pp "export type Json = boolean | number | string | null | JsonArray | JsonRecord" =*=
        "type Json = boolean | number | string | null | JsonArray | JsonRecord"
      pp "export interface JsonArray extends ReadonlyArray<Json> {}" =*=
        "type JsonArray = ReadonlyArray Json & {}"

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
        "type Ord a = Eq a & { readonly compare: (a, a) -> Ordering }"

      pp "export declare function getMonoid<A = never>(): Monoid<Ord<A>>" =*=
        "getMonoid :: forall a. () -> Monoid (Ord a)"

      pp "export declare function fromCompare<A>(compare: (x: A, y: A) => Ordering): Ord<A>" =*=
        "fromCompare :: forall a. ((a, a) -> Ordering) -> Ord a"

      pp (unlines'
        [ "export declare function getTupleOrd<T extends ReadonlyArray<Ord<any>>>("
        , "  ...ords: T"
        , "): Ord<{ [K in keyof T]: T[K] extends Ord<infer A> ? A : never }>"
        ]) =*=
        "getTupleOrd :: forall t. t extends (ReadonlyArray (Ord any)) => ...t -> Ord { [k in (keyof t)]: t[k] extends (Ord (infer a)) ? a : never }"

    it "fp-ts/TaskEither" $ do
      pp (unlines'
        [ "export declare const apW: <D, A>("
        , "  fa: TaskEither<D, A>"
        , ") => <E, B>(fab: TaskEither<E, (a: A) => B>) => TaskEither<D | E, B>"
        ]) =*=
        "apW :: forall d a e b. TaskEither d a -> TaskEither e (a -> b) -> TaskEither (d | e) b"
      pp "export declare type URI = typeof URI" =*=
        "type URI = typeof URI"

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

