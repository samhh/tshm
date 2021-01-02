# tshm

A parser and formatter for TypeScript declarations that outputs HM-style type signatures.

```
Usage: tshm [-f|--forall string] input
  A parser and formatter for TypeScript declarations that outputs HM-style type
  signatures.

Available options:
  -h,--help                Show this help text
  -f,--forall string       Specify a string to be used to express universal
                           quantification, for example "forall" or "∀". If set
                           to "none" or omitted, no universal quantification
                           will be displayed
```

Example:

```
$ tshm "export type Option<A> = None | Some<A>"
type Option a = None | Some a

$ tshm "export declare const invertAll: <A>(f: (x: A) => string) => (x: Record<string, A>) => Record<string, string[]>"
invertAll :: (a -> string) -> Record string a -> Record string (Array string)

$ tshm --forall ∀ "export declare const withIndex: <A, B, C>(
    f: (g: (x: A) => B) => (ys: A[]) => C[]
  ) => (g: (i: number) => (x: A) => B) => (ys: A[]) => C[]"
withIndex :: ∀ a b c. ((a -> b) -> Array a -> Array c) -> (number -> a -> b) -> Array a -> Array c
```

Should an invalid input be provided the program will fail with the appropriate exit code, enabling the use of tshm in shell pipelines.

Messages are always printed upon failure. Should the failure be due to a parser error, the raw error is printed to the console to assist in debugging.

## What's not yet supported?

This is not an exhaustive list!

### Input

The parser is not intended to be a perfect parser of TypeScript syntax, rather merely able to support most normal use cases. It may be a little strict with whitespace in some areas, and a little lax about correctness in others.

- Overloaded functions
- Enums
- Object keys that aren't ordinary identifiers e.g. index signatures
- Mapped types
- Object property accessors that aren't string literals
- Trailing commas

### Output

There is an open question as to how "Haskell-ified" the output should be.

- Generics aren't given clarifying parentheses in the presence of object reference types e.g. `F<A>['k']`
- Generics aren't given clarifying parentheses in subtypes e.g. `B extends Array<A>`
- Keywords aren't given clarifying parentheses in generics e.g. `A<keyof B>`
- Generics are needlessly parenthesised in tuples
- Default type arguments
- Output never utilises newlines
- `readonly` modifier isn't output

