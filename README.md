# tshm

A parser and formatter for TypeScript declarations that outputs HM-style type signatures.

Example usage:

```
$ tshm "export declare const f: <A>(a: A) => <E>(b: Either<string, Option<A>>) => A"
f :: a -> Either string (Option a) -> a

$ tshm "export type Option a = None | Some a"
type Option a = None | Some a
```

Should an invalid input be provided the program will fail with the appropriate exit code, enabling the use of tshm in shell pipelines.

Messages are always printed upon failure. Should the failure be due to a parser error, the raw error is printed to the console to assist in debugging.

## What's not yet supported?

This is not an exhaustive list!

### Input

Whilst the parser is currently very strict about whitespace/similar (see below), it's a little loose in some other areas. It is not intended to be a perfect parser of TypeScript syntax, rather merely able to support most normal use cases.

- Object keys that aren't ordinary static strings e.g. index signatures
- `unique symbol`
- `readonly` modifier in objects and tuples
- Dedicated parsing for newtype-ts
- Irregular spacing
- Newlines

### Output

- Generics aren't given clarifying parentheses in the presence of object reference types e.g. `F<A>['k']`
- Output never utilises newlines

There is an open question as to how "Haskell-ified" the output should be.

