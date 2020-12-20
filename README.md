# tshm

A parser and formatter for TypeScript declarations that outputs HM-style type signatures.

Example usage:

```
$ tshm "export declare const f: <A>(a: A) => <E>(b: Either<E, Option<A>>) => A"
f :: forall a e. a -> Either e (Option a) -> a

$ tshm "export type Option<A> = None | Some<A>"
type Option a = None | Some a
```

Check out the "printer" tests for some real-world examples.

Should an invalid input be provided the program will fail with the appropriate exit code, enabling the use of tshm in shell pipelines.

Messages are always printed upon failure. Should the failure be due to a parser error, the raw error is printed to the console to assist in debugging.

## What's not yet supported?

This is not an exhaustive list!

### Input

The parser is not intended to be a perfect parser of TypeScript syntax, rather merely able to support most normal use cases. It may be a little strict with whitespace in some areas, and a little lax about correctness in others.

- `function` keyword declarations
- Default type arguments
- Enums
- Object keys that aren't ordinary identifiers e.g. index signatures
- Mapped types
- Object property accessors that aren't string literals
- `unique symbol`
- Dedicated parsing for newtype-ts
- Trailing commas

### Output

There is an open question as to how "Haskell-ified" the output should be.

- Generics aren't given clarifying parentheses in the presence of object reference types e.g. `F<A>['k']`
- Generics aren't given clarifying parentheses in subtypes e.g. `B extends Array<A>`
- Keywords aren't given clarifying parentheses in generics e.g. `A<keyof B>`
- Generics are needlessly parenthesised in tuples
- Output never utilises newlines
- `readonly` modifier isn't output
- Toggleable universal quantification

