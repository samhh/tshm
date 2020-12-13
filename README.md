# tshm

A WIP parser for TypeScript declarations that seeks to output HM-style type signatures.

Example usage:

```
$ tshm "export declare const f: <A>(a: A) => (b: string) => A"
f :: a -> string -> a
```

Should an invalid input be provided the program will fail with the appropriate exit code, enabling the use of tshm in shell pipelines.

Messages are always printed upon failure. Should the failure be due to a parser error, the raw error is printed to the console to assist in debugging.

## What's not yet supported?

This is not an exhaustive list!

### Input

Whilst the parser is currently very strict about whitespace/similar (see below), it's a little loose in some other areas. It is not intended to be a perfect parser of TypeScript syntax, rather merely able to support most normal use cases.

- Syntactic parentheses e.g. `(() => void)[]`
- Nested special array syntax e.g. `string[][]`
- Nested object type references e.g. `MyType['prop1']['prop2']`
- Object literal keys that aren't ordinary static strings
- Irregular spacing
- Newlines
- Semicolons

### Output

There is an open question as to how "Haskell-ified" the output should be.

- No parentheses around top-level higher-kinded types e.g. `(Either e (Option a))` -> `Either e (Option a)`

