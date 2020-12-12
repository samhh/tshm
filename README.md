# tshm

A WIP parser for TypeScript declarations that seeks to output HM-style type signatures.

The CLI interface isn't implemented yet, but the parser and formatter can be tested via the REPL.

Example input and output:

```typescript
export declare const f: <A>(a: A) => (b: string) => A
```

```haskell
f :: A -> string -> A
```

## What's not yet supported?

This is not an exhaustive list!

### Input

- Tuple types
- Parameters and return types which are generics with two or more type arguments
- Intersection and union operators
- Syntactic parentheses e.g. `(() => void)[]`
- Nested special array syntax e.g. `string[][]`
- Object literal keys that aren't ordinary static strings
- Irregular spacing
- Newlines
- Semicolons

### Output

- HM-style higher-kinded types output e.g. `Option<string>` -> `Option string`
- Dedicated output for `extends` clauses
- Dedicated array syntax e.g. `string[]` or `Array<string>` -> `[string]`
- Lowercase single-char type arguments e.g. `A -> B` -> `a -> b`

