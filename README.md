# tshm

A parser and formatter for TypeScript declarations that outputs HM-style type signatures.

```
Usage: tshm (filepath | (-e|--eval code)) [-f|--forall string]
            [-r|--readonly]
  A parser and formatter for TypeScript declarations that outputs HM-style type
  signatures.

Available options:
  -h,--help                Show this help text
  -v,--version             Output version
  filepath                 Read and evaluate the file at the provided path
  -e,--eval code           Evaluate input code directly
  -f,--forall string       Specify a string to be used to express universal
                           quantification, for example "forall" or "∀". If set
                           to "none" or omitted, no universal quantification
                           will be displayed
  -r,--readonly            Display readonly modifiers
```

Example:

```
$ tshm -e "export type Option<A> = None | Some<A>"
type Option a = None | Some a

$ tshm -e "export type Milliseconds = Newtype<{ readonly Milliseconds: unique symbol }, number>"
newtype Milliseconds = number

$ tshm -e "export declare const invertAll: <A>(f: (x: A) => string) => (x: Record<string, A>) => Record<string, string[]>"
invertAll :: (a -> string) -> Record string a -> Record string (Array string)

$ tshm --forall ∀ -e "export declare const withIndex: <A, B, C>(
    f: (g: (x: A) => B) => (ys: A[]) => C[]
  ) => (g: (i: number) => (x: A) => B) => (ys: A[]) => C[]"
withIndex :: ∀ a b c. ((a -> b) -> Array a -> Array c) -> (number -> a -> b) -> Array a -> Array c
```

Should an invalid input be provided the program will fail with the appropriate exit code, enabling the use of tshm in shell pipelines.

Messages are always printed upon failure. Should the failure be due to a parser error, the raw error is printed to the console to assist in debugging.

## What's supported?

At present, the parser can reliably parse a single input declaration. Within this declaration, subjectively speaking, the most common forms of TypeScript syntax are supported. Long-term, the intention is that all syntax deemed valid by the language proper will be supported.

The parser does not perform any sort of correctness checking beyond its ability to understand the syntax.

## Contributing

tshm is currently developed against GHC 8.10.4.

The codebase is split up into two parts, `lib/` and `src/`, representing the internal library and the CLI interface respectively.

The library handles parsing and printing. During parsing we parse the input into an AST, performing no correctness checking beyond what's needed to reliably parse any valid TypeScript declaration. Printing is essentially the parsing process in reverse, taking a syntactic token and outputting a string representing it. The library has a number of unit tests defined for each the parser and the printer.

