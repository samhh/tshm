# tshm

A parser and formatter for TypeScript declarations that outputs HM-style type signatures.

```
Usage: tshm (filepath | (-e|--eval code)) [-f|--forall string] [-r|--readonly]
  A parser and formatter for TypeScript declarations that outputs HM-style type
  signatures.

Available options:
  -h,--help                Show this help text
  -v,--version             Output version
  filepath                 Read and evaluate the file at the provided path
  -e,--eval code           Evaluate input code directly. Alternatively the code
                           can be supplied directly via stdin without this
                           option
  -f,--forall string       Specify a string to be used to express universal
                           quantification, for example "forall" or "∀". If set
                           to "none" or omitted, no universal quantification
                           will be displayed
  -r,--readonly            Display readonly modifiers
```

Example:

```
$ echo "export type Option<A> = None | Some<A>" | tshm
type Option a = None | Some a

$ echo "export type Milliseconds = Newtype<{ readonly Milliseconds: unique symbol }, number>" | tshm
newtype Milliseconds = number

$ echo "export declare const invertAll: <A>(f: (x: A) => string) => (x: Record<string, A>) => Record<string, string[]>" | tshm
invertAll :: (a -> string) -> Record string a -> Record string (Array string)

$ echo "export declare const withIndex: <A, B, C>(
    f: (g: (x: A) => B) => (ys: A[]) => C[]
  ) => (g: (i: number) => (x: A) => B) => (ys: A[]) => C[]" | tshm --forall ∀
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

The library handles everything from parsing to compilation. During parsing we parse the input into an initial AST, performing no correctness checking beyond what's needed to reliably parse any valid TypeScript declaration. Prior to compilation we perform a reconciliation step in which semantics that couldn't be handled by the parser are taken care of. Compiling is essentially the parsing process in reverse, taking a syntactic token and outputting a string representing it. The library has a number of unit tests defined for each the parser, reconciler, and compiler.

