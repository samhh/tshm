# Changelog

## 0.4.0 -- 2021-07-20

This release is the first to include a prebuilt binary via CI, on Linux only for now. It also adds support for the `-v`/`--version` switch.

Continuing along the CLI front, support has been added for the `-a`/`--all` flag, which overrides the scope semantics of declarations to publicise everything. This is designed with quick CLI evaluation in mind, removing the need to prefix everything with "export".

Code can now be evaluated via stdin as well as via the preexisting `-e`/`--eval` flag.

The parser continues to receive upgrades, most notably:

- Support import statements - default, named, and asterisk
- Support default exports and export lists
- Support overloaded functions that aren't direct siblings
- Print the underlying type as referenced by "typeof" where possible

Nota bene that empty inputs are now supported and will no longer return a failure exit code. This is deemed semantically appropriate insofar as an empty file or string is technically a valid TypeScript declaration.

## 0.3.0 -- 2021-01-09

This release includes a couple of breaking changes to the CLI interface.

What was previously the default argument (code input) is now intended to be passed to the `-e`/`--eval` flag. The default argument is now a filepath.

The other is that the `-r`/`--readonly` switch has been added to allow you to toggle the display of readonly modifiers. The breaking change is that this defaults to false, whereas they were always printed previously.

Onto the parser, the following syntax is now supported:

- Enums
- String literal, number literal, computed, and index signature object literal keys
- Mapped types
- Inferred types
- Conditional types
- Template literal types
- Function parameter name destructuring

Additionally, the parser is now much, much less strict about whitespace. In some places it's more permissive than TypeScript itself!

Finally, some inconsistencies in the identifier parser have been addressed.

## 0.2.0 -- 2021-01-04

This release adds support for the `-f`/`--forall` flag, which can be used to specify how to express universal quantification. A breaking change here is that tshm no longer renders universal quantification by default (this can be enforced with the special input "none"). Examples:

```
$ tshm "declare const f: <A>() => A"
f :: () -> a

$ tshm -f none "declare const f: <A>() => A"
f :: () -> a

$ tshm -f forall "declare const f: <A>() => A"
f :: forall a. () -> a

$ tshm -f ∀ "declare const f: <A>() => A"
f :: ∀ a. () -> a
```

Aside from that, the CLI has been generally improved, now providing useful help text.

Overloaded functions, non-literal object property accessors, and trailing commas are now supported, and various inconsistencies and ambiguities around the application of parentheses have been addressed.

Mapped types are now supported, however their utility is limited without support for conditional types, which is a priority for the next release.

## 0.1.0 -- 2021-01-02

This is tshm's initial release!

As of 0.1.0, tshm can take various formats of TypeScript declarations as input and output HM-style type signatures. Support is good but imperfect.

