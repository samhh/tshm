# Changelog

## 0.2.0 -- _Unreleased_

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

Additionally, overloaded functions and trailing commas are now supported, and various inconsistencies around the application of parentheses have been addressed.

## 0.1.0 -- 2021-01-02

This is tshm's initial release!

As of 0.1.0, tshm can take various formats of TypeScript declarations as input and output HM-style type signatures. Support is good but imperfect.

