# 💶 Enriched profunctors

A `Profunctor` hierarchy built on non-@Hask@ categories.

## What?

The standard `Profunctor` definition in `profunctors` looks like this:

```
type Profunctor :: (Type -> Type -> Type) -> Constraint
class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
```

However, I seem to keep running into problems for which `a -> b` is far too
general an arrow, and I want far more constraints on what this can be. This
library builds on the [constrained category][constrained-category] hierarchy to
provide an interface for profunctors over categories other than just `(->)`.

[constrained-category]: https://github.com/i-am-tom/haskell/tree/main/constrained-categories
