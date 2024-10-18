# ✍️  Aprox

A class for approximate equality.

```haskell
type Approx :: Type -> Constraint
class Approx x where
  (~=) :: x -> x -> Bool
```

## What?

This package exposes `Approx`: a class describing approximate equality.
Approximate equality respects the reflexivity and symmetry rules of `Eq`, but
not transitivity or extensionality. The class has been implemented for basic
types, and a `Generically` instance has been provided to allow the class to be
derived for more complex ADTs.
