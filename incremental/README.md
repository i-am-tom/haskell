# 🧬 Incremental

A library for describing type derivatives.

```haskell
type Change :: Type -> Constraint
class Monoid (Δ x) => Change x where
  type Δ x :: Type

  (<~) :: x -> Δ x -> x
  (\\) :: x -> x -> Δ x
```

## What?

This library provides a vocabulary for describing changes between values of a
given type. What makes it interesting is that it can describe changes over
functions as well as sums and products, which allows us to calculate derivaties
of entire programs.

## How?

The implementation of this library is thanks to two papers:

- [A Theory of Changes for Higher-Order Languages: Incrementalizing λ-Calculi by Static Differentiation - Yufei Cai, Paolo G. Giarrusso, Tillmann Rendel, Klaus Ostermann](https://arxiv.org/abs/1312.0658)
- [Compiling to Categories - Conal Elliott](http://conal.net/papers/compiling-to-categories/)
