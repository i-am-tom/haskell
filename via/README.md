# `Via`

A type to allow deriving values via arbitrary isomorphisms.

## What?

I wanted to use `DerivingVia` to derive something via a `From` instance (from
the `witch` package). I assumed a type would exist for this, so I googled and
found [a Tweag project][iso-deriving]. However, it only allows you to derive
via one kind of isomorphism that has to be fixed at the declaration site. This
package is the same thing but more general.

## How?

Isomorphisms (or, in some cases, one way projections/injections) are defined
according to some constraint. For example, the isomorphism when two types are
constrained by `(~)` is `id`. When constrained by `Coercible`, it's `coerce`.
The `In` and `Out` classes allow you to define how a mapping between types
should be defined for any given class.

Then, `(x + c)` is a type constructor that we can use to derive via `x` using
the `c` constraint. For example:

```haskell
type Url :: Type
data Url = Url { .. }
  deriving stock (Eq, Generic, Ord)
  deriving Show via (String + From) Url
```

Here, we want to derive a `Show` instance via the `String` type, but our `Url`
type is not representationally equal to `String`. However, we declare that a
`From Url String` instance exists, and this type allows us to derive the
instance _via_ that relationship.

[iso-deriving]: https://www.tweag.io/blog/2020-04-23-deriving-isomorphically/
