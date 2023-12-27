# `An` existential type

A library for dealing with constrained existential types.

<!--
```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Data.An where

import Data.Hashable (Hashable (..))
import Data.Kind (Constraint, Type)
import Type.Reflection ((:~~:) (..), SomeTypeRep (..), Typeable, eqTypeRep, typeOf)
```
-->

## A practical API

### `A` definition

This library defines `An`, a type that contains an existential. `An` is indexed
by a constraint that this existential satisfies, which allows us to manipulate
the value without necessarily knowing its specific type.

To be cute, we also provide a type synonym, pattern synonym, and versions of
all the functions that refer to `A` instead of `An`, so you can write code that
sounds better read aloud.

```haskell
type An :: (Type -> Constraint) -> Type
data An c where An :: c x => x -> An c
```

```haskell
type A :: (Type -> Constraint) -> Type
type A = An
```

```haskell
pattern A :: forall c. forall x. c x => x -> An c
pattern A x = An x
{-# COMPLETE A #-}
```

I'm not entirely sure why I need two separate `forall`s in the pattern synonym;
if anyone could explain that to me, I'd be much obliged.

### `A` construction from folds

As well as the data constructor above, we can also construct `An` existential
using a rank-2 fold. This shape occasionally crops up when dealing with CPS.

```haskell
an :: forall c. (forall r. (forall x. c x => x -> r) -> r) -> A c
an k = k An
```

```haskell
a :: forall c. (forall r. (forall x. c x => x -> r) -> r) -> A c
a = an
```

### Ways to destruct `A` value

The inverse is also true: we can convert `An` existential into a rank-2 fold.
This also gives us a nice way to unpack existentials when we want to use them.

```haskell
foldA :: forall c. A c -> (forall r. (forall x. c x => x -> r) -> r)
foldA (An x) k = k x
```

```haskell
foldAn :: forall c. A c -> (forall r. (forall x. c x => x -> r) -> r)
foldAn = foldA
```

### Transforming `An` existential

Finally, we can change the constraint that `A` value has as long as the target
constraint is implied by the current one:

```haskell
forget :: forall c d. c :- d => A c -> A d
forget (An x) = An (Ghost x)
```

## Getting carried away

We can create `An` existential that witnesses multiple constraints using
something like this:

```haskell
type (&&) :: (Type -> Constraint) -> (Type -> Constraint) -> (Type -> Constraint)
class (c x, d x) => (c && d) x
instance (c x, d x) => (c && d) x
```

So, let's imagine we have `An (Eq && Show)`; how do we write a `Show` instance
for `An` that would also work for this? Well, our first guess might be some
quantified constraint:

```
instance (forall x. c x => Show x) => Show (An c) where
  show (An x) = show x
```

Sadly, [it doesn't work](https://gitlab.haskell.org/ghc/ghc/-/issues/17939),
which is a real shame. So, we have to get creative. First, we define a newtype
with a bunch of instances carried straight through from the underlying type:

```haskell
type Ghost :: Type -> Type
newtype Ghost x = Ghost x
  deriving newtype (Eq, Hashable, Ord, Show)
```

Next, we can define a modified version of our above implication that says,
instead of `c x` impliying `d x`, `c x` implies `d (Ghost x)`, which prevents
GHC from selecting the wrong instance.

```haskell
type (:-) :: (Type -> Constraint) -> (Type -> Constraint) -> Constraint
type c :- d = forall x. c x => d (Ghost x)

infix 1 :-
```

With that sorted, we can have all the instances we want _as long as_ we operate
via the `Ghost` type. It's far from ideal, but it does at least work.

```haskell
instance c :- Hashable && Typeable => Hashable (A c) where
  hashWithSalt d (An x) = hashWithSalt d (Ghost x)
  hash (An x) = hash (Ghost x)
```

```haskell
instance (c :- Show) => Show (An c) where
  show (An x) = show (Ghost x)
```

```haskell
instance c :- Eq && Typeable => Eq (A c) where
  An x == An y = do
    let xs = typeOf (Ghost x)
        ys = typeOf (Ghost y)
    
    case eqTypeRep xs ys of
      Just HRefl -> Ghost x == Ghost y
      Nothing -> False
```

```haskell
instance c :- Ord && Typeable => Ord (A c) where
  compare (An x) (An y) = do
    let xs@(SomeTypeRep -> xs') = typeOf (Ghost x)
        ys@(SomeTypeRep -> ys') = typeOf (Ghost y)
    
    case eqTypeRep xs ys of
      Just HRefl -> compare (Ghost x) (Ghost y)
      Nothing -> compare xs' ys'
```
