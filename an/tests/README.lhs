# `An` existential

Create existential values identified only by some constraint that they satisfy.

## Show me!

```haskell
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
```

<!--
```haskell
module README where

import Data.An
import Data.Kind (Constraint, Type)
import Data.Set (Set)
import Data.Set qualified as Set
import Type.Reflection (Typeable)
```
-->

We can define products of constraint constructors with `(&&)`:

```haskell
-- Settable x ~ (Ord x, Typeable x)
type Settable :: Type -> Constraint
type Settable = Ord && Typeable
```

Using the `An` type (or `A`, using the provided pattern and type synonyms), we
can produce a `Set` of `Settable` values:

```haskell
collection :: Set (A Settable) -- We need `Typeable` for comparing different types.
collection = Set.fromList [ An (800 :: Int), A "string" ]
```

We can add `A` new element to our collection as long as its constraints imply
`Settable`:

```haskell
add :: c :- Settable => A c -> Set (A Settable) -> Set (A Settable)
add x xs = Set.insert (forget x) xs
```

## Tell me!

<!--
```haskell
{-
```
-->

This package is an attempt to bodge [a problem I had with
GHC](https://gitlab.haskell.org/ghc/ghc/-/issues/17939): I want to be able to
write things like this:

```haskell
data Exists c where Exists :: c x => x -> Exists c

instance (forall x. c x => Show x) => Show (Exists c) where
  show (Exists x) = show x
```

... but, as is explained in the aforelinked GitLab issue, I can't do this. So,
my next best bet is to declare a `Ghost` type - a newtype whose instances are
all derived from the underlying type - and try to hide away the ugliness:

```haskell
newtype Ghost x = Ghost x

instance (forall x. c x => Show (Ghost x)) => Show (Exists c) where
  show (Exists x) = show (Ghost x)
```

Is it ideal? Absolutely not. Does it work? Not really. Can I do better? Not
without understanding a lot more about the implications of fixing this in the
compiler, and I'm simply not that clever.

<!--
```haskell
-}
```
-->
