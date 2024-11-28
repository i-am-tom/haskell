# `Named` values

The [`StableName`](https://hackage.haskell.org/package/base/docs/System-Mem-StableName.html)
API allows us to give names to things based on their memory address. With a
bunch of caveats, this means we have a way of producing an `Eq`- or `Hash`-like
interface for any type, including awkward ones like functions.

<!--
```haskell
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Data.Named
  ( Named (key, value)
  , unsafeMakeName
  ) where

import Data.Function (on)
import Data.Hashable (Hashable (..))
import Data.Kind (Type)
import System.Mem.StableName (StableName, makeStableName)
import System.IO.Unsafe (unsafePerformIO)
```
-->

## The type

A `Named` value is just the value along with its `StableName`. It's not very
clever. `Functor` and `Applicative` here are implemented unsafely, but I'm
going to beg for forgiveness and say it's not the biggest IO crime one could
commit.

```haskell
type Named :: Type -> Type
data Named x = Named { key :: StableName x, value :: x }

unsafeMakeName :: forall x. x -> StableName x
unsafeMakeName !x = unsafePerformIO (makeStableName x)

instance Show x => Show (Named x) where
  show = show . value

instance Functor Named where
  fmap f (Named _ !x) | !y <- f x = Named (unsafeMakeName y) y

instance Applicative Named where
  Named _ !f <*> Named _ !x | !y <- f x = Named (unsafeMakeName y) y
  pure !x = Named (unsafeMakeName x) x
```

Based on the `StableName`, we can now implement a couple favourites with no
constraints on the inner type at all:

```haskell
instance Eq (Named x) where
  (==) = (==) `on` key

instance Hashable (Named x) where
  hash = hash . key
  hashWithSalt d = hashWithSalt d . key
```
