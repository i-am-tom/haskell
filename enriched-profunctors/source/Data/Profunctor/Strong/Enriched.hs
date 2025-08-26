{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Strong profunctors for categories that aren't @Hask@.
module Data.Profunctor.Strong.Enriched where

import Control.Arrow (Kleisli (..))
import Control.Cartesian.Constrained (Cartesian (..), Cartesian', Product, dup, swap)
import Control.Category.Constrained (Obj, type (~>) (..))
import Data.Bifunctor (first, second)
import Data.Bitraversable (bitraverse)
import Data.Functor.Contravariant (Op (..))
import Data.Kind (Constraint, Type)
import Data.Profunctor.Enriched (Profunctor (..))
import GHC.Generics ((:*:) (..))

-- | An extension of the typical definition of a strong profunctor to allow for
-- profunctors over non-@Hask@ categories.
type Strong :: forall t. (t -> t -> Type) -> (t -> t -> Type) -> Constraint
class (Profunctor k p, Cartesian' k) => Strong k p | p -> k where
  -- | Lift a mapping over the left side of a product.
  first' :: (Obj k x, Obj k y, Obj k z) => p x y -> p (Product k x z) (Product k y z)
  first' = dimap swap swap . second'

  -- | Lift a mapping over the right side of a product.
  second' :: (Obj k x, Obj k y, Obj k z) => p x y -> p (Product k z x) (Product k z y)
  second' = dimap swap swap . first'

  {-# MINIMAL first' | second' #-}

strong :: (Strong k p, Obj k x, Obj k y, Obj k z) => k (Product k x y) z -> p x y -> p x z
strong k = dimap dup k . second'

-- | Because the category is determined by functional dependency, we can use
-- this synonym in constraints to save us some type variables.
type Strong' :: (t -> t -> Type) -> Constraint
type Strong' p = Strong (Cat p) p

instance Strong (->) (->) where
  first' :: (x -> y) -> ((x, z) -> (y, z))
  first' = first

  second' :: (x -> y) -> ((z, x) -> (z, y))
  second' = second

instance Strong Op Op where
  first' :: Op x y -> Op (Either x z) (Either y z)
  first' (Op f) = Op (first f)

  second' :: Op x y -> Op (Either z x) (Either z y)
  second' (Op f) = Op (second f)

instance (Monad m) => Strong (->) (Kleisli m) where
  first' :: Kleisli m x y -> Kleisli m (x, z) (y, z)
  first' (Kleisli f) = Kleisli (bitraverse f pure)

  second' :: Kleisli m x y -> Kleisli m (z, x) (z, y)
  second' (Kleisli f) = Kleisli (bitraverse pure f)

instance Strong (~>) (~>) where
  first' :: (x ~> y) -> ((x :*: z) ~> (y :*: z))
  first' (NT f) = NT \(x :*: z) -> f x :*: z

  second' :: (x ~> y) -> ((z :*: x) ~> (z :*: y))
  second' (NT f) = NT \(z :*: x) -> z :*: f x
