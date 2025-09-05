{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}

-- | Choice profunctors for categories that aren't @Hask@.
module Data.Profunctor.Choice.Enriched where

import Control.Arrow (Kleisli (..))
import Control.Category.Constrained (type (~>) (..))
import Control.Cocartesian.Constrained (Cocartesian (..), Cocartesian', swap)
import Data.Bifunctor (first, second)
import Data.Bitraversable (bitraverse)
import Data.Functor.Contravariant (Op (..))
import Data.Kind (Constraint, Type)
import Data.Profunctor.Enriched (Obj, Profunctor (..))
import GHC.Generics ((:+:) (..))

-- | An extension of the typical definition of a choice profunctor to allow for
-- profunctors over non-@Hask@ categories.
type Choice :: forall t. (t -> t -> Type) -> (t -> t -> Type) -> Constraint
class (Profunctor k p, Cocartesian' k) => Choice k p | p -> k where
  -- | Lift a mapping over the left side of a sum.
  left' :: (Obj p x, Obj p y, Obj p z) => p x y -> p (Sum k x z) (Sum k y z)
  left' = dimap swap swap . right'

  -- | Lift a mapping over the right side of a sum.
  right' :: (Obj p x, Obj p y, Obj p z) => p x y -> p (Sum k z x) (Sum k z y)
  right' = dimap swap swap . left'

  {-# MINIMAL left' | right' #-}

-- | Because the category is determined by functional dependency, we can use
-- this synonym in constraints to save us some type variables.
type Choice' :: (t -> t -> Type) -> Constraint
type Choice' p = Choice (Cat p) p

instance Choice (->) (->) where
  left' :: (x -> y) -> (Either x z -> Either y z)
  left' = first

  right' :: (x -> y) -> (Either z x -> Either z y)
  right' = second

instance Choice Op Op where
  left' :: Op x y -> Op (x, z) (y, z)
  left' (Op f) = Op (first f)

  right' :: Op x y -> Op (z, x) (z, y)
  right' (Op f) = Op (second f)

instance (Monad m) => Choice (->) (Kleisli m) where
  left' :: Kleisli m x y -> Kleisli m (Either x z) (Either y z)
  left' (Kleisli f) = Kleisli (bitraverse f pure)

  right' :: Kleisli m x y -> Kleisli m (Either z x) (Either z y)
  right' (Kleisli f) = Kleisli (bitraverse pure f)

instance Choice (~>) (~>) where
  left' :: (x ~> y) -> ((x :+: z) ~> (y :+: z))
  left' (NT f) = NT \case L1 x -> L1 (f x); R1 y -> R1 y

  right' :: (x ~> y) -> ((z :+: x) ~> (z :+: y))
  right' (NT f) = NT \case L1 x -> L1 x; R1 y -> R1 (f y)
