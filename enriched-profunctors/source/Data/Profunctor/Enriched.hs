{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

-- | Profunctors for categories that aren't @Hask@.
module Data.Profunctor.Enriched where

import Control.Arrow (Kleisli (..))
import Control.Category.Constrained (Category (..), Category', type (~>) (..))
import Data.Functor.Contravariant (Op)
import Data.Kind (Constraint, Type)
import Data.Type.Coercion (Coercion)
import Data.Type.Equality ((:~:), (:~~:))
import Prelude hiding (id, (.))

-- | An extension of the typical definition of a profunctor to allow for
-- profunctors whose 'dimap' implementation would require non-@Hask@
-- categories.
type Profunctor :: forall t. (t -> t -> Type) -> (t -> t -> Type) -> Constraint
class (Category' k, Cat p ~ k) => Profunctor (k :: t -> t -> Type) p | p -> k where
  -- | Having the category as a class parameter allows us to use it in
  -- quantified constraints. However, in practice, the constraint is always
  -- determined by the category, so we also have a type family for unquantified
  -- constraints.
  type Cat (p :: t -> t -> Type) :: (t -> t -> Type)

  -- | Map over both type variables.
  dimap :: (Obj k a, Obj k b, Obj k c, Obj k d) => k a b -> k c d -> p b c -> p a d
  default dimap :: (Obj k a, Obj k b, Obj k c, Obj k d, k ~ p) => k a b -> k c d -> p b c -> p a d
  dimap pre post f = post . f . pre

-- | Apply a mapping to the left hand (contravariant) side of the profunctor.
lmap :: (Profunctor k p, Obj k a, Obj k b, Obj k c) => k a b -> p b c -> p a c
lmap f = dimap f id

-- | Apply a mapping to the right hand (covariant) side of the profunctor.
rmap :: (Profunctor k p, Obj k a, Obj k b, Obj k c) => k b c -> p a b -> p a c
rmap f = dimap id f

-- | Because the category is determined by functional dependency, we can use
-- this synonym in constraints to save us some type variables.
type Profunctor' :: (t -> t -> Type) -> Constraint
type Profunctor' p = Profunctor (Cat p) p

instance Profunctor (->) (->) where
  type Cat (->) = (->)

instance (Monad m) => Profunctor (->) (Kleisli m) where
  type Cat (Kleisli m) = (->)

  dimap :: (a -> b) -> (c -> d) -> Kleisli m b c -> Kleisli m a d
  dimap pre post (Kleisli f) = Kleisli (fmap post . f . pre)

instance Profunctor Op Op where
  type Cat Op = Op

instance Profunctor Coercion Coercion where
  type Cat Coercion = Coercion

instance Profunctor (:~:) (:~:) where
  type Cat (:~:) = (:~:)

instance Profunctor (:~~:) (:~~:) where
  type Cat (:~~:) = (:~~:)

instance Profunctor (~>) (~>) where
  type Cat (~>) = (~>)
