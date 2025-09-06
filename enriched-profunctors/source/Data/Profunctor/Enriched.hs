{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

-- | Profunctors for categories that aren't @Hask@.
module Data.Profunctor.Enriched where

import Control.Arrow (Kleisli (..))
import Control.Category.Constrained (Category (id, (.)), Category', type (~>) (..))
import Control.Category.Constrained qualified as Cat
import Data.Functor.Contravariant (Op)
import Data.Kind (Constraint, Type)
import Data.Profunctor (Forget (..))
import Data.Type.Coercion (Coercion)
import Data.Type.Equality ((:~:), (:~~:))
import Prelude hiding (id, (.))

-- | The objects of a profunctor's underlying category.
type Obj :: (t -> t -> Type) -> t -> Constraint
type Obj p = Cat.Obj (Cat p)

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
  dimap :: (Obj p a, Obj p b, Obj p c, Obj p d) => k a b -> k c d -> p b c -> p a d
  default dimap :: (Obj p a, Obj p b, Obj p c, Obj p d, k ~ p) => k a b -> k c d -> p b c -> p a d
  dimap pre post f = post . f . pre

-- | Because the category is determined by functional dependency, we can use
-- this synonym in constraints to save us some type variables.
type Profunctor' :: (t -> t -> Type) -> Constraint
type Profunctor' p = Profunctor (Cat p) p

-- | Apply a mapping to the left hand (contravariant) side of the profunctor.
lmap :: (Profunctor k p, Obj p a, Obj p b, Obj p c) => k a b -> p b c -> p a c
lmap f = dimap f id

-- | Apply a mapping to the right hand (covariant) side of the profunctor.
rmap :: (Profunctor k p, Obj p a, Obj p b, Obj p c) => k b c -> p a b -> p a c
rmap f = dimap id f

instance Profunctor (->) (->) where
  type Cat (->) = (->)

instance Profunctor (->) (Forget r) where
  type Cat (Forget r) = (->)

  dimap :: (a -> b) -> (c -> d) -> Forget r b c -> Forget r a d
  dimap pre _ (Forget f) = Forget (f . pre)

type Forget1 :: (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> Type
newtype Forget1 r f g = Forget1 {runForget1 :: f ~> r}

instance (Functor r) => Profunctor (~>) (Forget1 r) where
  type Cat (Forget1 r) = (~>)

  dimap :: (Functor a, Functor b) => (a ~> b) -> (c ~> d) -> Forget1 r b c -> Forget1 r a d
  dimap pre _ (Forget1 k) = Forget1 (k . pre)

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
