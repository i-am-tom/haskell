{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A co-cartesian category with constraints upon its objects.
module Control.Cocartesian.Constrained where

import Control.Arrow (Kleisli (..))
import Control.Category.Constrained (Category (..), Trivial, type (~>) (..))
import Data.Constraint.Proof (Preserves2)
import Data.Functor.Contravariant (Op (..))
import Data.Kind (Constraint, Type)
import GHC.Generics ((:+:) (..))
import Prelude hiding (id, (.))

-- | A cocartesian category is a category that includes sum types along with
-- constructors and destructors.
type Cocartesian :: forall t. (t -> Constraint) -> (t -> t -> t) -> (t -> t -> Type) -> Constraint
class (Preserves2 s c, Category c k, Sum k ~ s) => Cocartesian c s (k :: t -> t -> Type) | k -> c s where
  -- | The type of sums within this category.
  type Sum (k :: t -> t -> Type) :: t -> t -> t

  -- | Sum destructor.
  (▽) :: (c x, c y, c z) => k x z -> k y z -> k (Sum k x y) z

  -- | Sum left constructor.
  inl :: (c x, c y) => k x (Sum k x y)

  -- | Sum right constructor.
  inr :: (c x, c y) => k y (Sum k x y)

-- | Apply an arrow to each side of a cocartesian coproduct type.
(***) :: (Cocartesian c s k, c x, c y, c z, c w) => k x z -> k y w -> k (x `s` y) (z `s` w)
f *** g = (inl . f) ▽ (inr . g)

-- | Swap a cocartesian coproduct.
swap :: (Cocartesian c s k, c x, c y) => k (x `s` y) (y `s` x)
swap = inr ▽ inl

-- | Reassociate parentheses to the left.
assocl :: (Cocartesian c s k, c x, c y, c z) => k (x `s` (y `s` z)) ((x `s` y) `s` z)
assocl = (inl . inl) ▽ ((inl . inr) ▽ inr)

-- | Reassociate parentheses to the right.
assocr :: (Cocartesian c s k, c x, c y, c z) => k ((x `s` y) `s` z) (x `s` (y `s` z))
assocr = (inl ▽ (inr . inl)) ▽ (inr . inr)

-- | Extract the object of a homogeneous sum.
flatten :: (Cocartesian c s k, c x) => k (x `s` x) x
flatten = id ▽ id

-- | Because both the object constraint and the sum type are determined by
-- functional dependency, we can use this synonym in constraints to save us
-- some type variables.
type Cocartesian' :: (t -> t -> Type) -> Constraint
type Cocartesian' k = Cocartesian (Obj k) (Sum k) k

instance Cocartesian Trivial Either (->) where
  type Sum (->) = Either

  (▽) :: (x -> z) -> (y -> z) -> (Either x y -> z)
  (▽) = either

  inl :: x -> Either x y
  inl = Left

  inr :: y -> Either x y
  inr = Right

instance Cocartesian Trivial (,) Op where
  type Sum Op = (,)

  (▽) :: Op x z -> Op y z -> Op (x, y) z
  Op f ▽ Op g = Op \x -> (f x, g x)

  inl :: Op x (x, y)
  inl = Op \(x, _) -> x

  inr :: Op y (x, y)
  inr = Op \(_, y) -> y

instance (Monad m) => Cocartesian Trivial Either (Kleisli m) where
  type Sum (Kleisli m) = Either

  (▽) :: Kleisli m x z -> Kleisli m y z -> Kleisli m (Either x y) z
  Kleisli f ▽ Kleisli g = Kleisli (f ▽ g)

  inl :: Kleisli m x (Either x y)
  inl = Kleisli (pure . inl)

  inr :: Kleisli m y (Either x y)
  inr = Kleisli (pure . inr)

instance Cocartesian Functor (:+:) (~>) where
  type Sum (~>) = (:+:)

  (▽) :: (x ~> z) -> (y ~> z) -> ((x :+: y) ~> z)
  NT f ▽ NT g = NT \case
    L1 x -> f x
    R1 y -> g y

  inl :: x ~> (x :+: y)
  inl = NT L1

  inr :: y ~> (x :+: y)
  inr = NT R1
