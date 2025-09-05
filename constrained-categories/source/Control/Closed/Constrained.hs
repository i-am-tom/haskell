{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}

-- | A closed category with constraints upon its objects.
module Control.Closed.Constrained where

import Control.Cartesian.Constrained (Cartesian (..), Cartesian')
import Control.Category.Constrained (Category (..), Trivial, type (~>) (..))
import Data.Constraint.Proof (Preserves2)
import Data.Kind (Constraint, Type)
import GHC.Generics ((:*:) (..))
import Prelude hiding (id, uncurry, (.))
import Prelude qualified

-- | A closed cartesian category is a category that includes morphisms as
-- objects.
type Closed :: forall t. (t -> Constraint) -> (t -> t -> t) -> (t -> t -> Type) -> Constraint
class (Preserves2 h c, Cartesian' k, Hom k ~ h) => Closed c h (k :: t -> t -> Type) | k -> c h where
  -- | The type of morphisms within this category.
  type Hom (k :: t -> t -> Type) :: t -> t -> t

  -- | Morphism currying.
  curry :: (c x, c y, c z) => k (Product k x y) z -> k x (h y z)

  -- | Morphism uncurrying.
  uncurry :: (c x, c y, c z) => k x (h y z) -> k (Product k x y) z

-- | An arrow from a mapping and an input to its output. In @(->)@, this is
-- function application.
apply :: (Cartesian c p k, Closed c h k, c x, c y) => k (p (h x y) x) y
apply = uncurry id

-- | Because both the object constraint and the morphism type are determined by
-- functional dependency, we can use this synonym in constraints to save us
-- some type variables.
type Closed' :: (t -> t -> Type) -> Constraint
type Closed' k = Closed (Obj k) (Hom k) k

instance Closed Trivial (->) (->) where
  type Hom (->) = (->)

  curry :: ((x, y) -> z) -> (x -> y -> z)
  curry = Prelude.curry

  uncurry :: (x -> y -> z) -> ((x, y) -> z)
  uncurry = Prelude.uncurry

-- | Natural transformations form a closed category, where we can represent the
-- products with the following type.
type End :: (Type -> Type) -> (Type -> Type) -> (Type -> Type)
newtype End f g x = End {unEnd :: forall e. (x -> e) -> f e -> g e}
  deriving stock (Functor)

instance Closed Functor End (~>) where
  type Hom (~>) = End

  curry :: (Functor x) => ((x :*: y) ~> z) -> (x ~> End y z)
  curry (NT f) = NT \x -> End \g y -> f (fmap g x :*: y)

  uncurry :: (x ~> End y z) -> ((x :*: y) ~> z)
  uncurry (NT f) = NT \(x :*: y) -> let End g = f x in g id y
