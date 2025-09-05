{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}

-- | A cartesian category with constraints upon its objects.
module Control.Cartesian.Constrained where

import Control.Category.Constrained (Category (..), Trivial, type (~>) (..))
import Data.Constraint.Proof (Preserves2)
import Data.Functor.Contravariant (Op (..))
import Data.Kind (Constraint, Type)
import GHC.Generics ((:*:) (..))
import Prelude hiding (id, (.))

-- | A cartesian category is a "Category" that includes product types along
-- with constructors and destructors.
type Cartesian :: forall t. (t -> Constraint) -> (t -> t -> t) -> (t -> t -> Type) -> Constraint
class (Preserves2 p c, Category c k, Product k ~ p) => Cartesian c p (k :: t -> t -> Type) | k -> c p where
  -- | The type of products within this category.
  type Product k :: t -> t -> t

  -- | Product constructor.
  (△) :: (c x, c y, c z) => k x y -> k x z -> k x (y `p` z)

  -- | Product left destructor.
  exl :: (c x, c y) => k (x `p` y) x

  -- | Product right destructor.
  exr :: (c x, c y) => k (x `p` y) y

-- | Apply an arrow to each side of a cartesian product type.
(***) :: (Cartesian c p k, c x, c y, c z, c w) => k x y -> k z w -> k (x `p` z) (y `p` w)
f *** g = (f . exl) △ (g . exr)

-- | Swap a cartesian product.
swap :: (Cartesian c p k, c x, c y) => k (x `p` y) (y `p` x)
swap = exr △ exl

-- | Reassociate parentheses to the left.
assocl :: (Cartesian c p k, c x, c y, c z) => k (x `p` (y `p` z)) ((x `p` y) `p` z)
assocl = (exl △ (exl . exr)) △ (exr . exr)

-- | Reassociate parentheses to the right.
assocr :: (Cartesian c p k, c x, c y, c z) => k ((x `p` y) `p` z) (x `p` (y `p` z))
assocr = (exl . exl) △ ((exr . exl) △ exr)

-- | Duplicate an object.
dup :: (Cartesian c p k, c x) => k x (x `p` x)
dup = id △ id

-- | Because both the object constraint and the product type are determined by
-- functional dependency, we can use this synonym in constraints to save us
-- some type variables.
type Cartesian' :: (t -> t -> Type) -> Constraint
type Cartesian' k = Cartesian (Obj k) (Product k) k

instance Cartesian Trivial (,) (->) where
  type Product (->) = (,)

  (△) :: (x -> y) -> (x -> z) -> (x -> (y, z))
  f △ g = \x -> (f x, g x)

  exl :: (x, y) -> x
  exl = \(x, _) -> x

  exr :: (x, y) -> y
  exr = \(_, y) -> y

instance Cartesian Trivial Either Op where
  type Product Op = Either

  (△) :: Op x y -> Op x z -> Op x (Either y z)
  Op f △ Op g = Op (either f g)

  exl :: Op (Either x y) x
  exl = Op Left

  exr :: Op (Either x y) y
  exr = Op Right

instance Cartesian Functor (:*:) (~>) where
  type Product (~>) = (:*:)

  (△) :: (x ~> y) -> (x ~> z) -> (x ~> (y :*: z))
  NT f △ NT g = NT \x -> f x :*: g x

  exl :: (x :*: y) ~> x
  exl = NT \(x :*: _) -> x

  exr :: (x :*: y) ~> y
  exr = NT \(_ :*: y) -> y
