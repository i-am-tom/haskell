{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Closed profunctors for categories that aren't @Hask@.
module Data.Profunctor.Closed.Enriched where

import Control.Category.Constrained (Obj, type (~>) (..))
import Control.Closed.Constrained (End (..))
import Control.Closed.Constrained qualified as Cat
import Data.Kind (Constraint, Type)
import Data.Profunctor.Enriched (Cat)
import Data.Profunctor.Strong.Enriched (Strong (..))

-- | An extension of the typical definition of a closed profunctor to allow for
-- profunctors over non-@Hask@ categories.
type Closed :: forall t. (t -> t -> Type) -> (t -> t -> Type) -> Constraint
class (Strong k p, Cat.Closed' k) => Closed k p | p -> k where
  -- | Lift a mapping into a 'Hom'.
  closed :: (Obj k x, Obj k y) => p x y -> p (Cat.Hom k i x) (Cat.Hom k i y)

type Closed' :: (t -> t -> Type) -> Constraint
type Closed' p = Closed (Cat p) p

instance Closed (->) (->) where
  closed :: (x -> y) -> ((i -> x) -> (i -> y))
  closed = (.)

instance Closed (~>) (~>) where
  closed :: (x ~> y) -> (End i x ~> End i y)
  closed (NT f) = NT \(End k) -> End \g -> f . k g
