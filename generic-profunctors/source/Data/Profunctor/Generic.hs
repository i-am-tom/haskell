{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Profunctors specifically for handling generic representations.
module Data.Profunctor.Generic where

import Control.Category.Constrained (Category ((.)), type (~>) (..))
import Control.Category.Generic (GenericC (..), GenericC', RepK)
import Data.Constraint.Proof (Maps)
import Data.Kind (Constraint, Type)
import Data.Profunctor.Choice.Enriched (Choice)
import Data.Profunctor.Enriched (Obj, Profunctor (..))
import Data.Profunctor.Strong.Enriched (Strong)
import GHC.Generics hiding (Generically)
import Prelude hiding ((.))

-- | Profunctors that we can use to handle generic representations.
type HasGenerics :: (RepK -> RepK -> Type) -> (RepK -> RepK -> Type) -> Constraint
class (Strong k p, Choice k p, GenericC' k) => HasGenerics k p | p -> k

instance (Strong k p, Choice k p, GenericC' k) => HasGenerics k p

-- | A neater type synonym for "HasGenerics".
type HasGenerics' :: (RepK -> RepK -> Type) -> Constraint
type HasGenerics' p = HasGenerics (Cat p) p

-- | A metadata profunctor transformation.
meta :: (HasGenerics' p, Obj p x, Obj p y) => p x y -> p (M1 i m x) (M1 i m y)
meta = dimap _unM1 _M1

-- | A class that links two profunctors together: one that supports arrows over
-- @Type@ data, and one that supports arrows over their generic representations.
type Generically :: (Type -> Type -> Type) -> (RepK -> RepK -> Type) -> Constraint
class
  (HasGenerics' p', Representation p ~ p', Maps (K1 R) (Obj p) (Obj p')) =>
  Generically p p'
    | p -> p'
  where
  -- | The profunctor for generic representations of @p@ objects. This should
  -- always be @p'@. Usually, we can use "Generically'" to make the signatures
  -- nicer, but having the type instance variable allows for quantified
  -- constraints.
  type Representation p :: RepK -> RepK -> Type

  -- | Create an arrow between types from an arrow between representations.
  generically :: (Generic x, Generic y) => p' (Rep x) (Rep y) -> p x y

  -- | Create an arrow between representations from an arrow between types.
  ungenerically :: p x y -> p' (K1 R x) (K1 R y)

-- | A type synonym to hide the second type variable.
type Generically' :: (Type -> Type -> Type) -> Constraint
type Generically' p = Generically p (Representation p)

instance Generically (->) (~>) where
  type Representation (->) = (~>)

  generically (NT f) = to . f . from
  ungenerically f = NT \(K1 x) -> K1 (f x)
