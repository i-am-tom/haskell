{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- | Categories for interacting with generic representations.
module Control.Category.Generic where

import Control.Cartesian.Constrained (Cartesian (..))
import Control.Category.Constrained (Category (..), type (~>) (..))
import Control.Cocartesian.Constrained (Cocartesian (..))
import Data.Constraint.Proof (Preserves)
import Data.Kind (Constraint, Type)
import GHC.Generics (M1 (..), (:*:), (:+:))

-- | A type synonym to make the signatures bearable.
type RepK :: Type
type RepK = Type -> Type

-- | A class for categories that are able to represent generic representations.
type GenericC :: ((Type -> Type) -> Constraint) -> ((Type -> Type) -> (Type -> Type) -> Type) -> Constraint
class
  (Cartesian c (:*:) k, Cocartesian c (:+:) k, forall i m. Preserves (M1 i m) c) =>
  GenericC c k
    | k -> c
  where
  -- | Wrap a value in metadata.
  _M1 :: (c x) => k x (M1 i m x)

  -- | Unwrap a value inside metadata.
  _unM1 :: (c x) => k (M1 i m x) x

instance GenericC Functor (~>) where
  _M1 = NT M1
  _unM1 = NT unM1

-- | A neater type synonym for "GenericC".
type GenericC' :: (RepK -> RepK -> Type) -> Constraint
type GenericC' k = GenericC (Obj k) k
