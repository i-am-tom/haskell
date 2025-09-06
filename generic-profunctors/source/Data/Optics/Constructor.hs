{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Generic optics for constructors.
module Data.Optics.Constructor where

import Control.Category.Generic (RepK)
import Data.Kind (Constraint, Type)
import Data.Optics.Helper (ConstructorPath, Step)
import Data.Profunctor.Choice.Enriched (left', right')
import Data.Profunctor.Enriched (Obj)
import Data.Profunctor.Generic (Generically (..), Generically', HasGenerics', meta)
import GHC.Generics hiding (Generically)
import GHC.TypeLits (Symbol)

-- | Generic machinery for "HasConstructor".
type GHasConstructor :: [Step] -> (RepK -> RepK -> Type) -> RepK -> Type -> Constraint
class (HasGenerics' p) => GHasConstructor path p rep a | path rep -> a where
  gconstructor :: p (K1 R a) (K1 R a) -> p rep rep

-- | Convenience synonym for "GHasConstructor".
type GHasConstructor' :: (RepK -> RepK -> Type) -> Symbol -> Type -> Type -> Constraint
type GHasConstructor' p x s a = GHasConstructor (ConstructorPath x s) p (Rep s) a

instance
  (GHasConstructor path p s a, Obj p s) =>
  GHasConstructor path p (M1 i m s) a
  where
  gconstructor = meta . gconstructor @path

instance
  (GHasConstructor path p l a, Obj p l, Obj p r) =>
  GHasConstructor ('Left ': path) p (l :+: r) a
  where
  gconstructor = left' . gconstructor @path

instance
  (GHasConstructor path p r a, Obj p l, Obj p r) =>
  GHasConstructor ('Right ': path) p (l :+: r) a
  where
  gconstructor = right' . gconstructor @path

instance (HasGenerics' p, s ~ a) => GHasConstructor '[] p (K1 R s) a where
  gconstructor = id

-- | A class for lifting profunctors over entire types.
type HasConstructor :: Symbol -> (Type -> Type -> Type) -> Type -> Type -> Constraint
class HasConstructor x p s a | x s -> a where
  -- | Lift an arrow over a field into an arrow over its ADT.
  constructor :: p a a -> p s s

instance
  (Generic s, Generically' p, GHasConstructor' (Representation p) x s a) =>
  HasConstructor x p s a
  where
  constructor = generically . gconstructor @(ConstructorPath x s) . ungenerically
