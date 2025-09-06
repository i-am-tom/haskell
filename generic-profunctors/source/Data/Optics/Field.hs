{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Generic optics for fields.
module Data.Optics.Field where

import Control.Category.Generic (RepK)
import Data.Kind (Constraint, Type)
import Data.Optics.Helper (FieldPath, Step)
import Data.Profunctor.Enriched (Obj)
import Data.Profunctor.Generic (Generically (..), Generically', HasGenerics', meta)
import Data.Profunctor.Strong.Enriched (first', second')
import GHC.Generics hiding (Generically)
import GHC.TypeLits (Symbol)

-- | Generic machinery for "HasField".
type GHasField :: [Step] -> (RepK -> RepK -> Type) -> RepK -> Type -> Constraint
class (HasGenerics' p) => GHasField path p rep a | path rep -> a where
  gfield :: p (K1 R a) (K1 R a) -> p rep rep

-- | Convenience synonym for "GHasField".
type GHasField' :: (RepK -> RepK -> Type) -> Symbol -> Type -> Type -> Constraint
type GHasField' p x s a = GHasField (FieldPath x s) p (Rep s) a

instance (GHasField path p s a, Obj p s) => GHasField path p (M1 i m s) a where
  gfield = meta . gfield @path

instance
  (GHasField path p l a, Obj p l, Obj p r) =>
  GHasField ('Left ': path) p (l :*: r) a
  where
  gfield = first' . gfield @path

instance
  (GHasField path p r a, Obj p l, Obj p r) =>
  GHasField ('Right ': path) p (l :*: r) a
  where
  gfield = second' . gfield @path

instance (HasGenerics' p, s ~ a) => GHasField '[] p (K1 R s) a where
  gfield = id

-- | A class for lifting profunctors over entire types.
type HasField :: Symbol -> (Type -> Type -> Type) -> Type -> Type -> Constraint
class HasField x p s a | x s -> a where
  -- | Lift an arrow over a field into an arrow over its ADT.
  field :: p a a -> p s s

instance
  (Generic s, Generically' p, GHasField' (Representation p) x s a) =>
  HasField x p s a
  where
  field = generically . gfield @(FieldPath x s) . ungenerically
