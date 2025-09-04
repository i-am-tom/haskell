{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Tools for writing proofs with constraints.
module Data.Constraint.Proof where

import Data.Constraint.Helpers (Every)
import Data.Kind (Constraint)

-- | Preservation: a constraint on a value implies a constraint on a value lifted over some @f@.
type Preserves :: (k -> k) -> (k -> Constraint) -> Constraint
class (Maps f c c) => Preserves f c

instance (Maps f c c) => Preserves f c

-- | Dual of @Preserves@.
type Copreserves :: (k -> k) -> (k -> Constraint) -> Constraint
class (Comaps f c c) => Copreserves f c

instance (Comaps f c c) => Copreserves f c

-- | Preservation in both directions.
type Bipreserves :: (k -> k) -> (k -> Constraint) -> Constraint
class (Preserves f c, Copreserves f c) => Bipreserves f c

instance (Preserves f c, Copreserves f c) => Bipreserves f c

-- | Preservation: a constraint on two values implies a constraint on the values lifted over some @f@.
type Preserves2 :: (k -> k -> k) -> (k -> Constraint) -> Constraint
class (Maps2 f c c) => Preserves2 f c

instance (Maps2 f c c) => Preserves2 f c

-- | Dual of @Preserves2@.
type Copreserves2 :: (k -> k -> k) -> (k -> Constraint) -> Constraint
class (Comaps2 f c c) => Copreserves2 f c

instance (Comaps2 f c c) => Copreserves2 f c

-- | Preservation in both directions.
type Bipreserves2 :: (k -> k -> k) -> (k -> Constraint) -> Constraint
class (Preserves2 f c, Copreserves2 f c) => Bipreserves2 f c

instance (Preserves2 f c, Copreserves2 f c) => Bipreserves2 f c

-- | Proof that @f@ maps a proof of @c@ to a proof of @c'@.
type Maps :: (j -> k) -> (j -> Constraint) -> (k -> Constraint) -> Constraint
class (forall x. (c x) => c' (f x)) => Maps f c c'

instance (forall x. (c x) => c' (f x)) => Maps f c c'

-- | Dual of @Maps@.
type Comaps :: (j -> k) -> (k -> Constraint) -> (j -> Constraint) -> Constraint
class (forall x. (c (f x)) => c' x) => Comaps f c c'

instance (forall x. (c (f x)) => c' x) => Comaps f c c'

-- | Mapping in both directions.
type Bimaps :: (j -> k) -> (j -> Constraint) -> (k -> Constraint) -> Constraint
class (Maps f c c', Comaps f c' c) => Bimaps f c c'

instance (Maps f c c', Comaps f c' c) => Bimaps f c c'

-- | Proof that @f@ maps a proof of @c@ to a proof of @c'@.
type Maps2 :: (k -> k -> l) -> (k -> Constraint) -> (l -> Constraint) -> Constraint
class (forall x y. (c x, c y) => c' (f x y)) => Maps2 f c c'

instance (forall x y. (c x, c y) => c' (f x y)) => Maps2 f c c'

-- | Dual of @Maps2@.
type Comaps2 :: (k -> k -> l) -> (l -> Constraint) -> (k -> Constraint) -> Constraint
class (forall x y. (c (f x y)) => Every c' '[x, y]) => Comaps2 f c c'

instance (forall x y. (c (f x y)) => Every c' '[x, y]) => Comaps2 f c c'

-- | Mapping in both directions.
type Bimaps2 :: (k -> k -> l) -> (k -> Constraint) -> (l -> Constraint) -> Constraint
class (Maps2 f c c', Comaps2 f c' c) => Bimaps2 f c c'

instance (Maps2 f c c', Comaps2 f c' c) => Bimaps2 f c c'

-- | Proof that @c@ implies @d@ and vice versa.
type Symm :: (k -> Constraint) -> (k -> Constraint) -> Constraint
class (forall x. (c x) => d x, forall x. (d x) => c x) => Symm c d

instance (forall x. (c x) => d x, forall x. (d x) => c x) => Symm c d
