{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

-- | An interface for approximate equality.
module Data.Approx where

import Data.Kind (Constraint, Type)
import Data.Ratio (Ratio)
import GHC.Generics

-- | A class for approximate equality. Implementations of this class must
-- adhere to two laws:
--
-- prop> x ~= x === True
-- prop> x ~= y === y ~= x
--
-- Note that transitivity and extensionality /are/ laws for @Eq@, but are not
-- required for an approximate equality implementation.
--
-- The error margin has been chosen completely arbitrarily, though we can
-- certainly make it variable in the future if the need arises.
type Approx :: Type -> Constraint
class Approx x where
  (~=) :: x -> x -> Bool
  default (~=) :: (Ord x, Fractional x) => x -> x -> Bool
  (~=) x y = abs (x - y) < 1e-2

instance Approx Double

instance Approx Float

instance (Integral x) => Approx (Ratio x)

instance (Generic x, GApprox (Rep x)) => Approx (Generically x) where
  Generically x ~= Generically y = gapprox (from x) (from y)

deriving via
  Generically (Maybe x)
  instance
    (Approx x) =>
    Approx (Maybe x)

deriving via
  Generically (Either x y)
  instance
    (Approx x, Approx y) =>
    Approx (Either x y)

deriving via
  Generically (x, y)
  instance
    (Approx x, Approx y) =>
    Approx (x, y)

deriving via
  Generically (x, y, z)
  instance
    (Approx x, Approx y, Approx z) =>
    Approx (x, y, z)

deriving via
  Generically (x, y, z, w)
  instance
    (Approx x, Approx y, Approx z, Approx w) =>
    Approx (x, y, z, w)

---

type GApprox :: (Type -> Type) -> Constraint
class GApprox rep where
  gapprox :: rep x -> rep x -> Bool

instance (GApprox x) => GApprox (M1 i m x) where
  gapprox (M1 x) (M1 y) = gapprox x y

instance GApprox V1 where
  gapprox = \case {}

instance (GApprox x, GApprox y) => GApprox (x :+: y) where
  gapprox (L1 x) (L1 y) = gapprox x y
  gapprox (R1 x) (R1 y) = gapprox x y
  gapprox _ _ = False

instance GApprox U1 where
  gapprox U1 U1 = True

instance (GApprox x, GApprox y) => GApprox (x :*: y) where
  gapprox (x :*: y) (z :*: w) = gapprox x z && gapprox y w

instance (Approx x) => GApprox (K1 R x) where
  gapprox (K1 x) (K1 y) = x ~= y
