{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Compute and apply deltas to values.
module Data.Change where

import Data.Functor.Const (Const (Const))
import Data.Kind (Constraint, Type)
import Data.Monoid (Sum (Sum))
import Data.Void (Void)
import GHC.Generics

-- | A class for describing incremental changes to types.
--
-- For many types, the relationships between values can be easily expressed. A
-- good example is @Int@. We can use an @Int@ to describe the difference
-- between two values, and thus the type can describe its own changes. In fact,
-- this is true for any abelian group. However, the concept of patches is more
-- general than that; what type describes changes in @String@? Or @Either@
-- values?
--
-- This class defines the @Δ@ associated type: the type of changes to the
-- original type. It also defines two functions for producing and consuming
-- these deltas.
--
-- If I knew more maths, I'd say that `Change` is an extension of monoid
-- actions. However, the fact that we also have a difference functions means
-- we have something more group-ish. Maybe a torsor? I'd be thrilled if someone
-- could tell me what this class is called in the literature.
type Change :: Type -> Constraint
class Monoid (Δ x) => Change x where

  -- | The type of deltas on a given type.
  --
  -- For a given type @T@, @Δ T@ is a type that describes differences between
  -- two values of type @T@. For numeric types, this can just be the difference
  -- between two types. However, this can be much more complicated, or even
  -- domain-specific.
  type Δ x :: Type

  -- | **Update** a given value with a given patch.
  --
  -- This function should update the value according to the same laws as the
  -- @Action@ class in @monoid-extras@:
  --
  -- prop> x <~ mempty == x
  -- prop> x <~ α <~ β == x <~ (α <> β)
  (<~) :: x -> Δ x -> x

  -- | Calculate the **difference** between two values as a patch. I
  -- conceptualise this as subtraction. These two functions can be tied
  -- together by the following law:
  --
  -- prop> x <~ (y \\ x) == y
  (\\) :: x -> x -> Δ x

infixl 5 <~
infixl 5 \\

deriving via Generically ()
  instance Change ()

deriving via Generically Bool
  instance Change Bool

instance Change Int where
  type Δ Int = Sum Int

  x <~ Sum y = x + y
  x \\ y = Sum (x - y)

instance Change Double where
  type Δ Double = Sum Double

  x <~ Sum y = x + y
  x \\ y = Sum (x - y)

deriving via Generically (Maybe x)
  instance (Change x)
    => Change (Maybe x)

deriving via Generically (Either x y)
  instance (Change x, Change y)
    => Change (Either x y)

deriving via Generically (x, y)
  instance (Change x, Change y)
    => Change (x, y)

instance (Change x, Change y) => Change (x -> y) where
  type Δ (x -> y) = x -> Δ x -> Δ y

  f <~ δ = \x -> f x <~ δ x mempty
  f \\ g = \x δ -> f (x <~ δ) \\ g (x <~ δ)

instance (Generic x, Monoid (Γ (Rep x) Void), GChange (Rep x))
    => Change (Generically x) where
  type Δ (Generically x) = Γ (Rep x) Void

  Generically x <~ δ = Generically (to (from x <~# δ))
  Generically x \\ Generically y = from x \\# from y

-- | A generic implementation of @Change@.
type GChange :: (Type -> Type) -> Constraint
class Monoid (Γ rep Void) => GChange rep where

  -- | A generic type's @Δ@.
  type Γ rep :: Type -> Type

  -- | @(<~)@ for generic types.
  (<~#) :: rep x -> Γ rep x -> rep x

  -- | @(\\)@ for generic types.
  (\\#) :: rep x -> rep x -> Γ rep x

instance GChange V1 where
  type Γ V1 = Const ()

  (<~#) = \case
  (\\#) = \case

instance GChange x => GChange (M1 i m x) where
  type Γ (M1 i m x) = Γ x

  M1 x <~# γ = M1 (x <~# γ)
  M1 x \\# M1 y = x \\# y

instance GChange U1 where
  type Γ U1 = Const ()

  U1 <~# Const () = U1
  U1 \\# U1 = Const ()

instance (GChange x, GChange y) => GChange (x :+: y) where
  type Γ (x :+: y) = Choice x y

  this <~# Ø           = this
  ____ <~# Flip (L1 y) = L1 y
  ____ <~# Flip (R1 y) = R1 y
  L1 x <~# Stay (L1 γ) = L1 (x <~# γ)
  R1 x <~# Stay (R1 γ) = R1 (x <~# γ)
  L1 x <~# Stay (R1 _) = L1  x
  R1 x <~# Stay (L1 _) = R1  x

  L1 x \\# L1 y = Stay (L1 (x \\# y))
  L1 x \\# R1 _ = Flip (L1  x)
  R1 x \\# L1 _ = Flip (R1  x)
  R1 x \\# R1 y = Stay (R1 (x \\# y))

instance (GChange x, GChange y) => GChange (x :*: y) where
  type Γ (x :*: y) = (Γ x :*: Γ y)

  (x :*: y) <~# (z :*: w) = (x <~# z) :*: (y <~# w)
  (x :*: y) \\# (z :*: w) = (x \\# z) :*: (y \\# w)

instance Change x => GChange (K1 R x) where
  type Γ (K1 R x) = Const (Δ x)

  K1 x <~# Const δ = K1 (x <~ δ)
  K1 x \\# K1 y = Const (x \\ y)

---

type Choice :: (Type -> Type) -> (Type -> Type) -> (Type -> Type)
data Choice x y v = Stay ((Γ x :+: Γ y) v) | Flip ((x :+: y) v) | Ø

deriving instance (Eq (x v), Eq (Γ x v), Eq (y v), Eq (Γ y v))
  => Eq (Choice x y v)

deriving instance (Ord (x v), Ord (Γ x v), Ord (y v), Ord (Γ y v))
  => Ord (Choice x y v)

deriving instance (Show (x v), Show (Γ x v), Show (y v), Show (Γ y v))
  => Show (Choice x y v)

instance (GChange x, GChange y) => Semigroup (Choice x y Void) where
  ___________ <> Flip x      = Flip x
  Flip (L1 x) <> Stay (L1 δ) = Flip (L1 (x <~# δ))
  Stay (L1 ξ) <> Stay (L1 δ) = Stay (L1 (ξ <> δ))
  Flip (R1 x) <> Stay (R1 δ) = Flip (R1 (x <~# δ))
  Stay (R1 ξ) <> Stay (R1 δ) = Stay (R1 (ξ <> δ))
  anything    <> Ø           = anything
  Ø           <> anything    = anything
  _           <> _           = Ø

instance (GChange x, GChange y) => Monoid (Choice x y Void) where
  mempty = Ø
