{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- |
-- Derivation by conversion to other types.
module Data.Via where

import Data.Approx (Approx (..))
import Data.Coerce (Coercible, coerce)
import Data.Function (on)
import Data.Kind (Constraint, Type)
import Witch (From (from))

-- | A newtype wrapper around the @y@ type. We use this to say, "I can map @x@
-- to @y@ and/or the reverse if @c@ is available".
type (+) :: Type -> (Type -> Type -> Constraint) -> Type -> Type
newtype (x + c) y = With { getWith :: y }

instance (Eq x, Out c x y) => Eq ((x + c) y) where
  (==) = (==) @x `on` (outbound @c . getWith)

instance (Approx x, Out c x y) => Approx ((x + c) y) where
  (~=) = (~=) @x `on` (outbound @c . getWith)

instance (Ord x, Out c x y) => Ord ((x + c) y) where
  compare = compare @x `on` (outbound @c . getWith)

instance (Show x, Out c x y) => Show ((x + c) y) where
  show = show @x . outbound @c . getWith

instance (Semigroup x, Exchange c x y) => Semigroup ((x + c) y) where
  this <> that = With (inbound @c (go this that))
    where go = (<>) @x `on` (outbound @c . getWith)

instance (Monoid x, Exchange c x y) => Monoid ((x + c) y) where
  mempty = With (inbound @c @x mempty)

-- | A synonym for a constraint that requires both directions of conversion.
type Exchange :: (Type -> Type -> Constraint) -> Type -> Type -> Constraint
type Exchange c x y = (In c x y, Out c x y)

-- | A generalisation of the @iso-deriving@ @Inject@ class to allow users to
-- decsribe the specific injection being used.
type In :: (Type -> Type -> Constraint) -> Type -> Type -> Constraint
class c x y => In c x y where
  inbound :: x -> y

instance x ~ y => In (~) x y where
  inbound = id

instance Coercible x y => In Coercible x y where
  inbound = coerce

instance From x y => In From x y where
  inbound = from

-- | A generalisation of the @iso-deriving@ @Project@ class to allow users to
-- describe the specific projection being used.
type Out :: (Type -> Type -> Constraint) -> Type -> Type -> Constraint
class c y x => Out c x y where
  outbound :: y -> x

instance x ~ y => Out (~) x y where
  outbound = id

instance Coercible x y => Out Coercible x y where
  outbound = coerce

instance From y x => Out From x y where
  outbound = from
