{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A generalised n-element heterogeneous sum type.
module Data.Variant
  ( Variant (Here, There)

  , fromEither
  , toEither

  , unwrap
  , unabsurd

  , All (foldC)
  , variant

  , Elem (embed, project)
  , Pluck (pluck)

  , qq
  ) where

import Data.Bifunctor (first)
import Data.Kind (Constraint, Type)
import Data.Variant.QQ (qq)

{-# ANN module ("HLint: ignore Use const" :: String) #-}


-- * Definition

-- | A 'Variant' is a sum type indexed by the possible types that could inhabit
-- it. For example, the following three values could all be values of type
-- 'Variant '[ Int, String, Bool ]':
--
--     Here 3
--     There (Here "hello")
--     There (There (Here True))
type Variant :: [Type] -> Type
data Variant xs where
  Here :: forall x xs. x -> Variant (x ': xs)
  There :: forall x xs. Variant xs -> Variant (x ': xs)

deriving stock instance AllC Eq xs => Eq (Variant xs)
deriving stock instance AllC Show xs => Show (Variant xs)

-- * Conversions

-- | Convert a two-element 'Variant' to an 'Either'.
toEither :: forall x y. Variant '[ x, y ] -> Either x y
toEither = variant Left (variant Right \case)

-- | Convert an 'Either' to a two-element 'Variant'.
fromEither :: forall x y. Either x y -> Variant '[ x, y ]
fromEither = either Here (There . Here)

-- | A one-element 'Variant' can be safely unwrapped. The inverse operation is
-- the 'Here' constructor.
unwrap :: forall x. Variant '[x] -> x
unwrap = variant id \case

-- | A zero-element 'Variant' is isomorphic to 'Void'. The inverse operation
-- (when we specialise @x@ to 'Void') is 'Data.Void.absurd'.
unabsurd :: forall x. Variant '[] -> x
unabsurd = \case

-- * Folds

-- | Given a function that is polymorphic over a constraint held by all
-- possible values in a 'Variant', apply the function to whichever value
-- happens to be present. For example, we can @'foldC' @'Show' 'show'@ to get
-- the 'String' representation of whatever value is in a 'Variant' /as long as/
-- all values in the 'Variant' implement 'Show'.
type All :: (Type -> Constraint) -> [Type] -> Constraint
class All c xs where
  -- | A constraint applied to all members of @xs@.
  type AllC c xs :: Constraint

  -- | Apply a function to whatever value happens to be in a 'VAriant'.
  foldC :: forall r. (forall x. c x => x -> r) -> Variant xs -> r

instance (c x, All c xs) => All c (x ': xs) where
  type AllC c (x ': xs) = (c x, AllC c xs)
  foldC f = variant f (foldC @c f)

instance All c '[] where
  type AllC c '[] = ()
  foldC _ = \case

-- | Scott-style folding over a 'Variant'.
variant :: forall x xs r. (x -> r) -> (Variant xs -> r) -> Variant (x ': xs) -> r
variant here there = \case
  Here x -> here x
  There xs -> there xs

-- * Working with 'Variant'

-- | A witness that @x@ exists within @xs@, and so we can inject @x@ values
-- into a @'Variant' xs@, as well as project them out.
type Elem :: [Type] -> Type -> Constraint
class Elem xs x where

  -- | Lift a value of type @x@ into a 'Variant' of types @xs@.
  embed :: x -> Variant xs

  -- | Try to extract a value of type @x@ from a 'Variant' of types @xs@.
  project :: Variant xs -> Maybe x

instance Elem (x ': xs) x where
  embed = Here
  project = variant Just \_ -> Nothing

instance {-# OVERLAPPABLE #-} Elem xs x => Elem (y ': xs) x where
  embed = There . embed
  project = variant (error "The impossible happened!") project

-- | Pluck a value from a 'Variant', thus leaving us with either the value /or/
-- a smaller 'variant'.
type Pluck :: Type -> [Type] -> [Type] -> Constraint
class Pluck x xy ys | x xy -> ys, xy ys -> x where

  -- | Pluck a value from a 'Variant'.
  pluck :: Variant xy -> Either (Variant ys) x

instance xy ~ ys => Pluck x (x ': xy) ys where
  pluck = variant Right Left

instance {-# INCOHERENT #-} (Pluck x xy ys, a ~ b)
    => Pluck x (a ': xy) (b ': ys) where
  pluck = variant (Left . Here) (first There . pluck)

-- | 'Split' a 'Variant' into two halves. Typically useful when you want to
-- keep the latter half polymorphic.
type Split :: [Type] -> [Type] -> [Type] -> Constraint
class Split xy xs ys | xy xs -> ys where

  -- | Split a 'Variant' into two pieces.
  split :: Variant xy -> Either (Variant xs) (Variant ys)

instance (Pluck x xy ms, Split ms xs ys)
    => Split xy (x ': xs) ys where
  split = either (first There . split) (Left . Here) . pluck

instance xy ~ ys => Split xy '[] ys where
  split = Right
