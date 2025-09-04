{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Helper functions for constraints.
module Data.Constraint.Helpers where

import Data.Kind (Constraint)

-- | Apply a list of constraints to a value.
type All :: [k -> Constraint] -> k -> Constraint
class All cs x

instance All '[] x

instance (c x, All cs x) => All (c ': cs) x

-- | Apply a constraint to every value in a list.
type Every :: (k -> Constraint) -> [k] -> Constraint
class Every c xs

instance Every c '[]

instance (c x, Every c xs) => Every c (x ': xs)

-- | @Every@ as a type family.
type Every' :: (k -> Constraint) -> [k] -> Constraint
type family Every' c xs where
  Every' c '[] = ()
  Every' c (x ': xs) = (c x, Every' c xs)

-- | Flip a constraint.
type Flip :: (j -> k -> Constraint) -> k -> j -> Constraint
class (c y x) => Flip c x y

instance (c y x) => Flip c x y
