{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Data.Constraint.Generic where

import Data.Constraint.Helpers (Every')
import Data.Kind (Constraint, Type)
import GHC.Generics

type Leaves :: (Type -> Type) -> [Type]
type family Leaves rep where
  Leaves (K1 i x) = '[x]
  Leaves (M1 i m x) = Leaves x
  Leaves (l :*: r) = Leaves l ++ Leaves r
  Leaves (l :+: r) = Leaves l ++ Leaves r
  Leaves U1 = '[]
  Leaves V1 = '[]

type (++) :: [k] -> [k] -> [k]
type family xs ++ ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

type LeavesAre :: Type -> (Type -> Constraint) -> Constraint
type LeavesAre s c = Every' c (Leaves (Rep s))
