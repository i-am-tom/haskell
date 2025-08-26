{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Test.CocartesianSpec where

import Control.Category.Constrained
import Control.Cocartesian.Constrained
import Data.Functor.Const (Const (..))
import Data.Functor.Contravariant (Op (..))
import GHC.Generics ((:+:) (..))
import Prelude hiding ((.), id)
import Test.Orphans ()
import Test.QuickCheck (Fun (..))

function :: Fun x y -> (x -> y)
function (Fun _ f) = f

prop_function_left_injection :: Fun Int String -> Fun Int String -> Int -> Bool
prop_function_left_injection (function -> f) (function -> g) x = ((f ▽ g) . inl) x == f x

prop_function_right_injection :: Fun Int String -> Fun Bool String -> Bool -> Bool
prop_function_right_injection (function -> f) (function -> g) y = ((f ▽ g) . inr) y == g y

prop_function_uniqueness :: Fun (Either Int Int) String -> Either Int Int -> Bool
prop_function_uniqueness (function -> f) x = (f . (inl ▽ inr)) x == f x

---

op :: Fun x y -> Op y x
op (Fun _ f) = Op f

prop_op_left_injection :: Fun Int String -> Fun Int String -> Int -> Bool
prop_op_left_injection (op -> f) (op -> g) x = getOp ((f ▽ g) . inl) x == getOp f x

prop_op_right_injection :: Fun Int String -> Fun Int String -> Int -> Bool
prop_op_right_injection (op -> f) (op -> g) x = getOp ((f ▽ g) . inr) x == getOp g x

prop_op_uniqueness :: Fun String (Int, Int) -> String -> Bool
prop_op_uniqueness (op -> f) x = getOp (f . (inl ▽ inr)) x == getOp f x

---

nt :: (Fun x y) -> (Const x ~> Const y)
nt (Fun _ f) = NT \(Const x) -> Const (f x)

prop_nt_left_injection :: Fun Int String -> Fun Int String -> Const Int Int -> Bool
prop_nt_left_injection (nt -> f) (nt -> g) x = runNT ((f ▽ g) . inl) x == runNT f x

prop_nt_right_injection :: Fun Int String -> Fun Int String -> Const Int Int -> Bool
prop_nt_right_injection (nt -> f) (nt -> g) x = runNT ((f ▽ g) . inr) x == runNT g x

prop_nt_uniqueness :: Fun (Either Int Int) Int -> (Const Int :+: Const Int) Int -> Bool
prop_nt_uniqueness f x = runNT (nt' f . (inl ▽ inr)) x == runNT (nt' f) x
  where
    nt' :: Fun (Either Int Int) Int -> (Const Int :+: Const Int) ~> Const Int
    nt' (Fun _ g) = NT \case
      L1 (Const i) -> Const (g (Left i))
      R1 (Const i) -> Const (g (Right i))