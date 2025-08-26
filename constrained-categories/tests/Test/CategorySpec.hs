{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

module Test.CategorySpec where

import Control.Arrow (Kleisli (..))
import Control.Category.Constrained
import Data.Functor.Contravariant (Op (..))
import Data.Functor.Const (Const (..))
import Prelude hiding (id, (.))
import Test.Orphans ()
import Test.QuickCheck (Fun (..))

function :: Fun x y -> (x -> y)
function (Fun _ f) = f

prop_function_associativity :: Fun Int Bool -> Fun String Int -> Fun Double String -> Double -> Bool
prop_function_associativity (function -> f) (function -> g) (function -> h) x
  = ((f . g) . h) x == (f . (g . h)) x

prop_function_left_identity :: Fun Int Bool -> Int -> Bool
prop_function_left_identity (function -> f) x = (id . f) x == f x

prop_function_right_identity :: Fun Int Bool -> Int -> Bool
prop_function_right_identity (function -> f) x = (f . id) x == f x

---

op :: Fun x y -> Op y x
op (Fun _ f) = Op f

prop_op_associativity :: Fun Bool Int -> Fun Int String -> Fun String Double -> Bool -> Bool
prop_op_associativity (op -> f) (op -> g) (op -> h) x = getOp ((f . g) . h) x == getOp (f . (g . h)) x

prop_op_left_identity :: Fun Bool Int -> Bool -> Bool
prop_op_left_identity (op -> f) x = getOp (id . f) x == getOp (f) x

prop_op_right_identity :: Fun Bool Int -> Bool -> Bool
prop_op_right_identity (op -> f) x = getOp (f . id) x == getOp (f) x

---

kleisli :: Fun x (Maybe y) -> Kleisli Maybe x y
kleisli (Fun _ f) = Kleisli f

prop_kleisli_associativity :: Fun Int (Maybe String) -> Fun String (Maybe Int) -> Fun Double (Maybe String) -> Double -> Bool
prop_kleisli_associativity (kleisli -> f) (kleisli -> g) (kleisli -> h) x
  = runKleisli ((f . g) . h) x == runKleisli (f . (g . h)) x

prop_kleisli_left_identity :: Fun Int (Maybe String) -> Int -> Bool
prop_kleisli_left_identity (kleisli -> f) x = runKleisli (id . f) x == runKleisli (f) x

prop_kleisli_right_identity :: Fun Int (Maybe String) -> Int -> Bool
prop_kleisli_right_identity (kleisli -> f) x = runKleisli (f . id) x == runKleisli (f) x

---

nt :: (Fun x y) -> (Const x ~> Const y)
nt (Fun _ f) = NT \(Const x) -> Const (f x)

prop_nt_associativity :: (Fun Int String) -> (Fun String Int) -> (Fun Double String) -> Double -> Bool
prop_nt_associativity (nt -> f) (nt -> g) (nt -> h) x
  = runNT ((f . g) . h) (Const x) == runNT (f . (g . h)) (Const x)

prop_nt_left_identity :: (Fun Int String) -> Int -> Bool
prop_nt_left_identity (nt -> f) x = runNT (id . f) (Const x) == runNT (f) (Const x)

prop_nt_right_identity :: (Fun Int String) -> Int -> Bool
prop_nt_right_identity (nt -> f) x = runNT (f . id) (Const x) == runNT (f) (Const x)
