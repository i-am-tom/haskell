{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

module Test.CartesianSpec where

import Control.Cartesian.Constrained
import Control.Category.Constrained
import Data.Functor.Const (Const (..))
import Data.Functor.Contravariant (Op (..))
import GHC.Generics ((:*:) (..))
import Test.Orphans ()
import Prelude hiding (id, (.))
import Test.QuickCheck (Fun (..))

function :: Fun x y -> (x -> y)
function (Fun _ f) = f

prop_function_left_projection :: Fun Int String -> Fun Int Bool -> Int -> Bool
prop_function_left_projection (function -> f) (function -> g) x = (exl . (f △ g)) x == f x

prop_function_right_projection :: Fun Int String -> Fun Int Bool -> Int -> Bool
prop_function_right_projection (function -> f) (function -> g) x = (exr . (f △ g)) x == g x

prop_function_uniqueness :: Fun Int (String, Bool) -> Int -> Bool
prop_function_uniqueness (function -> f) x = ((exl △ exr) . f) x == f x

---

op :: Fun x y -> Op y x
op (Fun _ f) = Op f

prop_op_left_projection :: Fun String Int -> Fun Bool Int -> String -> Bool
prop_op_left_projection (op -> f) (op -> g) x = getOp (exl . (f △ g)) x == getOp f x

prop_op_right_projection :: Fun String Int -> Fun Bool Int -> Bool -> Bool
prop_op_right_projection (op -> f) (op -> g) x = getOp (exr . (f △ g)) x == getOp g x

prop_op_uniqueness :: Fun (Either Int Int) String -> Either Int Int -> Bool
prop_op_uniqueness (op -> f) x = getOp ((exl △ exr) . f) x == getOp f x

---

nt :: (Fun x y) -> (Const x ~> Const y)
nt (Fun _ f) = NT \(Const x) -> Const (f x)

prop_nt_left_projection :: Fun Int String -> Fun Int Bool -> Const Int Int -> Bool
prop_nt_left_projection (nt -> f) (nt -> g) x = runNT (exl . (f △ g)) x == runNT f x

prop_nt_right_projection :: Fun Int String -> Fun Int Bool -> Const Int Int -> Bool
prop_nt_right_projection (nt -> f) (nt -> g) x = runNT (exr . (f △ g)) x == runNT g x

prop_nt_uniqueness :: Fun Int (String, Bool) -> Int -> Bool
prop_nt_uniqueness f (Const -> x) = runNT ((exl △ exr) . (nt' f)) x == runNT (nt' f) x
  where
    nt' :: Fun Int (String, Bool) -> Const Int ~> (Const String :*: Const Bool)
    nt' (Fun _ g) = NT \(Const i) -> let (s, b) = g i in Const s :*: Const b