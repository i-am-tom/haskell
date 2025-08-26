{-# LANGUAGE ViewPatterns #-}

module Test.StrongSpec where

import Control.Arrow (Kleisli (..))
import Control.Cartesian.Constrained (swap)
import Control.Category.Constrained
import Data.Functor.Const (Const (..))
import Data.Functor.Contravariant (Op (..))
import Data.Profunctor.Enriched (dimap)
import Data.Profunctor.Strong.Enriched (first', second')
import GHC.Generics ((:*:))
import Test.Orphans ()
import Test.QuickCheck (Fun (..))
import Prelude hiding (id, (.))

prop_function_first_swap :: Fun Int String -> (Int, String) -> Bool
prop_function_first_swap (Fun _ f) x = dimap swap swap (second' f) x == first' f x

prop_function_second_swap :: Fun Int String -> (String, Int) -> Bool
prop_function_second_swap (Fun _ f) x = dimap swap swap (first' f) x == second' f x

---

prop_op_first_swap :: Fun Int String -> Either Int String -> Bool
prop_op_first_swap f x = getOp (dimap swap swap (second' f')) x == getOp (first' f') x
  where
    Fun _ (Op -> f') = f

prop_op_second_swap :: Fun Int String -> Either String Int -> Bool
prop_op_second_swap f x = getOp (dimap swap swap (first' f')) x == getOp (second' f') x
  where
    Fun _ (Op -> f') = f

---

prop_kleisli_first_swap :: Fun Int String -> (Int, String) -> Bool
prop_kleisli_first_swap f x = runKleisli (dimap swap swap (second' f')) x == runKleisli (first' f') x
  where
    Fun _ (Kleisli -> f') = f

prop_kleisli_second_swap :: Fun Int String -> (String, Int) -> Bool
prop_kleisli_second_swap f x = runKleisli (dimap swap swap (first' f')) x == runKleisli (second' f') x
  where
    Fun _ (Kleisli -> f') = f

---

prop_nt_first_swap :: Fun Int String -> (Const Int :*: Const String) () -> Bool
prop_nt_first_swap (Fun _ f) x = runNT (dimap swap swap (second' f')) x == runNT (first' f') x
  where
    f' = NT (Const . f . getConst)

prop_nt_second_swap :: Fun Int String -> (Const String :*: Const Int) () -> Bool
prop_nt_second_swap (Fun _ f) x = runNT (dimap swap swap (first' f')) x == runNT (second' f') x
  where
    f' = NT (Const . f . getConst)
