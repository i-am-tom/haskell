{-# LANGUAGE ViewPatterns #-}

module Test.ChoiceSpec where

import Control.Arrow (Kleisli (..))
import Control.Category.Constrained
import Control.Cocartesian.Constrained (swap)
import Data.Functor.Const (Const (..))
import Data.Functor.Contravariant (Op (..))
import Data.Profunctor.Choice.Enriched (left', right')
import Data.Profunctor.Enriched (dimap)
import GHC.Generics ((:+:))
import Test.Orphans ()
import Test.QuickCheck (Fun (..))
import Prelude hiding (id, (.))

prop_function_left_swap :: Fun Int String -> Either Int String -> Bool
prop_function_left_swap (Fun _ f) x = dimap swap swap (right' f) x == left' f x

prop_function_right_swap :: Fun Int String -> Either String Int -> Bool
prop_function_right_swap (Fun _ f) x = dimap swap swap (left' f) x == right' f x

---

prop_op_left_swap :: Fun Int String -> (Int, String) -> Bool
prop_op_left_swap f x = getOp (dimap swap swap (right' f')) x == getOp (left' f') x
  where
    Fun _ (Op -> f') = f

prop_op_right_swap :: Fun Int String -> (String, Int) -> Bool
prop_op_right_swap f x = getOp (dimap swap swap (left' f')) x == getOp (right' f') x
  where
    Fun _ (Op -> f') = f

---

prop_kleisli_left_swap :: Fun Int String -> Either Int String -> Bool
prop_kleisli_left_swap f x = runKleisli (dimap swap swap (right' f')) x == runKleisli (left' f') x
  where
    Fun _ (Kleisli -> f') = f

prop_kleisli_right_swap :: Fun Int String -> Either String Int -> Bool
prop_kleisli_right_swap f x = runKleisli (dimap swap swap (left' f')) x == runKleisli (right' f') x
  where
    Fun _ (Kleisli -> f') = f

---

prop_nt_left_swap :: Fun Int String -> (Const Int :+: Const String) () -> Bool
prop_nt_left_swap (Fun _ f) x = runNT (dimap swap swap (right' f')) x == runNT (left' f') x
  where
    f' = NT (Const . f . getConst)

prop_nt_right_swap :: Fun Int String -> (Const String :+: Const Int) () -> Bool
prop_nt_right_swap (Fun _ f) x = runNT (dimap swap swap (left' f')) x == runNT (right' f') x
  where
    f' = NT (Const . f . getConst)
