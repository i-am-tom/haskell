{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Test.CocartesianSpec where

import Control.Category.Constrained
import Control.Cocartesian.Constrained
import Control.Monad.State (State, modify, runState)
import Data.Functor.Contravariant (Op (..))
import GHC.Generics ((:+:) (..))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Test.CategorySpec (gen_int, prepare_op)
import Prelude hiding (id, (.))

law_left_injection ::
  (Cocartesian c s k, c x, c y, Show i) =>
  (k x y -> k x y -> PropertyT IO ()) ->
  (i -> k x y) ->
  Gen i ->
  Property
law_left_injection (=~=) prepare gen = property do
  f <- fmap prepare (forAll gen)
  g <- fmap prepare (forAll gen)

  ((f ▽ g) . inl) =~= f

law_right_injection ::
  (Cocartesian c s k, c x, c y, Show i) =>
  (k x y -> k x y -> PropertyT IO ()) ->
  (i -> k x y) ->
  Gen i ->
  Property
law_right_injection (=~=) prepare gen = property do
  f <- fmap prepare (forAll gen)
  g <- fmap prepare (forAll gen)

  ((f ▽ g) . inr) =~= g

law_uniqueness ::
  (Cocartesian c s k, c x, c y, Show i) =>
  (k (s x x) y -> k (s x x) y -> PropertyT IO ()) ->
  (i -> k x y) ->
  Gen i ->
  Property
law_uniqueness (=~=) prepare gen = property do
  f <- fmap prepare (forAll gen)
  g <- fmap prepare (forAll gen)

  let h = f ▽ g
  ((h . inl) ▽ (h . inr)) =~= h

---

verify_function :: (Either Int Int -> Int) -> (Either Int Int -> Int) -> PropertyT IO ()
verify_function f g = forAll (Gen.either gen_int gen_int) >>= \x -> f x === g x

prepare_function :: Int -> Either Int Int -> Int
prepare_function x = either (+ x) (* x)

hprop_function_left_injection :: Property
hprop_function_left_injection = law_left_injection verify_function prepare_function gen_int

hprop_function_right_injection :: Property
hprop_function_right_injection = law_right_injection verify_function prepare_function gen_int

hprop_function_uniqueness :: Property
hprop_function_uniqueness = law_uniqueness verify_function (+) gen_int

---

verify_op :: Op (Int, Int) Int -> Op (Int, Int) Int -> PropertyT IO ()
verify_op (Op f) (Op g) = forAll gen_int >>= \x -> f x === g x

prepare_op' :: Int -> Op (Int, Int) Int
prepare_op' x = Op \y -> (x + y, x - y)

hprop_op_left_injection :: Property
hprop_op_left_injection = law_left_injection verify_op prepare_op' gen_int

hprop_op_right_injection :: Property
hprop_op_right_injection = law_right_injection verify_op prepare_op' gen_int

hprop_op_uniqueness :: Property
hprop_op_uniqueness = law_uniqueness verify_op prepare_op gen_int

---

verify_nt :: ((State Int :+: State Int) ~> State Int) -> ((State Int :+: State Int) ~> State Int) -> PropertyT IO ()
verify_nt (NT f) (NT g) = do
  inner <-
    forAll Gen.bool
      >>= pure . \case
        True -> L1 (pure "hi")
        False -> R1 (pure "bye")

  runState (f inner) 0 === runState (g inner) 0

prepare_nt :: Int -> ((State Int :+: State Int) ~> State Int)
prepare_nt x = NT \case
  L1 s -> s <* modify (+ x)
  R1 s -> s <* modify (* x)

hprop_nt_left_injection :: Property
hprop_nt_left_injection = law_left_injection verify_nt prepare_nt gen_int

hprop_nt_right_injection :: Property
hprop_nt_right_injection = law_left_injection verify_nt prepare_nt gen_int

hprop_nt_uniqueness :: Property
hprop_nt_uniqueness = law_left_injection verify_nt prepare_nt gen_int
