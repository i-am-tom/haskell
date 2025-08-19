{-# LANGUAGE BlockArguments #-}

module Test.CartesianSpec where

import Control.Cartesian.Constrained
import Control.Category.Constrained
import Control.Cocartesian.Constrained (flatten)
import Control.Monad.State (State, modify, runState)
import Data.Functor.Contravariant (Op (..))
import GHC.Generics ((:*:) (..))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Test.CategorySpec (gen_int, prepare_op)
import Prelude hiding (id, (.))

law_left_projection ::
  (Cartesian c p k, c x, c y, Show i) =>
  (k x y -> k x y -> PropertyT IO ()) ->
  (i -> k x y) ->
  Gen i ->
  Property
law_left_projection (=~=) prepare gen = property do
  f <- fmap prepare (forAll gen)
  g <- fmap prepare (forAll gen)

  (exl . (f △ g)) =~= f

law_right_projection ::
  (Cartesian c p k, c x, c y, Show i) =>
  (k x y -> k x y -> PropertyT IO ()) ->
  (i -> k x y) ->
  Gen i ->
  Property
law_right_projection (=~=) prepare gen = property do
  f <- fmap prepare (forAll gen)
  g <- fmap prepare (forAll gen)

  (exr . (f △ g)) =~= g

law_uniqueness ::
  (Cartesian c p k, c x, c y, Show i) =>
  (k x (p y y) -> k x (p y y) -> PropertyT IO ()) ->
  (i -> k x y) ->
  Gen i ->
  Property
law_uniqueness (=~=) prepare gen = property do
  f <- fmap prepare (forAll gen)
  g <- fmap prepare (forAll gen)

  let h = f △ g
  ((exl . h) △ (exr . h)) =~= h

---

verify_function :: (Int -> (Int, Int)) -> (Int -> (Int, Int)) -> PropertyT IO ()
verify_function f g = forAll gen_int >>= \x -> f x === g x

prepare_function :: Int -> Int -> (Int, Int)
prepare_function x y = (x + y, x - y)

hprop_function_left_projection :: Property
hprop_function_left_projection = law_left_projection verify_function prepare_function gen_int

hprop_function_right_projection :: Property
hprop_function_right_projection = law_right_projection verify_function prepare_function gen_int

hprop_function_uniqueness :: Property
hprop_function_uniqueness = law_uniqueness verify_function (+) gen_int

---

verify_op :: Op Int (Either Int Int) -> Op Int (Either Int Int) -> PropertyT IO ()
verify_op (Op f) (Op g) = forAll (Gen.either gen_int gen_int) >>= \x -> f x === g x

prepare_op' :: Int -> Op Int (Either Int Int)
prepare_op' x = Op \y -> x + flatten y

hprop_op_left_projection :: Property
hprop_op_left_projection = law_left_projection verify_op prepare_op' gen_int

hprop_op_right_projection :: Property
hprop_op_right_projection = law_right_projection verify_op prepare_op' gen_int

hprop_op_uniqueness :: Property
hprop_op_uniqueness = law_uniqueness verify_op prepare_op gen_int

---

verify_nt :: (State Int ~> (State Int :*: State Int)) -> (State Int ~> (State Int :*: State Int)) -> PropertyT IO ()
verify_nt (NT f) (NT g) = go (f (pure "hi")) === go (g (pure "hi"))
  where
    go (x :*: y) = (runState x 0, runState y 0)

prepare_nt :: Int -> (State Int ~> (State Int :*: State Int))
prepare_nt x = NT \s -> (go x s :*: go (-x) s)
  where
    go d s = s <* modify (+ d)

hprop_nt_left_projection :: Property
hprop_nt_left_projection = law_left_projection verify_nt prepare_nt gen_int

hprop_nt_right_projection :: Property
hprop_nt_right_projection = law_left_projection verify_nt prepare_nt gen_int

hprop_nt_uniqueness :: Property
hprop_nt_uniqueness = law_left_projection verify_nt prepare_nt gen_int
