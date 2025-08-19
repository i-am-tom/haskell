{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}

module Test.CategorySpec where

import Control.Arrow (Kleisli (..))
import Control.Category.Constrained
import Control.Monad.State (State, runState, state)
import Data.Coerce (coerce)
import Data.Functor.Contravariant (Op (..))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Prelude hiding (id, (.))

law_associativity ::
  (Category' k, Obj k x, Show i) =>
  (k x x -> k x x -> PropertyT IO ()) ->
  (i -> k x x) ->
  Gen i ->
  Property
law_associativity (=~=) prepare gen = property do
  f <- fmap prepare (forAll gen)
  g <- fmap prepare (forAll gen)
  h <- fmap prepare (forAll gen)

  let left = f . (g . h)
      right = (f . g) . h

  left =~= right

law_left_identity ::
  (Category' k, Obj k x, Show i) =>
  (k x x -> k x x -> PropertyT IO ()) ->
  (i -> k x x) ->
  Gen i ->
  Property
law_left_identity (=~=) prepare gen = property do
  fmap prepare (forAll gen) >>= \f -> (f . id) =~= f

law_right_identity ::
  (Category' k, Obj k x, Show i) =>
  (k x x -> k x x -> PropertyT IO ()) ->
  (i -> k x x) ->
  Gen i ->
  Property
law_right_identity (=~=) prepare gen = property do
  fmap prepare (forAll gen) >>= \f -> (id . f) =~= f

---

verify_function :: (Int -> Int) -> (Int -> Int) -> PropertyT IO ()
verify_function f g = forAll gen_int >>= \x -> f x === g x

gen_int :: Gen Int
gen_int = Gen.int (Range.linear 0 1000)

hprop_function_associativity :: Property
hprop_function_associativity = law_associativity verify_function (+) gen_int

hprop_function_left_identity :: Property
hprop_function_left_identity = law_left_identity verify_function (+) gen_int

hprop_function_right_identity :: Property
hprop_function_right_identity = law_right_identity verify_function (+) gen_int

---

verify_op :: Op Int Int -> Op Int Int -> PropertyT IO ()
verify_op (Op f) (Op g) = forAll gen_int >>= \x -> f x === g x

prepare_op :: Int -> Op Int Int
prepare_op = coerce ((+) @Int)

hprop_op_associativity :: Property
hprop_op_associativity = law_associativity verify_op prepare_op gen_int

hprop_op_left_identity :: Property
hprop_op_left_identity = law_left_identity verify_op prepare_op gen_int

hprop_op_right_identity :: Property
hprop_op_right_identity = law_right_identity verify_op prepare_op gen_int

---

verify_kleisli :: Kleisli Maybe Int Int -> Kleisli Maybe Int Int -> PropertyT IO ()
verify_kleisli (Kleisli f) (Kleisli g) = forAll gen_int >>= \x -> f x === g x

prepare_kleisli :: Int -> Kleisli Maybe Int Int
prepare_kleisli x = Kleisli \y -> if x `mod` 2 == 0 then Nothing else Just y

hprop_kleisli_associativity :: Property
hprop_kleisli_associativity = law_associativity verify_kleisli prepare_kleisli gen_int

hprop_kleisli_left_identity :: Property
hprop_kleisli_left_identity = law_left_identity verify_kleisli prepare_kleisli gen_int

hprop_kleisli_right_identity :: Property
hprop_kleisli_right_identity = law_right_identity verify_kleisli prepare_kleisli gen_int

---

verify_nt :: (State Int ~> State Int) -> (State Int ~> State Int) -> PropertyT IO ()
verify_nt (NT f) (NT g) = runState (f (pure "hi")) 0 === runState (g (pure "hi")) 0

prepare_nt :: Int -> (State Int ~> State Int)
prepare_nt x = NT \k -> k >>= \y -> state \z -> (y, x + z)

hprop_nt_associativity :: Property
hprop_nt_associativity = law_associativity verify_nt prepare_nt gen_int

hprop_nt_left_identity :: Property
hprop_nt_left_identity = law_left_identity verify_nt prepare_nt gen_int

hprop_nt_right_identity :: Property
hprop_nt_right_identity = law_right_identity verify_nt prepare_nt gen_int
