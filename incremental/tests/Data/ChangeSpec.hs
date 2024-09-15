{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.ChangeSpec where

import Data.Change
import Data.Kind (Type)
import GHC.Generics
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

law_change :: (Change x, Eq x, Show x) => Gen x -> Property
law_change gen = property do
  x <- forAll gen
  y <- forAll gen

  x <~ (y \\ x) === y

law_action_identity :: (Change x, Eq x, Show x) => Gen x -> Property
law_action_identity gen = property do
  x <- forAll gen
  x <~ mempty === x

law_action_composition :: (Change x, Eq x, Show x) => Gen x -> Property
law_action_composition gen = property do
  x <- forAll gen
  y <- forAll gen
  z <- forAll gen

  z <~ (y \\ z) <~ (x \\ y)
    === z <~ ((y \\ z) <> (x \\ y))

---

law_function_change
  :: (Show x, Change y, Show y, Change z, Eq z, Show z)
  => Gen x -> Gen y -> (x -> y -> z) -> Property
law_function_change gen_x gen_y k = property do
  f <- fmap k (forAll gen_x)
  g <- fmap k (forAll gen_x)
  x <- forAll gen_y

  (f <~ (g \\ f)) x === g x

law_function_action_identity
  :: (Show x, Change y, Show y, Change z, Eq z, Show z)
  => Gen x -> Gen y -> (x -> y -> z) -> Property
law_function_action_identity gen_x gen_y k = property do
  f <- fmap k (forAll gen_x)
  x <- forAll gen_y

  (f <~ mempty) x === f x

law_function_action_composition
  :: (Show x, Change y, Show y, Change z, Eq z, Show z)
  => Gen x -> Gen y -> (x -> y -> z) -> Property
law_function_action_composition gen_x gen_y k = property do
  f <- fmap k (forAll gen_x)
  g <- fmap k (forAll gen_x)
  h <- fmap k (forAll gen_x)
  x <- forAll gen_y

  let gh = g \\ h
      fg = f \\ g

  (h <~ gh <~ fg) x === (h <~ (gh <> fg)) x

---

hprop_unit_change :: Property
hprop_unit_change = law_change (pure ())

hprop_unit_action_identity :: Property
hprop_unit_action_identity = law_action_identity (pure ())

hprop_unit_action_composition :: Property
hprop_unit_action_composition = law_action_composition (pure ())

---

hprop_bool_change :: Property
hprop_bool_change = law_change Gen.bool

hprop_bool_action_identity :: Property
hprop_bool_action_identity = law_action_identity Gen.bool

hprop_bool_action_composition :: Property
hprop_bool_action_composition = law_action_composition Gen.bool

---

gen_int :: Gen Int
gen_int = Gen.int (Range.linear 0 1000)

hprop_int_change :: Property
hprop_int_change = law_change gen_int

hprop_int_action_identity :: Property
hprop_int_action_identity = law_action_identity gen_int

hprop_int_action_composition :: Property
hprop_int_action_composition = law_action_composition gen_int

---

gen_pair :: Gen (Int, Bool)
gen_pair = liftA2 (,) gen_int Gen.bool

hprop_pair_change :: Property
hprop_pair_change = law_change gen_pair

hprop_pair_action_identity :: Property
hprop_pair_action_identity = law_action_identity gen_pair

hprop_pair_action_composition :: Property
hprop_pair_action_composition = law_action_composition gen_pair

---

gen_either :: Gen (Either Int Bool)
gen_either = Gen.choice [ fmap Left gen_int, fmap Right Gen.bool ]

hprop_either_change :: Property
hprop_either_change = law_change gen_either

hprop_either_action_identity :: Property
hprop_either_action_identity = law_action_identity gen_either

hprop_either_action_composition :: Property
hprop_either_action_composition = law_action_composition gen_either

---

type State :: Type
data State = State { this :: Int, that :: Int }
  deriving stock (Eq, Ord, Show)
  deriving stock (Generic)
  deriving Change via (Generically State)

gen_state :: Gen State
gen_state = liftA2 State gen_int gen_int

hprop_state_change :: Property
hprop_state_change = law_change gen_state

hprop_state_action_identity :: Property
hprop_state_action_identity = law_action_identity gen_state

hprop_state_action_composition :: Property
hprop_state_action_composition = law_action_composition gen_state

---

hprop_bool_to_bool_change :: Property
hprop_bool_to_bool_change
  = law_function_change Gen.bool Gen.bool (/=)

hprop_bool_to_bool_action_identity :: Property
hprop_bool_to_bool_action_identity
  = law_function_action_identity Gen.bool Gen.bool (/=)

hprop_bool_to_bool_action_composition :: Property
hprop_bool_to_bool_action_composition
  = law_function_action_composition Gen.bool Gen.bool (/=)

---

hprop_int_to_int_change :: Property
hprop_int_to_int_change
  = law_function_change gen_int gen_int (+)

hprop_int_to_int_action_identity :: Property
hprop_int_to_int_action_identity
  = law_function_action_identity gen_int gen_int (+)

hprop_int_to_int_action_composition :: Property
hprop_int_to_int_action_composition
  = law_function_action_composition gen_int gen_int (+)

---

hprop_int_to_bool_change :: Property
hprop_int_to_bool_change
  = law_function_change gen_int gen_int (==)

hprop_int_to_bool_action_identity :: Property
hprop_int_to_bool_action_identity
  = law_function_action_identity gen_int gen_int (==)

hprop_int_to_bool_action_composition :: Property
hprop_int_to_bool_action_composition
  = law_function_action_composition gen_int gen_int (==)
