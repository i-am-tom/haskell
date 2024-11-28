{-# LANGUAGE BlockArguments #-}

module Data.ApproxSpec where

import Data.Approx (Approx (..))
import Hedgehog ((===), Gen, Property, diff, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

{- HLINT ignore "Use camelCase" -}

law_reflexivity :: (Approx x, Eq x, Show x) => Gen x -> Property
law_reflexivity xs = property do
  x <- forAll xs
  x === x

law_symmetry :: (Approx x, Show x) => Gen x -> Property
law_symmetry xs = property do
  x <- forAll xs
  y <- forAll xs

  (x ~= y) === (y ~= x)

law_roundtrip :: (Approx x, Floating x, Show x) => Gen x -> Property
law_roundtrip xs = property $ forAll xs >>= \x ->
  let y = (x * pi) / pi in diff x (~=) y

---

gen_double :: Gen Double
gen_double = Gen.double (Range.linearFrac 0 1000)

gen_float :: Gen Float
gen_float = Gen.float (Range.linearFrac 0 1000)

gen_rational :: Gen Rational
gen_rational = Gen.realFrac_ (Range.linearFrac 0 1000)

---

hprop_double_reflexivity :: Property
hprop_double_reflexivity = law_reflexivity gen_double

hprop_double_symmetry :: Property
hprop_double_symmetry = law_symmetry gen_double

hprop_double_roundtrip :: Property
hprop_double_roundtrip = law_roundtrip gen_double

hprop_float_reflexivity :: Property
hprop_float_reflexivity = law_reflexivity gen_float

hprop_float_symmetry :: Property
hprop_float_symmetry = law_symmetry gen_float

hprop_float_roundtrip :: Property
hprop_float_roundtrip = law_roundtrip gen_float

hprop_ratio_reflexivity :: Property
hprop_ratio_reflexivity = law_reflexivity gen_rational

hprop_ratio_symmetry :: Property
hprop_ratio_symmetry = law_symmetry gen_rational

hprop_maybe_reflexivity :: Property
hprop_maybe_reflexivity = law_reflexivity do
  Gen.maybe gen_double

hprop_maybe_symmetry :: Property
hprop_maybe_symmetry = law_symmetry do
  Gen.maybe gen_double

hprop_either_reflexivity :: Property
hprop_either_reflexivity = law_reflexivity do
  Gen.either gen_double gen_float

hprop_either_symmetry :: Property
hprop_either_symmetry = law_symmetry do
  Gen.either gen_double gen_float

hprop_duo_reflexivity :: Property
hprop_duo_reflexivity = law_reflexivity do
  liftA2 (,) gen_double gen_float

hprop_duo_symmetry :: Property
hprop_duo_symmetry = law_symmetry do
  liftA2 (,) gen_double gen_float

hprop_trio_reflexivity :: Property
hprop_trio_reflexivity = law_reflexivity do
  (,,) <$> gen_double <*> gen_float <*> gen_rational

hprop_trio_symmetry :: Property
hprop_trio_symmetry = law_symmetry do
  (,,) <$> gen_double <*> gen_float <*> gen_rational

hprop_quatro_reflexivity :: Property
hprop_quatro_reflexivity = law_reflexivity do
  (,,,) <$> gen_double <*> gen_float <*> gen_rational <*> gen_double

hprop_quatro_symmetry :: Property
hprop_quatro_symmetry = law_symmetry do
  (,,,) <$> gen_double <*> gen_float <*> gen_rational <*> gen_double
