{-# LANGUAGE BlockArguments #-}

module Measurement.Coordinate.PlaneSpec where

import Data.Approx (Approx ((~=)))
import Hedgehog (Gen, Property, diff, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Measurement.AngleSpec (gen_radians)
import Measurement.Coordinate.Plane
import Measurement.Helpers (law_roundtrip)

{- HLINT ignore "Use camelCase" -}

hprop_polar_origin_distance :: Property
hprop_polar_origin_distance = property do
  x@(Polar r _) <- forAll gen_polar
  diff (distance x origin) (~=) r

hprop_cartesian_2d_polar :: Property
hprop_cartesian_2d_polar = law_roundtrip @Polar gen_cartesian_2d

hprop_polar_cartesian_2d :: Property
hprop_polar_cartesian_2d = law_roundtrip @Cartesian gen_polar

gen_cartesian_2d :: Gen Cartesian
gen_cartesian_2d = do
  x <- Gen.double (Range.linearFrac 0 100)
  y <- Gen.double (Range.linearFrac 0 100)

  pure Cartesian {x, y}

gen_polar :: Gen Polar
gen_polar = do
  r <- Gen.double (Range.linearFrac 0 100)
  θ <- gen_radians

  pure Polar {r, θ}
