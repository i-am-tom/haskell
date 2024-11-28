{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Measurement.Coordinate.SpaceSpec where

import Data.Approx (Approx ((~=)))
import Hedgehog (Gen, Property, diff, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Measurement.AngleSpec (gen_radians)
import Measurement.Coordinate.Space
import Measurement.Helpers (law_roundtrip)

{- HLINT ignore "Use camelCase" -}

hprop_cylindrical_origin_distance :: Property
hprop_cylindrical_origin_distance = property do
  x@(Cylindrical r _ z) <- forAll gen_cylindrical
  diff (distance x origin) (~=) (sqrt (r * r + z * z))

hprop_spherical_origin_distance :: Property
hprop_spherical_origin_distance = property do
  x@(Spherical ρ _ _) <- forAll gen_spherical
  diff (distance x origin) (~=) ρ

hprop_cartesian_3d_spherical :: Property
hprop_cartesian_3d_spherical = law_roundtrip @Spherical gen_cartesian_3d

hprop_cartesian_3d_cylindrical :: Property
hprop_cartesian_3d_cylindrical = law_roundtrip @Cylindrical gen_cartesian_3d

hprop_spherical_cartesian_3d :: Property
hprop_spherical_cartesian_3d = law_roundtrip @Cartesian gen_spherical

hprop_spherical_cylindrical :: Property
hprop_spherical_cylindrical = law_roundtrip @Cylindrical gen_spherical

hprop_cylindrical_cartesian_3d :: Property
hprop_cylindrical_cartesian_3d = law_roundtrip @Cartesian gen_cylindrical

hprop_cylindrical_spherical :: Property
hprop_cylindrical_spherical = law_roundtrip @Spherical gen_cylindrical

gen_cartesian_3d :: Gen Cartesian
gen_cartesian_3d = do
  x <- Gen.double (Range.linearFrac 0 100)
  y <- Gen.double (Range.linearFrac 0 100)
  z <- Gen.double (Range.linearFrac 0 100)

  pure Cartesian { x, y, z }

gen_cylindrical :: Gen Cylindrical
gen_cylindrical = do
  r <- Gen.double (Range.linearFrac 0 100)
  θ <- gen_radians
  z <- Gen.double (Range.linearFrac 0 100)

  pure Cylindrical { r, θ, z }

gen_spherical :: Gen Spherical
gen_spherical = do
  ρ <- Gen.double (Range.linearFrac 0 100)
  θ <- gen_radians
  φ <- gen_radians

  pure Spherical { ρ, θ, φ }
