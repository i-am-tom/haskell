{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Measurement.CoordinateSpec where

import Data.Approx (Approx ((~=)))
import Hedgehog (Gen, Property, diff, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Measurement.AngleSpec (gen_radians)
import Measurement.Coordinate
import Measurement.Helpers (law_roundtrip)

{-# ANN module "HLint: use camelCase" #-}

hprop_polar_origin_distance :: Property
hprop_polar_origin_distance = property do
  x@(Polar r _) <- forAll gen_polar
  diff (distance2D x origin2D) (~=) r

hprop_cylindrical_origin_distance :: Property
hprop_cylindrical_origin_distance = property do
  x@(Cylindrical r _ z) <- forAll gen_cylindrical
  diff (distance3D x origin3D) (~=) (sqrt (r * r + z * z))

hprop_spherical_origin_distance :: Property
hprop_spherical_origin_distance = property do
  x@(Spherical ρ _ _) <- forAll gen_spherical
  diff (distance3D x origin3D) (~=) ρ

---

hprop_cartesian_2d_polar :: Property
hprop_cartesian_2d_polar = law_roundtrip @Polar gen_cartesian_2d

hprop_polar_cartesian_2d :: Property
hprop_polar_cartesian_2d = law_roundtrip @Cartesian2D gen_polar

hprop_cartesian_3d_spherical :: Property
hprop_cartesian_3d_spherical = law_roundtrip @Spherical gen_cartesian_3d

hprop_cartesian_3d_cylindrical :: Property
hprop_cartesian_3d_cylindrical = law_roundtrip @Cylindrical gen_cartesian_3d

hprop_spherical_cartesian_3d :: Property
hprop_spherical_cartesian_3d = law_roundtrip @Cartesian3D gen_spherical

hprop_spherical_cylindrical :: Property
hprop_spherical_cylindrical = law_roundtrip @Cylindrical gen_spherical

hprop_cylindrical_cartesian_3d :: Property
hprop_cylindrical_cartesian_3d = law_roundtrip @Cartesian3D gen_cylindrical

hprop_cylindrical_spherical :: Property
hprop_cylindrical_spherical = law_roundtrip @Spherical gen_cylindrical

---

gen_cartesian_2d :: Gen Cartesian2D
gen_cartesian_2d = do
  x <- Gen.double (Range.linearFrac 0 100)
  y <- Gen.double (Range.linearFrac 0 100)

  pure Cartesian2D{ x, y }

gen_polar :: Gen Polar
gen_polar = do
  r <- Gen.double (Range.linearFrac 0 100)
  θ <- gen_radians

  pure Polar{ r, θ }

gen_cartesian_3d :: Gen Cartesian3D
gen_cartesian_3d = do
  x <- Gen.double (Range.linearFrac 0 100)
  y <- Gen.double (Range.linearFrac 0 100)
  z <- Gen.double (Range.linearFrac 0 100)

  pure Cartesian3D { x, y, z }

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
