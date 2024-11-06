{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}

module Measurement.AngleSpec where

import Hedgehog (Gen, Property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Measurement.Angle
import Measurement.Helpers (law_roundtrip, shouldBeApprox)
import Test.Hspec (Spec)
import Test.Hspec (describe, it)

spec_angle_zero :: Spec
spec_angle_zero = do
  describe "degrees" do
    it "degrees / grades"  do 0 degrees `shouldBeApprox` 0 grades
    it "degrees / radians" do 0 degrees `shouldBeApprox` 0 radians
    it "degrees / turns"   do 0 degrees `shouldBeApprox` 0 turns

  describe "grades" do
    it "grades / degrees" do 0 grades `shouldBeApprox` 0 degrees
    it "grades / radians" do 0 grades `shouldBeApprox` 0 radians
    it "grades / turns"   do 0 grades `shouldBeApprox` 0 turns

  describe "radians" do
    it "radians / degrees" do 0 radians `shouldBeApprox` 0 degrees
    it "radians / radians" do 0 radians `shouldBeApprox` 0 radians
    it "radians / turns"   do 0 radians `shouldBeApprox` 0 turns

  describe "turns" do
    it "turns / degrees" do 0 turns `shouldBeApprox` 0 degrees
    it "turns / radians" do 0 turns `shouldBeApprox` 0 radians
    it "turns / turns"   do 0 turns `shouldBeApprox` 0 turns

spec_angle_full_rotation :: Spec
spec_angle_full_rotation = do
  describe "degrees" do
    it "degrees / grades"  do 360 degrees `shouldBeApprox` 400 grades
    it "degrees / radians" do 360 degrees `shouldBeApprox` (2 * pi) radians
    it "degrees / turns"   do 360 degrees `shouldBeApprox` 1 turn

  describe "grades" do
    it "grades / degrees" do 400 grades `shouldBeApprox` 360 degrees
    it "grades / radians" do 400 grades `shouldBeApprox` (2 * pi) radians
    it "grades / turns"   do 400 grades `shouldBeApprox` 1 turn

  describe "radians" do
    it "radians / degrees" do (2 * pi) radians `shouldBeApprox` 360 degrees
    it "radians / grades"  do (2 * pi) radians `shouldBeApprox` 400 grades
    it "radians / turns"   do (2 * pi) radians `shouldBeApprox` 1 turn

  describe "turns" do
    it "turns / degrees" do 1 turn `shouldBeApprox` 360 degrees
    it "turns / grades"  do 1 turn `shouldBeApprox` 400 grades
    it "turns / radians" do 1 turn `shouldBeApprox` 1 turn

---

hprop_degrees_grades :: Property
hprop_degrees_grades = law_roundtrip @Degrees gen_grades

hprop_degrees_radians :: Property
hprop_degrees_radians = law_roundtrip @Degrees gen_radians

hprop_degrees_turns :: Property
hprop_degrees_turns = law_roundtrip @Degrees gen_turns

hprop_grades_degrees :: Property
hprop_grades_degrees = law_roundtrip @Grades gen_degrees

hprop_grades_radians :: Property
hprop_grades_radians = law_roundtrip @Grades gen_radians

hprop_grades_turns :: Property
hprop_grades_turns = law_roundtrip @Grades gen_turns

hprop_radians_degrees :: Property
hprop_radians_degrees = law_roundtrip @Radians gen_degrees

hprop_radians_grades :: Property
hprop_radians_grades = law_roundtrip @Radians gen_grades

hprop_radians_turns :: Property
hprop_radians_turns = law_roundtrip @Radians gen_turns

hprop_turns_degrees :: Property
hprop_turns_degrees = law_roundtrip @Turns gen_degrees

hprop_turns_grades :: Property
hprop_turns_grades = law_roundtrip @Turns gen_grades

hprop_turns_radians :: Property
hprop_turns_radians = law_roundtrip @Turns gen_radians

---

gen_degrees :: Gen Degrees
gen_degrees = fmap Degrees (Gen.double (Range.linearFrac 0 1000))

gen_grades :: Gen Grades
gen_grades = fmap Grades (Gen.double (Range.linearFrac 0 1000))

gen_radians :: Gen Radians
gen_radians = fmap Radians (Gen.double (Range.linearFrac 0 1000))

gen_turns :: Gen Turns
gen_turns = fmap Turns (Gen.double (Range.linearFrac 0 1000))
