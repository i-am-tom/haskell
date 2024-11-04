{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Kind (Type)
import Postfix
import Postfix.Instances.Orphans ()
import Test.Hspec (describe, hspec, it, shouldBe)
import Witch

type Hours :: Type
newtype Hours = Hours { unHours :: Double }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Fractional, Floating)

type Minutes :: Type
newtype Minutes = Minutes { unMinutes :: Double }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Fractional, Floating)

type Seconds :: Type
newtype Seconds = Seconds { unSeconds :: Double }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Fractional, Floating)

instance From Hours Minutes where from (Hours x) = Minutes (x * 60)
instance From Minutes Seconds where from (Minutes x) = Seconds (x * 60)

instance From Minutes Hours where from (Minutes x) = Hours (x / 60)
instance From Seconds Minutes where from (Seconds x) = Minutes (x / 60)

instance From Hours Seconds where from = from @Minutes . from
instance From Seconds Hours where from = from @Minutes . from

postfix [ ''Hours, ''Minutes, ''Seconds ]

---

main :: IO ()
main = hspec do
  describe "aliases" do
    it "hours" do
      Hours 1 `shouldBe` Hour  1
      Hour  1 `shouldBe` hours 1
      hours 1 `shouldBe` hour  1

    it "minutes" do
      Minutes 1 `shouldBe` Minute  1
      Minute  1 `shouldBe` minutes 1
      minutes 1 `shouldBe` minute  1

    it "seconds" do
      Seconds 1 `shouldBe` Second  1
      Second  1 `shouldBe` seconds 1
      seconds 1 `shouldBe` second  1

  describe "instances" do
    it "hours" do
      Hour  1 `shouldBe` 1 hour
      Hours 5 `shouldBe` 5 hours

    it "minutes" do
      Minute  1 `shouldBe` 1 minute
      Minutes 5 `shouldBe` 5 minutes

    it "seconds" do
      Second  1 `shouldBe` 1 second
      Seconds 5 `shouldBe` 5 seconds

  describe "conversions" do
    it "hours" do
      1 hour `shouldBe` 60 minutes
      1 hour `shouldBe` 3600 seconds

    it "minutes" do
      1 minute `shouldBe` 60 seconds
      60 minutes `shouldBe` 1 hour

    it "seconds" do
      60 seconds `shouldBe` 1 minute
      3600 seconds `shouldBe` 1 hour
