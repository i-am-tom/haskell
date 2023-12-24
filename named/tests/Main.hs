{-# LANGUAGE BlockArguments #-}

module Main where

import Data.Named
import README qualified
import Test.Hspec (hspec, it, shouldBe, shouldNotBe)

main :: IO ()
main = do
  README.main

  hspec do
    it "Lifts" do
      x <- liftN1 (\f -> f True False) README.and
      y <- liftN1 (\f -> f True False) README.and
      z <- liftN1 (\f -> f True True) README.and

      x `shouldBe` y
      x `shouldNotBe` z

    it "Lifts twice" do
      x <- liftN2 (\f g -> f True False && g True False) README.and README.or
      y <- liftN2 (\f g -> f True False && g True False) README.and README.or
      z <- liftN2 (\f g -> f True True && g True True) README.and README.or

      x `shouldBe` y
      x `shouldNotBe` z
