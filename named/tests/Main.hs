{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Applicative (liftA2)
import Data.Named
import Test.Hspec (hspec, it, shouldBe, shouldNotBe)

main :: IO ()
main = hspec do
  let and_ :: Named (Bool -> Bool -> Bool)
      and_ = pure (&&)

      or_ :: Named (Bool -> Bool -> Bool)
      or_ = pure (||)

  it "Lifts" do
    let x = fmap (\f -> f True False) and_
        y = fmap (\f -> f True False) and_
        z = fmap (\f -> f True  True) and_

    x `shouldBe` y
    x `shouldNotBe` z

  it "Lifts twice" do
    let x = liftA2 (\f g -> f True False && g True False) and_ or_
        y = liftA2 (\f g -> f True False && g True False) and_ or_
        z = liftA2 (\f g -> f True  True && g True  True) and_ or_

    x `shouldBe` y
    x `shouldNotBe` z
