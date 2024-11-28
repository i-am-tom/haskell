{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Kind (Type)
import Data.Variant
import README ()
import Test.Hspec (Expectation, hspec, it, shouldBe)

type T :: [Type]
type T = '[Int, String, Bool]

main :: IO ()
main = hspec do
  let (===) :: Variant T -> Variant T -> Expectation
      (===) = shouldBe

  it "quasiquotes as an expression" do
    [qq|0| 123     |] === embed (123 :: Int)
    [qq|1| "hello" |] === embed "hello"
    [qq|2| True    |] === embed True

  it "quasiquotes as a pattern" do
    let test :: Variant T -> String
        test = \case
          [qq|0| x |] -> show x
          [qq|1| x |] -> x
          [qq|2| x |] -> if x then "yay" else "boo"
          ___________ -> "???"

    test (embed (123 :: Int)) `shouldBe` "123"
    test (embed "hello") `shouldBe` "hello"
    test (embed True) `shouldBe` "yay"
