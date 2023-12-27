{-# LANGUAGE BlockArguments #-}

module Main where

import Data.An
import Data.Hashable (Hashable (hash))
import Test.Hspec (hspec)
import Test.Hspec.QuickCheck (prop)
import Type.Reflection (Typeable)

main :: IO ()
main = hspec do
  let lift :: Either String Int -> An (Ord && Hashable && Show && Typeable)
      lift = either A A

  prop "Eq" \x y -> (lift x == lift y) == (x == y)
  prop "Hashable" \x -> either hash hash x == hash (lift x)
  prop "Ord" \x y -> compare (lift x) (lift y) == compare x y
  prop "Show" \x -> either show show x == show (lift x)
