{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Coerce (Coercible)
import Data.Kind (Type)
import Data.Monoid (Sum (Sum))
import Data.Via
import GHC.Generics (Generic)
import Witch (From (..))
import Test.Hspec (describe, hspec)
import Test.Hspec.QuickCheck (prop)

type Fahrenheit :: Type
newtype Fahrenheit = Fahrenheit Double
  deriving stock (Generic, Show)
  deriving (Semigroup, Monoid) via (Sum Double + Coercible) Fahrenheit
  deriving (Eq, Ord) via (Celsius + From) Fahrenheit

instance From Fahrenheit Celsius where
  from (Fahrenheit x) = Celsius ((x - 32) / 1.8)

type Celsius :: Type
newtype Celsius = Celsius Double
  deriving stock (Eq, Generic, Ord, Show)
  deriving (Semigroup, Monoid) via (Fahrenheit + Coercible) Celsius

instance From Celsius Fahrenheit where
  from (Celsius x) = Fahrenheit (x * 1.8 + 32)

main :: IO ()
main = hspec do
  describe "Coercible" do
    prop "Semigroup Fahrenheit" \x y ->
      Fahrenheit x <> Fahrenheit y == Fahrenheit (x + y)

    prop "Semigroup Celsius" \x y ->
      Celsius x <> Celsius y == Celsius (x + y)

  describe "From" do
    prop "Eq Fahrenheit" \(Fahrenheit -> x) (Fahrenheit -> y) ->
      (x == y) == (from @_ @Celsius x == from @_ @Celsius y)

    prop "Ord Fahrenheit" \(Fahrenheit -> x) (Fahrenheit -> y) ->
      compare x y == compare (from @_ @Celsius x) (from @_ @Celsius y)
