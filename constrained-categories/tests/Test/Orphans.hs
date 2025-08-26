{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Orphans where

import GHC.Generics ((:*:) (..), (:+:) (..))
import Test.QuickCheck (Arbitrary (..), oneof)

instance (Arbitrary (f x), Arbitrary (g x)) => Arbitrary ((f :+: g) x) where
  arbitrary = oneof [fmap L1 arbitrary, fmap R1 arbitrary]

instance (Arbitrary (f x), Arbitrary (g x)) => Arbitrary ((f :*: g) x) where
  arbitrary = (:*:) <$> arbitrary <*> arbitrary
