{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}

module Measurement.Helpers where

import Control.Monad (unless)
import Data.Approx (Approx ((~=)))
import Hedgehog (Gen, Property, annotateShow, diff, forAll, property)
import Test.Hspec (Expectation, HasCallStack, expectationFailure)
import Data.Kind (Constraint, Type)
import Witch (From (from))

{- HLINT ignore "Use camelCase" -}

type (><) :: Type -> Type -> Constraint
type x >< y = (From x y, From y x)

-- | We can covert @x@ to @y@ and back to produce the original @x@ value,
-- ignoring some amount of precision error.
law_roundtrip :: forall y x. (Approx x, x >< y, Show x, Show y) => Gen x -> Property
law_roundtrip gen = property do
  x <- forAll gen

  let y :: y
      y = from x

  annotateShow y
  diff x (~=) (from y)


-- | The first argument should approximately equal the second according to the
-- @Approx x@ instance.
shouldBeApprox :: (HasCallStack, Show x, Approx x) => x -> x -> Expectation
shouldBeApprox x y = unless (x ~= y) $ expectationFailure do
  show x ++ "is not approximately" ++ show y
