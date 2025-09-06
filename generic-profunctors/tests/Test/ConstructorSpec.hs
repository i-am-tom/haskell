{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

module Test.ConstructorSpec where

import Data.Kind (Type)
import Data.Optics.Constructor (constructor)
import Data.Profunctor (Forget (..))
import GHC.Generics (Generic)
import Test.Hspec (Spec, describe, it)

type Choice :: Type
data Choice = This Int | That Bool
  deriving (Eq, Generic, Ord, Show)

setThis :: Int -> Choice -> Choice
setThis x = constructor @"This" \_ -> x

getThis :: Choice -> Maybe Int
getThis = runForget (constructor @"This" (Forget Just))

spec_constructor :: Spec
spec_constructor = describe "Constructor" do
  it "can set the This constructor" do
    setThis 10 (This 5) == This 10

  it "can get the This constructor" do
    getThis (This 5) == Just 5
