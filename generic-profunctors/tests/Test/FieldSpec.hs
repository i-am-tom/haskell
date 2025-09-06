{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

module Test.FieldSpec where

import Data.Kind (Type)
import Data.Optics.Field (field)
import Data.Profunctor (Forget (..))
import GHC.Generics (Generic)
import Test.Hspec (Spec, describe, it)

type User :: Type
data User = User {name :: String, age :: Int, likesDogs :: Bool}
  deriving (Eq, Generic, Ord, Show)

setName :: String -> User -> User
setName x = field @"name" \_ -> x

getName :: User -> String
getName = runForget (field @"name" (Forget id))

spec_fields :: Spec
spec_fields = describe "Fields" do
  it "can set the name field" do
    setName "Not Tom" (User "Tom" 32 True) == User "Not Tom" 30 True

  it "can get the name field" do
    getName (User "Tom" 30 True) == "Tom"
