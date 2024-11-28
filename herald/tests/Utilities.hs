{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Utilities where

import Data.Aeson (Value)
import Data.Aeson.Types (parseEither)
import Data.Function (on)
import Data.Kind (Type)
import Data.OpenApi (Pattern, validateJSONWithPatternChecker)
import Data.Text (Text)
import Data.Text qualified as Text
import Herald.Schema
import Test.Hspec (Spec, it, shouldBe)
import Text.Regex.PCRE ((=~))

type Test :: Type
data Test
  = Acceptance
      { _input :: Value,
        _schema :: Herald Value ()
      }
  | Rejection
      { _input :: Value,
        _schema :: Herald Value (),
        _parser :: String,
        _validator :: [String]
      }

test :: Test -> Spec
test = \case
  Acceptance {_input, _schema} -> do
    it "parser" do
      parseEither (parser _schema) _input
        `shouldBe` Right ()

    it "validator" do
      validateJSONWithPatternChecker checker mempty (schema _schema) _input
        `shouldBe` []
  Rejection {_input, _schema, _parser, _validator} -> do
    it "parser" do
      parseEither (parser _schema) _input
        `shouldBe` Left _parser

    it "validator" do
      validateJSONWithPatternChecker checker mempty (schema _schema) _input
        `shouldBe` _validator
  where
    checker :: Pattern -> Text -> Bool
    checker = flip (=~) `on` Text.unpack
