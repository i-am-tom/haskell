{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module ObjectSpec where

import Control.Monad (void)
import Data.Aeson (Value (..))
import Herald.Schema
import Test.Hspec (Spec)
import Utilities (Test (..), test)

spec_object_basic_bad :: Spec
spec_object_basic_bad =
  test
    Rejection
      { _input = Number 123,
        _parser = "Error in $: parsing object failed, expected Object, but encountered Number",
        _validator = ["expected JSON value of type OpenApiObject"],
        _schema = object $ pure ()
      }

-- TODO: divergent behaviour
--
-- spec_object_non_empty :: Spec
-- spec_object_non_empty = test Rejection
--   { _input     = Object [("hello", Number 123)]
--   , _parser    = _ -- Doesn't fail with unmentioned keys.
--   , _validator = ["property \"hello\" is found in JSON value, but it is not mentioned in Swagger schema"]
--   , _schema    = object $ pure ()
--   }

spec_object_basic_good :: Spec
spec_object_basic_good =
  test
    Acceptance
      { _input = Object [],
        _schema = object $ pure ()
      }

spec_object_required_bad :: Spec
spec_object_required_bad =
  test
    Rejection
      { _input = Object [],
        _parser = "Error in $: key \"hello\" not found",
        _validator = ["property \"hello\" is required, but not found in \"{}\""],
        _schema = object $ void $ required "hello" $ pure ()
      }

spec_object_required_good :: Spec
spec_object_required_good =
  test
    Acceptance
      { _input = Object [],
        _schema = object $ pure ()
      }

spec_object_optional_missing :: Spec
spec_object_optional_missing =
  test
    Acceptance
      { _input = Object [],
        _schema = object $ void $ optional "hello" $ pure ()
      }

spec_object_optional_present_bad :: Spec
spec_object_optional_present_bad =
  test
    Rejection
      { _input = Object [("hello", Number 123)],
        _parser = "Error in $: parsing string failed, expected String, but encountered Number",
        _validator = ["expected JSON value of type OpenApiString"],
        _schema = object $ void $ optional "hello" $ string (pure ())
      }

spec_object_optional_present_good :: Spec
spec_object_optional_present_good =
  test
    Acceptance
      { _input = Object [("hello", Number 123)],
        _schema = object $ void $ optional "hello" $ pure ()
      }

spec_object_additional_properties_bad :: Spec
spec_object_additional_properties_bad =
  test
    Rejection
      { _input = Object [("whatever", Number 123)],
        _parser = "Error in $: parsing string failed, expected String, but encountered Number",
        _validator = ["expected JSON value of type OpenApiString"],
        _schema = object $ void $ additionalProperties $ string $ pure ()
      }

spec_object_additional_properties_good :: Spec
spec_object_additional_properties_good =
  test
    Acceptance
      { _input = Object [("whatever", "hello")],
        _schema = object $ void $ additionalProperties $ string $ pure ()
      }

spec_object_additional_properties_with_other :: Spec
spec_object_additional_properties_with_other =
  test
    Acceptance
      { _input = Object [("hello", Number 123)],
        _schema = object do
          _ <- optional "hello" $ pure ()
          _ <- additionalProperties $ string $ pure ()

          pure ()
      }
