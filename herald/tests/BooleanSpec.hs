{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}

module BooleanSpec where

import Data.Aeson (Value (..))
import Herald.Schema
import Test.Hspec (Spec)
import Utilities (Test (..), test)

spec_boolean_basic_bad :: Spec
spec_boolean_basic_bad =
  test
    Rejection
      { _input = Number 123,
        _parser = "Error in $: parsing boolean failed, expected Boolean, but encountered Number",
        _validator = ["expected JSON value of type OpenApiBoolean"],
        _schema = boolean $ pure ()
      }

spec_boolean_basic_good :: Spec
spec_boolean_basic_good =
  test
    Acceptance
      { _input = Bool True,
        _schema = boolean $ pure ()
      }

spec_boolean_enum_bad :: Spec
spec_boolean_enum_bad =
  test
    Rejection
      { _input = Bool True,
        _parser = "Error in $: value must be one of: False",
        _validator = ["expected one of \"[false]\" but got Bool True"],
        _schema = boolean $ enum [(False, ())]
      }

spec_boolean_enum_good :: Spec
spec_boolean_enum_good =
  test
    Acceptance
      { _input = Bool False,
        _schema = boolean $ enum [(False, ())]
      }
