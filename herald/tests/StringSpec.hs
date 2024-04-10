{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module StringSpec where

import Data.Aeson (Value (Number))
import Herald.Schema
import Test.Hspec (Spec)
import Utilities (Test (..), test)

spec_string_basic_bad :: Spec
spec_string_basic_bad = test Rejection
  { _input     = Number 123
  , _parser    = "Error in $: parsing string failed, expected String, but encountered Number"
  , _validator = ["expected JSON value of type OpenApiString"]
  , _schema    = string (pure ())
  }

spec_string_basic_good :: Spec
spec_string_basic_good = test Acceptance
  { _input  = "hello"
  , _schema = string (pure ())
  }

spec_string_max_length_bad :: Spec
spec_string_max_length_bad = test Rejection
  { _input     = "hello"
  , _parser    = "Error in $: input should at most be 3 characters"
  , _validator = ["string is too long (length should be <=3)"]
  , _schema    = string (maxLength 3)
  }

spec_string_max_length_good :: Spec
spec_string_max_length_good = test Acceptance
  { _input  = "hello"
  , _schema = string (maxLength 5)
  }

spec_string_min_length_bad :: Spec
spec_string_min_length_bad = test Rejection
  { _input     = "hello"
  , _parser    = "Error in $: input should at least be 10 characters"
  , _validator = ["string is too short (length should be >=10)"]
  , _schema    = string (minLength 10)
  }

spec_string_min_length_good :: Spec
spec_string_min_length_good = test Acceptance
  { _input  = "hello"
  , _schema = string (minLength 5)
  }

spec_string_pattern_bad :: Spec
spec_string_pattern_bad = test Rejection
  { _input     = "abcd"
  , _parser    = "Error in $: input should match the regular expression ^a+$"
  , _validator = ["string does not match pattern \"^a+$\""]
  , _schema    = string (pattern "^a+$")
  }

spec_string_pattern_good :: Spec
spec_string_pattern_good = test Acceptance
  { _input  = "abcd"
  , _schema = string (pattern "a..d")
  }
