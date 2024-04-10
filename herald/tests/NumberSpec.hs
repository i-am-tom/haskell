{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module NumberSpec where

import Control.Monad (void)
import Data.Aeson (Value (Number))
import Herald.Schema
import Test.Hspec (Spec)
import Utilities (Test (..), test)

spec_number_basic_bad :: Spec
spec_number_basic_bad = test Rejection
  { _input     = "hello"
  , _parser    = "Error in $: parsing number failed, expected Number, but encountered String"
  , _validator = ["expected JSON value of type OpenApiNumber"]
  , _schema    = number (pure ())
  }

spec_number_basic_good :: Spec
spec_number_basic_good = test Acceptance
  { _input  = Number 123
  , _schema = number (pure ())
  }

spec_number_integral_bad :: Spec
spec_number_integral_bad = test Rejection
  { _input     = Number 3.5
  , _parser    = "Error in $: expected integer value"
  , _validator = ["not an integer"]
  , _schema    = number $ void $ integral @Int
  }

spec_number_integral_good :: Spec
spec_number_integral_good = test Acceptance
  { _input  = Number 3
  , _schema = number $ void $ integral @Int
  }

spec_number_maximum_bad :: Spec
spec_number_maximum_bad = test Rejection
  { _input     = Number 3.5
  , _parser    = "Error in $: value should not exceed 3.0"
  , _validator = ["value 3.5 exceeds maximum (should be <=3.0)"]
  , _schema    = number $ maximum_ 3
  }

spec_number_maximum_good :: Spec
spec_number_maximum_good = test Acceptance
  { _input  = Number 3
  , _schema = number $ maximum_ 3
  }

spec_number_exclusive_maximum_bad :: Spec
spec_number_exclusive_maximum_bad = test Rejection
  { _input     = Number 3.5
  , _parser    = "Error in $: value should be less than 3.5"
  , _validator = ["value 3.5 exceeds maximum (should be <3.5)"]
  , _schema    = number $ exclusiveMaximum *> maximum_ 3.5
  }

spec_number_exclusive_maximum_good :: Spec
spec_number_exclusive_maximum_good = test Acceptance
  { _input  = Number 3
  , _schema = number $ exclusiveMaximum *> maximum_ 3.5
  }

spec_number_multiple_of_bad :: Spec
spec_number_multiple_of_bad = test Rejection
  { _input     = Number 3
  , _parser    = "Error in $: input should be a multiple of 2.0"
  , _validator = ["expected a multiple of 2.0 but got 3.0"]
  , _schema    = number $ multipleOf 2
  }

spec_number_multiple_of_good :: Spec
spec_number_multiple_of_good = test Acceptance
  { _input  = Number 10.5
  , _schema = number $ multipleOf 3.5
  }
