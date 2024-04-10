{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module ArraySpec where

import Control.Monad (void)
import Data.Aeson (Value (..))
import Herald.Schema
import Test.Hspec (Spec)
import Utilities (Test (..), test)

spec_array_basic_bad :: Spec
spec_array_basic_bad = test Rejection
  { _input     = Number 123
  , _parser    = "Error in $: parsing array failed, expected Array, but encountered Number"
  , _validator = ["expected JSON value of type OpenApiArray"]
  , _schema    = array $ pure ()
  }

spec_array_basic_good :: Spec
spec_array_basic_good = test Acceptance
  { _input  = Array (fmap Bool [ True, False ])
  , _schema = array $ pure ()
  }

spec_array_items_bad :: Spec
spec_array_items_bad = test Rejection
  { _input     = Array [Number 5, String "hello"]
  , _parser    = "Error in $: parsing boolean failed, expected Boolean, but encountered Number"
  , _schema    = array $ void $ items $ boolean $ pure ()
  , _validator =
      [ "expected JSON value of type OpenApiBoolean"
      , "expected JSON value of type OpenApiBoolean"
      ]
  }

spec_array_items_good :: Spec
spec_array_items_good = test Acceptance
  { _input  = Array (fmap Bool [ True, False ])
  , _schema = array $ void $ items $ boolean $ pure ()
  }

spec_array_max_items_bad :: Spec
spec_array_max_items_bad = test Rejection
  { _input     = Array (fmap Number [ 1, 2, 3, 4, 5, 6 ])
  , _parser    = "Error in $: array must not have more than 5 items"
  , _validator = ["array exceeds maximum size (should be <=5)"]
  , _schema    = array $ maxItems 5
  }

spec_array_max_items_good :: Spec
spec_array_max_items_good = test Acceptance
  { _input  = Array [ String "hello" ]
  , _schema = array $ maxItems 5
  }

spec_array_min_items_bad :: Spec
spec_array_min_items_bad = test Rejection
  { _input     = Array []
  , _parser    = "Error in $: array must have at least 1 items"
  , _validator = ["array is too short (size should be >=1)"]
  , _schema    = array $ minItems 1
  }

spec_array_min_items_good :: Spec
spec_array_min_items_good = test Acceptance
  { _input  = Array (fmap Number [ 1, 2 ])
  , _schema = array $ minItems 1
  }

spec_array_unique_bad :: Spec
spec_array_unique_bad = test Rejection
  { _input     = Array [ Bool True, Number 1, Bool True ]
  , _parser    = "Error in $: all array values must be unique"
  , _validator = ["array is expected to contain unique items, but it does not"]
  , _schema    = array unique
  }

spec_array_unique_good :: Spec
spec_array_unique_good = test Acceptance
  { _input  = Array [ Bool True, Number 3 ]
  , _schema = array unique
  }

spec_array_element_too_short :: Spec
spec_array_element_too_short = test Rejection
  { _input     = Array [ Bool True ]
  , _parser    = "Error in $: element #3 does not exist"
  , _validator = ["array size is invalid (should be exactly 4)"]
  , _schema    = array (element 3 (boolean (pure ())))
  }

spec_array_element_bad :: Spec
spec_array_element_bad = test Rejection
  { _input     = Array [ Bool True, Number 123 ]
  , _parser    = "Error in $: parsing string failed, expected String, but encountered Number"
  , _validator = ["expected JSON value of type OpenApiString"]
  , _schema    = array (element 1 (string (pure ())))
  }

spec_array_element_good :: Spec
spec_array_element_good = test Acceptance
  { _input  = Array [ Bool True, Number 3 ]
  , _schema = array (element 1 (number (pure ())))
  }
