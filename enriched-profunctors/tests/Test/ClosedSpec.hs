{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Test.ClosedSpec where

import Control.Category.Constrained (Category (..))
import Control.Closed.Constrained (uncurry, curry)
import Data.Profunctor.Closed.Enriched (Closed (..))
import Data.Profunctor.Enriched (dimap)
import Test.QuickCheck (Fun (..))
import Prelude hiding ((.), curry, uncurry)

prop_function_closed :: Fun Int String -> (Fun Int (Fun String Int)) -> Int -> String -> Bool
prop_function_closed (Fun _ f) (Fun _ g) y z = (closed . closed) f g' y z == (dimap uncurry curry . closed) f g' y z
  where g' = \x -> let Fun _ g'' = g x in g''