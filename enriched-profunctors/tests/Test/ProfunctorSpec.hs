{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

module Test.ProfunctorSpec where


import Control.Arrow (Kleisli (..))
import Control.Category.Constrained
import Data.Functor.Const (Const (..))
import Data.Functor.Contravariant (Op (..))
import Data.Profunctor.Enriched
import Prelude hiding ((.), id)
import Test.QuickCheck (Fun (..))
prop_function_identity :: Fun Int String -> Int -> Bool
prop_function_identity (Fun _ f) x = dimap id id f x == f x

prop_kleisli_identity :: Fun Int (Maybe Int) -> Int -> Bool
prop_kleisli_identity f x = runKleisli (dimap id id f') x == runKleisli f' x
  where Fun _ (Kleisli -> f') = f

prop_op_identity :: Fun Int Int -> Int -> Bool
prop_op_identity f x = getOp (dimap id id f') x == getOp f' x
  where Fun _ (Op -> f') = f

prop_nt_identity :: Fun Int Int -> Int -> Bool
prop_nt_identity (Fun _ f) (Const -> x) = runNT (dimap id id f') x == runNT f' x
  where f' = NT (Const . f . getConst)