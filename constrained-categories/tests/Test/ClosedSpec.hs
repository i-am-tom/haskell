{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Test.ClosedSpec where

import Control.Applicative ((<|>))
import Control.Cartesian.Constrained
import Control.Category.Constrained
import Control.Closed.Constrained
import GHC.Generics ((:*:) (..))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Test.CategorySpec (gen_int)
import Prelude hiding (curry, id, uncurry, (.))

law_evaluation :: (Closed c h k, c x, c y, c z, Show i) => (k (Product k x y) z -> k (Product k x y) z -> PropertyT IO ()) -> (i -> k (Product k x y) z) -> Gen i -> Property
law_evaluation (=~=) prepare gen = property do
  f <- fmap prepare (forAll gen)
  uncurry (curry f) =~= f

law_extensionality :: (Closed c h k, c x, c y, c z, Show i) => (k x (h y z) -> k x (h y z) -> PropertyT IO ()) -> (i -> k x (h y z)) -> Gen i -> Property
law_extensionality (=~=) prepare gen = property do
  f <- fmap prepare (forAll gen)
  curry (uncurry f) =~= f

---

hprop_function_evaluation :: Property
hprop_function_evaluation = law_evaluation verify prepare gen_int
  where
    verify :: ((Int, Int) -> (Int, Int)) -> ((Int, Int) -> (Int, Int)) -> PropertyT IO ()
    verify f g = forAll (liftA2 (,) gen_int gen_int) >>= \x -> f x === g x

    prepare :: Int -> (Int, Int) -> (Int, Int)
    prepare x (y, z) = (x + y, x + z)

hprop_function_extensionality :: Property
hprop_function_extensionality = law_extensionality verify prepare gen_int
  where
    prepare :: Int -> Int -> Int -> Int
    prepare x y z = x + y + z

    verify :: (Int -> Int -> Int) -> (Int -> Int -> Int) -> PropertyT IO ()
    verify f g = do
      x <- forAll gen_int
      y <- forAll gen_int

      f x y === g x y

---

hprop_nt_evaluation :: Property
hprop_nt_evaluation = law_evaluation verify prepare gen_int
  where
    verify :: ((Maybe :*: Maybe) ~> (Maybe :*: Maybe)) -> ((Maybe :*: Maybe) ~> (Maybe :*: Maybe)) -> PropertyT IO ()
    verify (NT f) (NT g) = do
      x <- forAll (Gen.maybe gen_int)
      y <- forAll (Gen.maybe gen_int)

      f (x :*: y) === g (x :*: y)

    prepare :: Int -> (Maybe :*: Maybe) ~> (Maybe :*: Maybe)
    prepare n = NT \(x :*: y) -> if n `mod` 2 == 0 then x :*: y else y :*: x

hprop_nt_extensionality :: Property
hprop_nt_extensionality = law_extensionality verify prepare gen_int
  where
    verify :: (Maybe ~> End Maybe Maybe) -> (Maybe ~> End Maybe Maybe) -> PropertyT IO ()
    verify (NT f) (NT g) = do
      x <- forAll (Gen.maybe gen_int)
      y <- forAll (Gen.maybe gen_int)

      unEnd (f x) id y === unEnd (g x) id y

    prepare :: Int -> (Maybe ~> End Maybe Maybe)
    prepare n = NT \x -> End \f y ->
      if n `mod` 2 == 0
        then fmap f x <|> y
        else y <|> fmap f x
