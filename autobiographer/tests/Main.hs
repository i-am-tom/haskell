{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Applicative (Alternative ((<|>)))
import Control.Applicative.Timeline
import Data.Foldable (asum)
import Data.Kind (Type)
import Data.Set (Set)
import Data.Set qualified as Set
import Hedgehog ((===), Gen, Property, PropertyT, checkParallel, discover, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

{- HLINT ignore "Functor law" -}
{- HLINT ignore "Monoid law, right identity" -}
{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Use camelCase" -}

type Timeliner :: Type
data Timeliner
  = Alt [ Timeliner ]
  | Ap Timeliner Timeliner
  | Pure
  | Over Duration Double Double
  | Wait Duration
  deriving (Eq, Ord, Show)

gen_timeliner :: Gen Timeliner
gen_timeliner = Gen.recursive Gen.choice
  [ pure Pure
  , fmap (Wait . Duration) (Gen.integral (Range.linear 1 10000))
  , Over
      <$> fmap Duration (Gen.integral (Range.linear 1 10000))
      <*> Gen.double (Range.linearFrac (-10000) 10000)
      <*> Gen.double (Range.linearFrac (-10000) 10000)
  ]
  [ fmap Alt (Gen.list (Range.linear 0 10) gen_timeliner)
  , Gen.subterm2 gen_timeliner gen_timeliner Ap
  ]

interpret :: Timeliner -> Timeline (Set Double) x
interpret = \case
  Alt xs     -> asum (map interpret xs)
  Ap f x     -> interpret f <*> interpret x
  Pure       -> mempty
  Over t m c -> over t \(Progress x) -> Set.singleton (m * x + c)
  Wait     t -> wait t

(=~=) :: (Monad m, Eq s, Show s) => Timeline s x -> Timeline s x -> PropertyT m ()
(=~=) x y = do
  let bounds :: Duration
      bounds = max (duration x) (duration y)

  duration <- forAll (Gen.integral (Range.linear 0 bounds))
  snapshot x duration === snapshot y duration

infixr 1 =~=

prop_associativity :: Property
prop_associativity = property do
  x <- fmap interpret (forAll gen_timeliner)
  y <- fmap interpret (forAll gen_timeliner)
  z <- fmap interpret (forAll gen_timeliner)
  x <> (y <> z) =~= (x <> y) <> z

prop_left_identity :: Property
prop_left_identity = property do
  x <- fmap interpret (forAll gen_timeliner)
  mempty <> x =~= x

prop_right_identity :: Property
prop_right_identity = property do
  x <- fmap interpret (forAll gen_timeliner)
  x <> mempty =~= x

prop_functor_identity :: Property
prop_functor_identity = property do
  x <- fmap interpret (forAll gen_timeliner)
  fmap id x =~= x

prop_applicative_identity :: Property
prop_applicative_identity = property do
  x <- fmap interpret (forAll gen_timeliner)
  pure id <*> x =~= x

prop_applicative_composition :: Property
prop_applicative_composition = property do
  x <- fmap interpret (forAll gen_timeliner)
  y <- fmap interpret (forAll gen_timeliner)
  z <- fmap interpret (forAll gen_timeliner)

  pure (.) <*> x <*> y <*> z =~= x <*> (y <*> z)

prop_applicative_interchange :: Property
prop_applicative_interchange = property do
  x <- fmap interpret (forAll gen_timeliner)
  x <*> pure "hello" =~= pure ($ "hello") <*> x

prop_alternative_monoid :: Property
prop_alternative_monoid = property do
  x <- fmap interpret (forAll gen_timeliner)
  y <- fmap interpret (forAll gen_timeliner)

  x <> y =~= x <|> y

prop_alternative :: Property
prop_alternative = property do
  x <- fmap interpret (forAll gen_timeliner)
  y <- fmap interpret (forAll gen_timeliner)

  let bounds :: Duration
      bounds = max (duration x) (duration y)

  duration <- forAll (Gen.integral (Range.linear 0 bounds))
  snapshot x duration <> snapshot y duration === snapshot (x <|> y) duration

prop_alternative_associativity :: Property
prop_alternative_associativity = property do
  x <- fmap interpret (forAll gen_timeliner)
  y <- fmap interpret (forAll gen_timeliner)
  z <- fmap interpret (forAll gen_timeliner)
  x <|> (y <|> z) =~= (x <|> y) <|> z

prop_alternative_left_identity :: Property
prop_alternative_left_identity = property do
  x <- fmap interpret (forAll gen_timeliner)
  mempty <|> x =~= x

prop_alternative_right_identity :: Property
prop_alternative_right_identity = property do
  x <- fmap interpret (forAll gen_timeliner)
  x <|> mempty =~= x

prop_initial :: Property
prop_initial = property do
  x <- fmap interpret (forAll gen_timeliner)
  y <- fmap interpret (forAll gen_timeliner)
  n <- fmap Duration $ forAll do
    Gen.integral (Range.linear 1 10000)

  snapshot (x *> over n (Set.singleton . toDouble) *> y) (duration x)
    === Set.singleton 0

prop_final :: Property
prop_final = property do
  x <- fmap interpret (forAll gen_timeliner)
  y <- fmap interpret (forAll gen_timeliner)
  n <- fmap Duration $ forAll do
    Gen.integral (Range.linear 1 10000)

  snapshot (x *> over n (Set.singleton . toDouble) *> y) (duration x + n)
    === snapshot y 0

main :: IO Bool
main = checkParallel $$discover
