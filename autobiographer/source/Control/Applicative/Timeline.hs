{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- An 'Applicative' for describing a timeline.
module Control.Applicative.Timeline where

import Control.Applicative (Alternative (..))
import Data.Kind (Type)
import Numeric.Natural (Natural)

-- | A time duration, described as a non-negative number.
type Duration :: Type
newtype Duration = Duration {milliseconds :: Natural}
  deriving newtype (Eq, Ord, Enum, Integral, Num, Read, Real, Show)

-- | An applicative for describing timelines. We store the total duration of
-- the animation, as well as a function for sampling any moment in that
-- animation.
type Timeline :: Type -> Type -> Type
data Timeline s x
  = Timeline
  { duration :: Duration,
    snapshot :: Duration -> s
  }
  deriving stock (Functor)

instance (Semigroup s) => Semigroup (Timeline s x) where
  Timeline s f <> Timeline t g = Timeline (max s t) (f <> g)

instance (Monoid s) => Monoid (Timeline s x) where
  mempty = Timeline 0 \_ -> mempty

instance (Monoid s) => Applicative (Timeline s) where
  Timeline s f <*> Timeline t g = Timeline (s + t) \now ->
    if now < s then f now else g (now - s)

  pure _ = Timeline 0 \_ -> mempty

instance (Monoid s) => Alternative (Timeline s) where
  (<|>) = (<>)
  empty = mempty

-- | A number between @0@ and @1@ (inclusively) that describes the progress
-- through an animation, with @0@ meaning the very start and @1@ meaning the
-- very end.
type Progress :: Type
newtype Progress = Progress {toDouble :: Double}
  deriving newtype (Enum, Eq, Fractional, Num, Ord, Read, Real, Show)

-- | Describe an animation that lasts for a given period of time.
over :: forall s x. (Monoid s) => Duration -> (Progress -> s) -> Timeline s x
over duration k = Timeline duration \now -> do
  let progress :: Progress
      progress = fromIntegral now / fromIntegral duration

  if now > duration then mempty else k progress

-- | Do nothing for a given period of time.
wait :: forall s x. (Monoid s) => Duration -> Timeline s x
wait total = Timeline total \_ -> mempty

-- | A number of frames requested per second.
type FPS :: Type
newtype FPS = FPS {framesPerSecond :: Natural}
  deriving newtype (Eq, Ord, Enum, Integral, Num, Real)

-- | Given an 'FPS' and a 'Timeline', sample that many frames per second, and
-- return a list of those frames.
sample :: forall s x. FPS -> Timeline s x -> [s]
sample fps Timeline {duration, snapshot} = do
  frame <- [0 .. duration * fromIntegral fps `div` 1000]

  let timestamp :: Duration
      timestamp = frame * 1000 `div` fromIntegral fps

  pure (snapshot timestamp)
