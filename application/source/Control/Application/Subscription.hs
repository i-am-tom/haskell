{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Signals that produce events.
module Control.Application.Subscription
  ( Subscription,
    makeSubscription,
    Subscriptions,
    update,
    fps,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Managed (Async)
import Control.Concurrent.Async.Managed qualified as Async
import Control.Monad (forever)
import Control.Monad.Managed (Managed)
import Data.Align (Semialign (align))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable (..))
import Data.Kind (Type)
import Data.Named (Named)
import Data.Named qualified as Named
import Data.These (These (..))
import Numeric.Natural (Natural)
import System.Mem.StableName (StableName)
import Witherable (wither)

-- | A 'Sub' is some 'IO' action that produces events. When produced, these
-- events are added to a message queue to be processed via the
-- 'Control.Application.update' logic.
--
-- Note that subscriptions have an 'Eq' instance via a 'System.Mem.StableName':
-- this allows us to make subscribing and unsubscribing totally declarative.
-- This may come back to bite us, however.
type Subscription :: Type -> Type
newtype Subscription event = Subscription (Named ((event -> IO ()) -> IO ()))
  deriving stock (Eq, Functor)
  deriving newtype (Hashable)

makeSubscription :: ((event -> IO ()) -> IO ()) -> Subscription event
makeSubscription = Subscription . pure

-- | Convenience synonym for the 'StableName' we use for subscriptions.
type Name :: Type -> Type
type Name event = StableName ((event -> IO ()) -> IO ())

-- | A map of named subscription handlers. We keep these in case a user
-- unsubscribes from an event so we can kill the thread.
type Subscriptions :: Type -> Type
type Subscriptions event = HashMap (Name event) (Async ())

-- | Review a new set of requested subscriptions. We look for changes between
-- the new set and the incumbent set, and create/destroy threads as needed.
update :: forall event. (event -> IO ()) -> Subscriptions event -> HashSet (Subscription event) -> Managed (Subscriptions event)
update k current incoming = wither go (align current prepared)
  where
    prepared :: HashMap (Name event) (Subscription event)
    prepared = HashMap.fromList do
      subscription@(Subscription inner) <- HashSet.toList incoming
      pure (Named.key inner, subscription)

    go :: These (Async ()) (Subscription event) -> Managed (Maybe (Async ()))
    go = \case
      -- If the subscription no longer features in the incoming subscriptions,
      -- then it should be cancelled.
      This async -> Nothing <$ Async.cancel async
      -- If the subscription appears in both the current and incoming sets,
      -- then we can maintain the subscription.
      These async _ -> pure (Just async)
      -- If the subscription appears in the incoming set but not the current
      -- set, then we need to create the new 'Async' listener.
      That (Subscription subscription) -> fmap Just do
        Async.withAsync (Named.value subscription k)

-- | Raise an event a given number of times per second. Note that these events
-- will join the event queue at this rate, so the actual FPS will never exceed
-- this value.
fps :: Natural -> event -> Subscription event
fps count event = makeSubscription \signal -> forever do
  threadDelay (1_000_000 `div` fromIntegral count) *> signal event
