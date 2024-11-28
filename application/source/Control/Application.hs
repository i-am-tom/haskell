{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- A structure for creating event-driven UIs.
module Control.Application
  ( Application (..),

    run,
    simulate,

    Command.Command,
    Command.command,
    Command.delay,
    Command.effect,

    Subscription.Subscription,
    Subscription.makeSubscription,
    Subscription.fps
  ) where

import Control.Application.Command (Command)
import Control.Application.Command qualified as Command
import Control.Application.Subscription (Subscription, Subscriptions)
import Control.Application.Subscription qualified as Subscription
import Control.Concurrent.Async.Managed qualified as Async (withAsync)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Monad (foldM)
import Control.Monad.Managed (Managed, liftIO)
import Control.Monad.Managed qualified as Managed
import Data.HashSet (HashSet)
import Data.Kind (Type)
import System.Exit (ExitCode)

-- | 
-- The type of an application. This is very similar to the Elm architecture,
-- most notably except for the fact that 'update' is capable of killing the
-- program. Also, there is a semantic difference, as when we 'run'
-- applications, we run all subscriptions and commands concurrently alongside
-- the main thread, rather than having a single-threaded event loop like the
-- JavaScript runtime.
--
-- Note that there's no specific 'render' function: this can be implemented as
-- a 'Command' according to some FPS 'subscriptions'.
type Application :: Type -> Type -> Type
data Application event state
  = Application
      { -- | An initial state for the application, as well as an optional
        -- action to produce an event.
        initial :: (Command event, state),

        -- | Given a current state and a new event, compute the subsequent
        -- state, possibly triggering a further action in the process. Note
        -- that this function can choose to exit the application.
        update :: state -> event -> Either ExitCode (Command event, state),

        -- | A list of signals to which we'd like to subscribe under the given
        -- state. For example, clock ticks or keyboard button presses.
        subscriptions :: state -> HashSet (Subscription event)
      }

-- | Run an application in IO, exiting only when `update` gives us an exit
-- code rather than a subsequent state. Note that, under this execution model,
-- subscriptions and commands run concurrently alongside the main thread, but
-- make transactional writes to a message queue of events.
run :: forall event state. Application event state -> IO ExitCode
run Application{..} = do
  queue <- newChan

  let step :: Subscriptions event -> (Command event, state) -> Managed ExitCode
      step listeners (source, state) = do
          reviewed <- Subscription.update (writeChan queue) listeners (subscriptions state)
          _        <- Async.withAsync (Command.execute source (writeChan queue))

          liftIO (readChan queue) >>= either pure (step reviewed) . update state

  Managed.with (step mempty initial) pure

-- | For the sake of testing, it's useful to be able to observe the state of an
-- application after a set of events without involving IO. For example,
-- consider an invariant such as "pressing ESC at most 5 times will always get
-- you back to the home screen":
-- 
-- * Generate any arbitrary application state
-- * Generate any arbitrary list of events
-- * Append 5 ESC events to that list
-- * Use `simulate` to determine the eventual state
-- * Check that state corresponds to the homepage
--
-- With `simulate`, we can write tests (both unit and property!) to make
-- assertions about the behaviour of our application.
simulate :: forall event state. Application event state -> [event] -> Either ExitCode state
simulate Application{..} = foldM (\acc -> fmap snd . update acc) (snd initial)
