{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- Programs that generate events.
module Control.Application.Command
  ( Command (..),

    command,
    effect,
    delay,
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Data.Kind (Type)
import Numeric.Natural (Natural)
import Witherable (Filterable (..))

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}

-- | A 'Command' is a program that generates some number of events. There are
-- two ways in which we can construct commands:
--
-- * We can use 'command' to lift an 'IO' action into a command.
-- * We can use the 'Semigroup' and 'Monoid' instances to combine commands.
--
-- Note, crucially, that the absence of an 'Applicative' or 'Monad' instance
-- means that a 'Command' can not produce an endless stream of events: every
-- 'IO' action runs concurrently, and if further work is required, the
-- generated event should trigger a further command in the event loop.
type Command :: Type -> Type
newtype Command e = Command { execute :: (e -> IO ()) -> IO () }
  deriving stock (Functor)

instance Filterable Command where
  mapMaybe f (Command xs) = Command \k -> xs \x -> mapM_ k (f x)
  catMaybes (Command xs) = Command \k -> xs \x -> mapM_ k x

instance Semigroup (Command e) where
  Command xs <> Command ys = Command \k -> concurrently_ (xs k) (ys k)

instance Monoid (Command e) where
  mempty = Command \_ -> pure ()

-- | Lift an 'IO' action into a 'Command'.
command :: IO e -> Command e
command xs = Command \k -> xs >>= k

-- | Run an arbitrary 'IO' action that cannot return anything.
effect :: IO () -> Command e
effect xs = Command \_ -> xs

-- | Run a particular command after the given delay.
delay :: Natural -> event -> Command event
delay time event = Command \k -> threadDelay (fromIntegral time) *> k event
