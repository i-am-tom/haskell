{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}

-- | A representation of event streams.
module Data.Event where

import Control.Concurrent.Async (concurrently)
import Control.Monad (forever, join)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Kind (Type)
import Data.Named (name, value)
import Data.HashSet qualified as HashSet
import Witherable (Filterable (..))

{-# ANN module "HLint: Use const" #-}

-- | An 'Source' is a function that takes an event handler and returns an
-- ""unsubscribe"" action. This type makes no comment on how synchronously or
-- asynchronously this should happen.
type Source :: Type -> Type
newtype Source x = Source { subscribe :: (x -> IO ()) -> IO (IO ()) }
  deriving stock (Functor)

instance Semigroup (Source x) where
  Source this <> Source that = Source \k -> do
    (these, those) <- concurrently (this k) (that k)
    pure (these <> those)

instance Monoid (Source x) where
  mempty = Source \_ -> mempty

instance Filterable Source where
  catMaybes xs = Source \k -> subscribe xs (mapM_ k)
  mapMaybe f xs = Source \k -> subscribe xs (mapM_ k . f)

-- | Create a new event source, along with a function to push events to it.
create :: forall x. IO (Source x, x -> IO ())
create = do
  listeners <- newIORef HashSet.empty

  let push :: x -> IO ()
      push x = readIORef listeners >>= mapM_ \k -> value k x

      event :: Source x
      event = Source \k -> do
        named <- name k

        atomicModifyIORef' listeners \ks ->
          ( HashSet.insert named ks
          , atomicModifyIORef' listeners \ks' ->
              (HashSet.delete named ks', ())
          )

  pure (event, push)

-- | We can always lift an 'IO' action into an 'Source' by calling the handler
-- exactly once.
once :: forall x. IO x -> Source x
once xs = Source \k -> pure () <$ (xs >>= k)

-- | Run an 'IO' action 'forever', calling the 'Source' handler each time it
-- runs. Note that calling this function can block the calling thread forever!
-- Probably best done in a separate thread.
repeatedly :: forall x. IO x -> Source x
repeatedly xs = Source \k -> forever (xs >>= k)

-- | Create a stateful event stream using some given step function.
unfold :: forall s x. (s -> IO (x, s)) -> s -> Source x
unfold f s = Source \k -> let go a = f a >>= \(x, b) -> k x *> go b in go s

-- | Create an event by folding over a different one.
fold :: forall x y. (x -> y -> y) -> y -> Source x -> Source y
fold f initial xs = Source \k -> do
  ref <- newIORef initial

  subscribe xs \x -> join do
    atomicModifyIORef' ref \y ->
      (f x y, k (f x y))
