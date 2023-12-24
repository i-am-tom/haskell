-- |
-- 'Control.Monad.Managed' utilities for doing 'Control.Concurrent.Async'
-- computations.
module Control.Concurrent.Async.Managed
  ( Async,

    withAsync,
    withAsyncBound,
    withAsyncOn,
    withAsyncWithUnmask,
    withAsyncOnWithUnmask
  ) where

import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Monad.Managed (MonadManaged, managed)

-- | A 'Control.Monad.Managed.Managed' version of 'Async.withAsync'.
withAsync :: forall m x. MonadManaged m => IO x -> m (Async x)
withAsync xs = managed (Async.withAsync xs)

-- | A 'Control.Monad.Managed.Managed' version of 'Async.withAsyncBound'.
withAsyncBound :: forall m x. MonadManaged m => IO x -> m (Async x)
withAsyncBound xs = managed (Async.withAsyncBound xs)

-- | A 'Control.Monad.Managed.Managed' version of 'Async.withAsyncOn'.
withAsyncOn :: forall m x. MonadManaged m => Int -> IO x -> m (Async x)
withAsyncOn capability xs = managed (Async.withAsyncOn capability xs)

-- | A 'Control.Monad.Managed.Managed' version of 'Async.withAsyncWithUnmask'.
withAsyncWithUnmask :: forall m x. MonadManaged m => ((forall e. IO e -> IO e) -> IO x) -> m (Async x)
withAsyncWithUnmask k = managed (Async.withAsyncWithUnmask k)

-- | A 'Control.Monad.Managed.Managed' version of 'Async.withAsyncOnWithUnmask'.
withAsyncOnWithUnmask :: forall m x. MonadManaged m => Int -> ((forall e. IO e -> IO e) -> IO x) -> m (Async x)
withAsyncOnWithUnmask capability unmask = managed (Async.withAsyncOnWithUnmask capability unmask)
