-- |
-- 'Control.Monad.Managed' utilities for doing 'Control.Concurrent.Async'
-- computations.
module Control.Concurrent.Async.Managed
  ( Async,

    -- ** Captured continuations
    withAsync,
    withAsyncBound,
    withAsyncOn,
    withAsyncWithUnmask,
    withAsyncOnWithUnmask,

    -- ** Regularly lifted functions
    wait,
    poll,
    waitCatch,
    cancel,
    cancelMany,
    uninterruptibleCancel,
    cancelWith,
    waitAny,
    waitAnyCatch,
    waitAnyCancel,
    waitAnyCatchCancel,
    waitEither,
    waitEitherCatch,
    waitEitherCancel,
    waitEitherCatchCancel,
    waitEither_,
    waitBoth,
    link,
    linkOnly,
    link2,
    link2Only,
  ) where

import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Exception (SomeException)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Managed (MonadManaged, managed)
import GHC.Exception.Type (Exception)

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

-- | A lifted version of 'wait'.
wait :: forall m x. MonadIO m => Async x -> m x
wait a = liftIO (Async.wait a)

-- | A lifted version of 'poll'.
poll :: forall m x. MonadIO m => Async x -> m (Maybe (Either SomeException x))
poll as = liftIO (Async.poll as)

-- | A lifted version of 'waitCatch'.
waitCatch :: forall m x. MonadIO m => Async x -> m (Either SomeException x)
waitCatch as = liftIO (Async.waitCatch as)

-- | A lifted version of 'cancel'.
cancel :: forall m x. MonadIO m => Async x -> m ()
cancel as = liftIO (Async.cancel as)

-- | A lifted version of 'cancelMany'.
cancelMany :: forall m x. MonadIO m => [Async x] -> m ()
cancelMany as = liftIO (Async.cancelMany as)

-- | A lifted version of 'uninterruptibleCancel'.
uninterruptibleCancel :: forall m x. MonadIO m => Async x -> m ()
uninterruptibleCancel as = liftIO (Async.uninterruptibleCancel as)

-- | A lifted version of 'cancelWith'.
cancelWith :: forall e m x. (MonadIO m, Exception e) => Async x -> e -> m ()
cancelWith as e = liftIO (Async.cancelWith as e)

-- | A lifted version of 'waitAny'.
waitAny :: forall m x. MonadIO m => [Async x] -> m (Async x, x)
waitAny xs = liftIO (Async.waitAny xs)

-- | A lifted version of 'waitAnyCatch'.
waitAnyCatch :: forall m x. MonadIO m => [Async x] -> m (Async x, Either SomeException x)
waitAnyCatch xs = liftIO (Async.waitAnyCatch xs)

-- | A lifted version of 'waitAnyCancel'.
waitAnyCancel :: forall m x. MonadIO m => [Async x] -> m (Async x, x)
waitAnyCancel xs = liftIO (Async.waitAnyCancel xs)

-- | A lifted version of 'waitAnyCatchCancel'.
waitAnyCatchCancel :: forall a m. MonadIO m => [Async a] -> m (Async a, Either SomeException a)
waitAnyCatchCancel as = liftIO (Async.waitAnyCatchCancel as)

-- | A lifted version of 'waitEither'.
waitEither :: forall a b m. MonadIO m => Async a -> Async b -> m (Either a b)
waitEither as bs = liftIO (Async.waitEither as bs)

-- | A lifted version of 'waitEitherCatchCancel'.
waitEitherCatch :: forall a b m. MonadIO m => Async a -> Async b -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatch as bs = liftIO (Async.waitEitherCatch as bs)

-- | A lifted version of 'waitEitherCatchCancel'.
waitEitherCancel :: forall a b m. MonadIO m => Async a -> Async b -> m (Either a b)
waitEitherCancel as bs = liftIO (Async.waitEitherCancel as bs)

-- | A lifted version of 'waitEitherCatchCancel'.
waitEitherCatchCancel :: forall a b m. MonadIO m => Async a -> Async b -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatchCancel as bs = liftIO (Async.waitEitherCatchCancel as bs)

-- | A lifted version of 'waitEither_'.
waitEither_ :: forall a b m. MonadIO m => Async a -> Async b -> m ()
waitEither_ as bs = liftIO (Async.waitEither_ as bs)

-- | A lifted version of 'waitBoth'.
waitBoth :: forall a b m. MonadIO m => Async a -> Async b -> m (a, b)
waitBoth as bs = liftIO (Async.waitBoth as bs)

-- | A lifted version of 'link'.
link :: forall a m. MonadIO m => Async a -> m ()
link as = liftIO (Async.link as)

-- | A lifted version of 'linkOnly'.
linkOnly :: forall a m. MonadIO m => (SomeException -> Bool) -> Async a -> m ()
linkOnly f as = liftIO (Async.linkOnly f as)

-- | A lifted version of 'link2'.
link2 :: forall a b m. MonadIO m => Async a -> Async b -> m ()
link2 as bs = liftIO (Async.link2 as bs)

-- | A lifted version of 'link2Only'.
link2Only :: forall a b m. MonadIO m => (SomeException -> Bool) -> Async a -> Async b -> m ()
link2Only f as bs = liftIO (Async.link2Only f as bs)
