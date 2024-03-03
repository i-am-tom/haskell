{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

-- | A representation of event streams.
--
-- As with every event stream module I end up writing, it inevitably ends up as
-- a port of Phil Freeman's @purescript-event@ package.
module Data.Event
  ( Source (..),

    create,
    fold,
    sample,
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad (join)
import Data.Foldable (for_)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Monoid (Ap (Ap))
import Witherable (Filterable (..))

{-# ANN module "HLint: Use const" #-}

-- | An 'Source' is a function that takes an event handler and returns an
-- ""unsubscribe"" action. This type makes no comment on how synchronously or
-- asynchronously this should happen.
--
-- prop> subscribe xs j <> subscribe xs k === subscribe xs (j <> k)
-- prop> subscribe xs k <> subscribe ys k === subscribe (xs <> ys) k
type Source :: Type -> Type
newtype Source x = Source { subscribe :: (x -> IO ()) -> IO (IO ()) }
  deriving stock (Functor)
  deriving (Semigroup, Monoid) via (Ap Source x)

instance Filterable Source where
  catMaybes (Source xs) = Source \k -> xs \x -> mapM_ k x
  mapMaybe f (Source xs) = Source \k -> xs \x -> mapM_ k (f x)

instance Applicative Source where
  liftA2 f xs ys = Source \k -> do
    rx <- newIORef Nothing
    ry <- newIORef Nothing

    cx <- subscribe xs \x -> join do
      atomicModifyIORef' rx \_ ->
        ( Just x, readIORef ry >>= mapM_ \y -> k (f x y) )

    cy <- subscribe ys \y -> join do
      atomicModifyIORef' ry \_ ->
        ( Just y, readIORef rx >>= mapM_ \x -> k (f x y) )

    pure (cx <> cy)

  pure x = Source \k -> pure () <$ k x

instance Alternative Source where
  xs <|> ys = Source (subscribe xs <> subscribe ys)
  empty = Source \_ -> mempty

-- | Create a new event source with a function to push events through.
create :: forall x. IO (Source x, x -> IO ())
create = do
  items <- newIORef (0, IntMap.empty)

  let push :: x -> IO ()
      push x = do
        (_, ks) <- readIORef items
        for_ ks \k -> k x

      event :: Source x
      event = Source \k -> do
        index <- atomicModifyIORef' items \(i, ks) ->
          ((i + 1, IntMap.insert i k ks), i)

        pure $ atomicModifyIORef' items \(i, ks) ->
          ((i, IntMap.delete index ks), ())

  pure (event, push)

-- | Create an event by folding over a different one.
fold :: forall x y. (x -> y -> y) -> y -> Source x -> Source y
fold f initial xs = Source \k -> do
  ref <- newIORef initial

  subscribe xs \x -> join do
    atomicModifyIORef' ref \y ->
      let x' = f x y in (x', k x')

-- | Every time a @when@ event occurs, apply the last function from the second
-- source to produce events in a new source.
sample :: forall when x. Source when -> Source (when -> x) -> Source x
sample when what = Source \k -> do
  fs <- newIORef Nothing

  cf <- subscribe what \f -> writeIORef fs (Just f)
  cx <- subscribe when \x -> readIORef fs >>= mapM_ \f -> k (f x)

  pure (cf <> cx)
