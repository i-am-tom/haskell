{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Applicative ((<|>), empty)
import Control.Monad (guard)
import Data.Event
import Data.Foldable (for_)
import Data.Functor (void)
import Data.IORef (atomicModifyIORef', newIORef)
import Test.Hspec (hspec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Witherable (catMaybes, mapMaybe)

main :: IO ()
main = hspec do
  let shouldYield :: forall x. (Eq x, Show x) => Source x -> [x] -> IO (IO ())
      shouldYield source values = do
        queue <- newIORef values

        subscribe source \x -> do
          next <- atomicModifyIORef' queue \case
            c : cs -> (cs, c)
            [    ] -> error "??"

          x `shouldBe` next

      infix 1 `shouldYield`

  prop "functor" \(xs :: [Int]) (y :: Int) -> do
    (source, push) <- create

    _ <- fmap (+ y) source `shouldYield` fmap (+ y) xs
    mapM_ push xs

  prop "semigroup" \(xs :: [Int]) (ys :: [Int]) -> do
    (xsource, xpush) <- create
    (ysource, ypush) <- create

    _ <- xsource <> ysource `shouldYield` [xs <> ys]
    xpush xs *> ypush ys

  it "mempty" $ void do
    mempty @(Source [Int]) `shouldYield` [[]]

  prop "catMaybes" \(xs :: [Maybe Int]) -> do
    (source, push) <- create

    _ <- catMaybes source `shouldYield` (catMaybes xs)
    mapM_ push xs

  prop "mapMaybe" \(xs :: [Int]) -> do
    (source, push) <- create

    let f :: Int -> Maybe Int
        f x = x <$ guard (mod x 2 == 0)

    _ <- mapMaybe f source `shouldYield` (mapMaybe f xs)
    mapM_ push xs

  prop "<*>" \(x :: Int) (xss :: [Int]) -> do
    (xsource, xpush) <- create
    (ysource, ypush) <- create

    _ <- liftA2 (+) xsource ysource `shouldYield` x : succ x : map succ xss
    xpush 0 *> ypush x *> xpush 1 *> mapM_ ypush xss

  prop "pure" \(x :: Int) -> void do
    pure x `shouldYield` [x]

  prop "<|>" \(xs :: [Either Int Int]) -> do
    (xsource, xpush) <- create
    (ysource, ypush) <- create

    _ <- xsource <|> ysource `shouldYield` map (either id id) xs
    mapM_ (either xpush ypush) xs

  it "empty" $ void do
    empty `shouldYield` [] @Int

  prop "create" \(xs :: [Int]) (ys :: [Int]) -> do
    (source, push) <- create

    cancel <- source `shouldYield`  xs
    _ <- source `shouldYield` (xs <> ys)

    mapM_ push xs *> cancel

    _ <- source `shouldYield` ys
    mapM_ push ys

  prop "fold" \(x :: Int) (xss :: [Int]) -> do
    (source, push) <- create

    _ <- fold (+) 0 source `shouldYield` scanl (+) x xss
    mapM_ push (x : xss)

  prop "sample" \(chunks :: [([Int], Int)]) -> do
    (xsource, xpush) <- create
    (ysource, ypush) <- create

    _ <- sample ysource (fmap const xsource) `shouldYield` map snd chunks
    for_ chunks \(xs, x) -> mapM_ xpush xs *> xpush x *> ypush ()
