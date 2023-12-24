{-# LANGUAGE BlockArguments #-}
module Main where

import Test.Hspec (hspec, it, shouldBe)
import Data.Event qualified as Event
import Control.Monad (join)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)

main :: IO ()
main = hspec do
  it "once" do
    let event :: Event.Source Int
        event = Event.once (pure 1)

    _ <- Event.subscribe event (shouldBe 1)
    pure ()

  it "create" do
    (source, push) <- Event.create
    current <- newIORef (1 :: Int)

    _ <- Event.subscribe source \x -> join do
      atomicModifyIORef' current \y ->
        ( succ y, x `shouldBe` y )

    push 1
    push 2
    push 3

  it "merge" do
    current <- newIORef (0 :: Int)

    let x :: Event.Source Int
        x = Event.once (pure 2)

        y :: Event.Source Int
        y = Event.once (pure 3)

    _ <- Event.subscribe (x <> y) \n ->
      atomicModifyIORef' current \s ->
        ( s + n, () )

    total <- readIORef current
    total `shouldBe` 5
