{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Control.Application (Application (..), Command, effect, makeSubscription, run)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Kind (Type)
import System.Exit (ExitCode (..))
import Test.Hspec (hspec, it, shouldBe)

type Event :: Type
data Event = Event Int | Tick
  deriving (Eq, Ord, Show)

main :: IO ()
main = do
  first <- newEmptyMVar
  second <- newEmptyMVar
  sink <- newIORef []

  let leak :: Event -> Command event
      leak x = effect $ modifyIORef sink \xs -> xs ++ [x]

  let example :: Application Event [Event]
      example =
        Application
          { initial = mempty,
            update = \xs x ->
              if length xs == 10
                then
                  Left ExitSuccess
                else
                  Right (leak x, x : xs),
            subscriptions = \state ->
              let mvar :: MVar Event
                  mvar = if even (length state) then first else second
               in [ makeSubscription \k -> forever (takeMVar mvar >>= k)
                  ]
          }

  withAsync (run example) \_ -> hspec do
    it "fires commands correctly" do
      putMVar first (Event 1)
      threadDelay 100 -- Concurrency
      events <- readIORef sink
      events `shouldBe` [Event 1]

    it "updates subscriptions correctly" do
      putMVar first (Event 3)
      threadDelay 100

      before <- readIORef sink
      before `shouldBe` [Event 1]

      putMVar second (Event 2)
      threadDelay 100

      after <- readIORef sink
      after `shouldBe` [Event 1, Event 2, Event 3]
