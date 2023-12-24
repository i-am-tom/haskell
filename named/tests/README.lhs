# `Named` values

Values that have been named. Stably.

## What?

<!--
```haskell
{-# LANGUAGE BlockArguments #-}
module README where

import Data.Named
import Prelude hiding (and, or)
import System.IO.Unsafe (unsafePerformIO)
```
-->

```haskell
and :: Named (Bool -> Bool -> Bool)
and = unsafePerformIO (name \x y -> x && y)

or :: Named (Bool -> Bool -> Bool)
or = unsafePerformIO (name \x y -> x || y)

-- Prints "Yay!"
main :: IO ()
main
  | and == or = putStrLn "Huh?"
  | otherwise = putStrLn "Yay!"
```

## Why?

We can't have `Eq` on functions, but sometimes all we actually need is
something as flimsy as pointer equality. The `StableName` API gives us a nice
solution here.
