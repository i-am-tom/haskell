# An open `Variant` type

An extensible type a bit like `Either`, surely to be replaced by yet another
library ["heavily based"](https://github.com/haskell-works/oops?tab=readme-ov-file#credits)
on [my work](https://github.com/i-am-tom/oops/issues/4).

## Show me!

<!--
```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module README where

import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Variant
import Text.Printf (hPrintf)
import System.IO (stdout)

data UserEvent  = UserEvent { username :: String }
data OrderEvent = OrderEvent { order_id :: Int }
data AdminEvent = AdminEvent { is_allowed :: Bool }
```
-->

`Variant '[ x, y, z ]` holds either an `x`, a `y`, or a `z`. Generally, a
`Variant` is like a generalisation of `Either`.

```haskell
-- Possible values are given in the type-level list.
type Event :: Type
type Event = Variant '[ UserEvent, OrderEvent, AdminEvent ]

samples :: [Event]
samples =
  [ embed UserEvent { username = "Tom" }
  , embed OrderEvent { order_id = 10 }
  , embed UserEvent { username = "Not Tom" }
  , embed AdminEvent { is_allowed = True }
  ]
```

If `x ∈ xs`, then we can `embed` values of type `x` into a `Variant xs`. We can
also try to extract an `x` from a `Variant xs`:

```haskell
userEvents :: [UserEvent]
userEvents = mapMaybe project samples
```

Finally, we can pattern match on a `Variant` to handle different cases. Note
that the number in the pattern match here tells us the index in the `Variant`'s
type list.

```haskell
userProjection :: Event -> IO ()
userProjection = \case
  [qq|0| UserEvent{ username = u } |] -> hPrintf stdout "Projecting %s event.." u
  [qq|2| AdminEvent{} |] -> putStrLn "Projecting admin event.."
  _ -> putStrLn "Skipping unrelated event.."
```
