# Representing `event` streams

Years ago, Phil Freeman wrote [the only event library I'll ever
love](https://github.com/paf31/purescript-event), and I've been copying bits
out of it ever since. This is all the stuff that I needed in Haskell. You can
safely bet money that I'll eventually end up porting behaviours here too.

---

A `Source` is a producer of events to which we can subscribe. We can combine
sources monoidally, and we can filter the events they output.

## Show me!

We can define an event independently of any others using `once`, `repeatedly`,
`unfold`, or most generally, `create`.

```haskell
inputs :: Event Char
inputs = repeatedly getChar
```

We can also define events in terms of other events, using `Semigroup`,
`Functor`, or `Filterable`.

```haskell
controls :: Event Direction
controls = mapMaybe go inputs
  where
    go :: Char -> Maybe Direction
    go = \case
      'W' -> Just Up
      'A' -> Just Left
      'S' -> Just Down
      'D' -> Just Right
      ___ -> Nothing
```

Finally, we can incorporate some notion of state using `fold`:

```haskell
position :: Event V2
position = fold step (V2 0 0) controls
  where
    step :: Direction -> V2 -> V2
    step = \case
      Up    -> y +~ 0.1
      Down  -> y -~ 0.1
      Left  -> x -~ 0.1
      Right -> x +~ 0.1
```
