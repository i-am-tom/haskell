# Autobiographer

An interface for describing timelines.

## What?

This library exposes a `Timeline` type that we can build with the `Alternative`
interface. The API is as follows:

```haskell
-- What is the total length of this animation timeline?
duration :: Timeline s x -> Duration

-- What does the animation look like at a given moment in time?
snapshot :: Duration -> s

-- How should the animation change over the given period of time?
over :: forall s x. Monoid s => Duration -> (Progress -> s) -> Timeline s x

-- The animation should do nothing for the given period of time.
wait :: forall s x. Monoid s => Duration -> Timeline s x

-- Sample a timeline according to a given FPS.
sample :: forall s x. FPS -> Timeline s x -> [s]
```
