# Representing `event` streams

Years ago, Phil Freeman wrote [the only event library I'll ever
love](https://github.com/paf31/purescript-event), and I've been copying bits
out of it ever since. This is all the stuff that I needed in Haskell. You can
safely bet money that I'll eventually end up porting behaviours here too.

---

A `Source` is a producer of events to which we can subscribe. We can combine
sources monoidally, and we can filter the events they output.
