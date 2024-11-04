# Numeric `postfix` constructors

A neat trick that seems to exist everywhere but as a library.

## Postfix constructors

If you thought Haskell didn't support postfix constructors, you're right. For
example, let's consider a numeric type:

```haskell
newtype Seconds = Seconds Double
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Fractional, Floating)
```

We can create values such as `Seconds 5` without issue, but we get an error
should we try to interpret `5 Seconds`:

```
No instance for ‘Num ((Double -> Seconds) -> t0)’ arising from the literal ‘5’
(maybe you haven't applied a function to enough arguments?)
```

... but that's not a "no", is it? That's just telling us we're missing a
constraint. Thus, we could fix this error by adding an instance:

```haskell
instance Num ((Double -> Seconds) -> Seconds) where
  fromInteger x k = k (fromInteger x)
  ...
```

With this, `5 Seconds :: Seconds` compiles happily! However, if we take another
look at that instance head, what stops us writing other instances?

```haskell
instance From Seconds Minutes
    => Num ((Double -> Seconds) -> Minutes) where
  fromInteger x k = from (k (fromInteger x))
  ...
```

Now, when we write `120 Seconds :: Minutes`, we get `2 Minutes`! With that, we
can build up a system of units with these implicit conversions.

## Inference

The one problem here is that, if we just ask GHC what the type of `5 Seconds`
is, we'll get an unnecessarily general answer. The problem is that GHC can't
pick an instance without us specifying the target type. However, we can use
some `INCOHERENT` magic to help us:

```haskell
instance {-# INCOHERENT #-} (x ~ y, Num i, Num y)
    => Num ((i -> x) -> y) where
```

This instance simply says that, if we can't match one of our specified
instances with certainty, we assume the target unit matches the constructor.

## How to use the library

This library generates all this boilerplate for you. Define your types and
the conversions between them (via the `From` typeclass in the `witch` library),
and this library generates everything else. It even generates singular versions
of your constructors to avoid the unsightly `1 seconds`.

```haskell
-- All the template haskell you could possibly want.
postfix [ ''Hours, ''Minutes, ''Seconds ]

examples :: [Hours]
examples = [ 1 hour, 86400 seconds, 150 minutes ]
```
