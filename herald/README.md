# 📣 Herald

A library for defining `openapi3` schemas and `aeson` parsers at the same time.

```haskell
-- | The 'Herald' type.
type Herald :: Type -> Type -> Type

-- | Extract the 'Schema' from a 'Herald' value.
schema :: forall output. Herald Value output -> Schema

-- | Extract the 'Parser' from a 'Herald' value.
parser :: forall output. Herald Value output -> Value -> Parser output
```

## Why do I want this?

* Most JSON parsing isn't that involved. Having a parser `Monad` means that we
  can't generate a static schema from our parser definition, so this library
  uses an `Alternative` instead.

* Schemas are a pain to write and maintain, but if I can generate them from my
  parsers, then it's one less thing to worry about.

* There are many tools for [generating arbitrary data](https://github.com/json-schema-faker/json-schema-faker)
  from JSON and OpenAPI schemata, so this library can help you write property
  tests against your API too.

## What does it look like?

```haskell
type User :: Type
data User
  = User
      { _name      :: Text
      , _age       :: Int
      , _likesDogs :: Bool
      }
  deriving stock (Eq, Ord, Show)

userSchema :: Herald Value User
userSchema = object do
  _ <- title "User"
  _ <- description "A user of our application"

  _name <-
    required "name" $ string do
      _ <- title "Name"
      _ <- description "An ASCII representation of the user's name"
      _ <- pattern "[a-z'- ]+"
      _ <- minLength 1
      v <- value

      pure v

  _age <-
    required "age" $ number do
      _ <- title "Age"
      _ <- description "The age of the user"
      _ <- minimum_ 0
      _ <- maximum_ 150
      i <- integral

      pure i

  _likesDogs <-
    optional' "likes_dogs" True $ boolean do
        _ <- title "Do you like dogs?"
        _ <- description "A value assumed to be True"
        b <- value

        pure b

  pure User{..}
```

_NB: all the `_ <- ...` and seemingly redundant `pure` statements here are to
appease the `ApplicativeDo` extension. We can write our schema without `do`
notation and make it more succinct, albeit less easy to read._

## The API

### Top-level

#### The `Herald` type and its projections

```haskell
-- | The 'Herald' type.
type Herald :: Type -> Type -> Type

-- | Extract the 'Schema' from a 'Herald' value.
schema :: forall output. Herald Value output -> Schema

-- | Extract the 'Parser' from a 'Herald' value.
parser :: forall output. Herald Value output -> Value -> Parser output
```

#### Assigning types to JSON structures

The first step is to declare the type of the underlying structure. We can
assign any of the standard JSON types.

```haskell
-- | Declare that the schema be for an array type.
array :: forall output. Herald Array output -> Herald Value output

-- | Declare that the schema be for a boolean type.
boolean :: forall output. Herald Bool output -> Herald Value output

-- | Declare a null type.
null_ :: Herald Value ()

-- | Declare that the schema be for a number type.
number :: forall output. Herald Scientific output -> Herald Value output

-- | Declare that the schema be for an object type.
object :: forall output. Herald Object output -> Herald Value output

-- | Declare that the schema be for a string type.
string :: forall output. Herald Text output -> Herald Value output
```

#### Metadata and polymorphic operations

A number of functions exist to add information to the schema without affecting
the parser.

```haskell
-- | Set a title for the current schema object.
title :: forall input. Text -> Herald input ()

-- | Set a description for the current schema object.
description :: forall input. Text -> Herald input ()

-- | Set the current field as deprecated.
deprecated :: forall input. Herald input ()

-- | Add an example to the rendered schema.
example :: forall input. ToJSON input => input -> Herald input ()
```

We're also able to access both the value being parsed, _or_ specify an
enumerated list of options that we can map to any type we want. These
operations are not specific to any particular type.

```haskell
-- | Declare that the current value be one of a set of allowed values.
enum :: forall input output. (Ord input, Show input, ToJSON input) => Map input output -> Herald input output

-- | Access the value being parsed.
value :: forall input. Herald input input
```

### Arrays

```haskell
-- | Declare a schema to be respected by all elements within an array.
items :: forall output. Herald JSON.Value output -> Herald JSON.Array [output]

-- | Declare a schema for a specific element of the tuple.
element :: forall output. Natural -> Herald JSON.Value output -> Herald JSON.Array output

-- | Declare that an array must not have more than a given number of elements.
maxItems :: Natural -> Herald JSON.Array ()

-- | Declare that an array must not have fewer than a given number of elements.
minItems :: Natural -> Herald JSON.Array ()

-- | Declare that all array values must be unique.
unique :: Herald JSON.Array ()
```

### Numbers

```haskell
-- | Declare that a numeric value be integral.
integral :: forall output. (Bounded output, Integral output) => Herald Scientific output

-- | Declare that a numeric value must not exceed a maximum.
maximum_ :: Scientific -> Herald Scientific ()

-- | Declare that a 'maximum' be exclusive.
exclusiveMaximum :: Herald Scientific ()

-- | Declare that a numeric value must not fall below a minimum.
minimum_ :: Scientific -> Herald Scientific ()

-- | Declare that a 'minimum' be exclusive.
exclusiveMinimum :: Herald Scientific ()

-- | Declare that a number be a multiple of the given value.
multipleOf :: Scientific -> Herald Scientific ()
```

### Objects

```haskell
-- | Declare an optional key within a given object.
optional :: forall output. Key -> Herald JSON.Value output -> Herald JSON.Object (Maybe output)

-- | Declare an optional key within a given object, as well as a fallback.
optional' :: forall output. ToJSON output => Key -> output -> Herald JSON.Value output -> Herald JSON.Object output

-- | Declare a required key within a given object.
required :: forall output. Key -> Herald JSON.Value output -> Herald JSON.Object output

-- | Declare that all properties /not/ declared as 'required' or 'optional'
-- adhere to a given schema.
additionalProperties :: forall output. Herald JSON.Value output -> Herald JSON.Object (KeyMap output)
```

### Strings

```haskell
-- | Declare that a string has a given maximum length.
maxLength :: Natural -> Herald Text ()

-- | Declare that a string has a given minimum length.
minLength :: Natural -> Herald Text ()

-- | Declare that a string match a given regex.
pattern :: OpenApi.Pattern -> Herald Text ()
```
