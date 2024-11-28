{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- A library for creating JSON schemas and parsers at the same time.
module Herald.Schema
  ( -- ** The core type
    Herald,
    schema,
    parser,

    -- ** Defining a type
    array,
    boolean,
    null_,
    number,
    object,
    string,

    -- ** Type-agnostic utilities
    title,
    description,
    deprecated,
    enum,
    example,
    value,

    -- ** Array utilities
    items,
    element,
    maxItems,
    minItems,
    unique,

    -- ** Number utilities
    integral,
    maximum_,
    exclusiveMaximum,
    minimum_,
    exclusiveMinimum,
    multipleOf,

    -- ** Object utilities
    optional,
    optional',
    required,
    additionalProperties,

    -- ** String utilities
    maxLength,
    minLength,
    pattern_,
  )
where

import Control.Applicative (Alternative (..))
import Control.Lens hiding (chosen, element, enum, index)
import Control.Monad (unless)
import Data.Aeson (Array, Key, Object, ToJSON, Value, (.:), (.:?))
import Data.Aeson qualified as JSON
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, prependFailure, typeMismatch)
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Monoid (Ap (Ap))
import Data.OpenApi (OpenApiItems (..), Schema)
import Data.OpenApi qualified as OpenApi
import Data.Scientific (Scientific, isInteger, toBoundedInteger)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Numeric.Natural (Natural)
import Text.Regex.PCRE ((=~))

-- | The 'Herald' type.
type Herald :: Type -> Type -> Type
data Herald i o = Herald (Schema -> Schema) (Schema -> i -> Parser o)
  deriving (Semigroup, Monoid) via Ap (Herald i) o
  deriving stock (Functor)

-- | Extract the 'Schema' from a 'Herald' value.
schema :: forall output. Herald Value output -> Schema
schema (Herald builder _) = builder mempty

-- | Extract the 'Parser' from a 'Herald' value.
parser :: forall output. Herald Value output -> Value -> Parser output
parser (Herald builder parse) = parse (builder mempty)

instance Alternative (Herald input) where
  Herald s p <|> Herald t q = Herald (liftA2 flatten s t) (p <> q)
    where
      flatten :: Schema -> Schema -> Schema
      flatten this that
        | flattenable this, flattenable that = this <> that
        | flattenable this = this & OpenApi.oneOf <>~ Just [OpenApi.Inline that]
        | flattenable that = that & OpenApi.oneOf <>~ Just [OpenApi.Inline this]
      flatten this that = mempty & OpenApi.oneOf ?~ map OpenApi.Inline [this, that]

      flattenable :: Schema -> Bool
      flattenable x =
        isJust (x ^. OpenApi.discriminator)
          && isJust (x ^. OpenApi.oneOf)

  empty = Herald mempty mempty

instance Applicative (Herald input) where
  Herald s p <*> Herald t q = Herald (s . t) \x i -> p x i <*> q x i
  pure x = Herald id \_ _ -> pure x

-- | Declare that the schema be for an array type.
--
-- @
-- arraySchema = 'array' do
--   _ <- 'title' "Names Array"
--   'items' stringSchema
-- @
array :: forall output. Herald Array output -> Herald Value output
array (Herald s p) = Herald (s . type_) (JSON.withArray "array" . p)
  where
    type_ :: Schema -> Schema
    type_ = OpenApi.type_ ?~ OpenApi.OpenApiArray

-- | Declare that the schema be for a boolean type.
--
-- @
-- booleanSchema = 'boolean' do
--   _ <- 'title' "Do you like dogs?"
--   'value'
-- @
boolean :: forall output. Herald Bool output -> Herald Value output
boolean (Herald s p) = Herald (s . type_) (JSON.withBool "boolean" . p)
  where
    type_ :: Schema -> Schema
    type_ = OpenApi.type_ ?~ OpenApi.OpenApiBoolean

-- | Declare a null type.
--
-- @
-- nullSchema = 'null_'
-- @
null_ :: Herald Value ()
null_ = Herald (OpenApi.type_ ?~ OpenApi.OpenApiNull) \_ -> \case
  JSON.Null -> pure ()
  v -> prependFailure "parsing null failed, " (typeMismatch "Null" v)

-- | Declare that the schema be for a number type.
--
-- @
-- numberSchema = 'number' do
--   _ <- 'title' "Age Field"
--   _ <- 'minimum_' 0
--
--   'integral'
-- @
number :: forall output. Herald Scientific output -> Herald Value output
number (Herald s p) = Herald (s . type_) (JSON.withScientific "number" . p)
  where
    type_ :: Schema -> Schema
    type_ = OpenApi.type_ ?~ OpenApi.OpenApiNumber

-- | Declare that the schema be for an object type.
--
-- @
-- objectSchema = 'object' do
--   _ <- 'title' "User Input"
--
--   name      <- 'required' "name" stringSchema
--   age       <- 'required' "age" numberSchema
--   likesDogs <- 'required' "likes_dogs" booleanSchema
--
--   pure (name, age, likesDogs)
-- @
object :: forall output. Herald Object output -> Herald Value output
object (Herald s p) = Herald (s . type_) (JSON.withObject "object" . p)
  where
    type_ :: Schema -> Schema
    type_ = OpenApi.type_ ?~ OpenApi.OpenApiObject

-- | Declare that the schema be for a string type.
--
-- @
-- stringSchema = 'string' do
--   _ <- 'title' "Name Input"
--   _ <- 'description' "A name represented in ASCII characters"
--
--   'pattern_' "^[a-z'- ]+$"
--   'value'
-- @
string :: forall output. Herald Text output -> Herald Value output
string (Herald s p) = Herald (s . type_) (JSON.withText "string" . p)
  where
    type_ :: Schema -> Schema
    type_ = OpenApi.type_ ?~ OpenApi.OpenApiString

-- TODO: add discriminators
-- discriminate
--   :: forall output
--    . Key
--   -> [(Text, Herald Object output)]
--   -> Herald Object output

-- | Set a title for the current schema object.
--
-- @
-- titleSchema = 'string' do
--   _ <- 'title' "This is a string"
--   'value'
-- @
title :: forall input. Text -> Herald input ()
title t = Herald (OpenApi.title ?~ t) \_ _ -> pure ()

-- | Set a description for the current schema object.
--
-- @
-- descriptionSchema = 'number' do
--   _ <- 'title' "Even number"
--   _ <- 'description' "An even number is a multiple of two"
--
--   'multipleOf' 2
--   'value'
-- @
description :: forall input. Text -> Herald input ()
description t = Herald (OpenApi.description ?~ t) \_ _ -> pure ()

-- | Set the current field as deprecated.
--
-- @
-- deprecatedSchema = 'object' do
--   'optional' "myspace_url" $ 'string' do
--     'deprecated'
-- @
deprecated :: forall input. Herald input ()
deprecated = Herald (OpenApi.deprecated ?~ True) \_ _ -> pure ()

-- | Declare that the current value be one of a set of allowed values.
--
-- @
-- enumSchema = 'string' do
--   _ <- 'title' "Direction Input"
--
--   'enum'
--     [ ("north", North)
--     , ("east",   East)
--     , ("south", South)
--     , ("west",   West)
--     ]
-- @
enum :: forall input output. (Ord input, Show input, ToJSON input) => Map input output -> Herald input output
enum values = Herald (OpenApi.enum_ ?~ map JSON.toJSON allowed) \_ key ->
  case Map.lookup key values of
    Just found -> pure found
    Nothing -> fail ("value must be one of: " ++ intercalate ", " (map show allowed))
  where
    allowed :: [input]
    allowed = Map.keys values

-- | Add an example to the rendered schema.
--
-- @
-- exampleSchema = 'number' do
--   _ <- 'title' "Your favourite number"
--   _ <- 'example' 72
-- @
example :: forall input. (ToJSON input) => input -> Herald input ()
example input = Herald (OpenApi.example ?~ JSON.toJSON input) \_ _ -> pure ()

-- | Access the value being parsed.
--
-- @
-- valueSchema = 'number' do
--   _ <- 'title' "Absoluter Input"
--   x <- 'value'
--
--   pure (abs x)
-- @
value :: forall input. Herald input input
value = Herald id \_ -> pure

-- | Declare a schema to be respected by all elements within an array.
--
-- @
-- itemsSchema = 'array' do
--   _ <- 'title' "Names Input"
--   'items' stringSchema
-- @
items :: forall output. Herald Value output -> Herald Array [output]
items (Herald s p) = Herald updated \spec arr -> do
  let optic :: Traversal' OpenApi.Schema OpenApi.Schema
      optic = OpenApi.items . _Just . _Object . OpenApi._Inline

  let parse :: Value -> Parser output
      parse input = case spec ^? optic of
        Just inner -> p inner input
        Nothing -> fail "Internal error?"

  traverse parse (Vector.toList arr)
  where
    updated :: OpenApi.Schema -> OpenApi.Schema
    updated x = mempty & OpenApi.items ?~ OpenApiItemsObject (OpenApi.Inline (s x))

-- | Declare a schema for a specific element of the tuple.
--
-- @
-- elementSchema = 'array' do
--   _ <- 'title' "User tuple"
--
--   name      <- 'element' 0 stringSchema
--   age       <- 'element' 1 numberSchema
--   likesDogs <- 'element' 2 booleanSchema
--
--   pure (name, age, likesDogs)
-- @
element :: forall output. Natural -> Herald Value output -> Herald Array output
element (fromIntegral -> index) (Herald k p) = Herald (OpenApi.items %~ update) \spec input -> do
  let optic :: Traversal' OpenApi.Schema OpenApi.Schema
      optic = OpenApi.items . _Just . _Array . ix index . OpenApi._Inline

  case spec ^? optic of
    Just single -> case input Vector.!? index of
      Just chosen -> p single chosen
      Nothing -> fail ("element #" ++ show index ++ " does not exist")
    Nothing -> fail "Internal error?"
  where
    update :: Maybe OpenApi.OpenApiItems -> Maybe OpenApi.OpenApiItems
    update =
      Just . OpenApi.OpenApiItemsArray . insert . \case
        Just (OpenApi.OpenApiItemsArray xs) ->
          xs ++ replicate (index + 1 - length xs) (OpenApi.Inline mempty)
        _ -> replicate (index + 1) (OpenApi.Inline mempty)

    insert :: [OpenApi.Referenced OpenApi.Schema] -> [OpenApi.Referenced OpenApi.Schema]
    insert = ix index . OpenApi._Inline %~ k

-- | Declare that an array must not have more than a given number of elements.
--
-- @
-- maxItemsSchema = 'array' do
--   _ <- 'title' "Top MySpace friends"
--   _ <- 'items' stringSchema
--   _ <- 'maxItems' 8
--
--   'value'
-- @
maxItems :: Natural -> Herald Array ()
maxItems count = Herald (OpenApi.maxItems ?~ fromIntegral count) \_ input ->
  unless (length input <= fromIntegral count) do
    fail ("array must not have more than " ++ show count ++ " items")

-- | Declare that an array must not have fewer than a given number of elements.
--
-- @
-- minItemsSchema = 'array' do
--   _ <- 'title' "Favourite foods"
--   _ <- 'items' stringSchema
--   _ <- 'minItems' 1
--
--   'value'
-- @
minItems :: Natural -> Herald Array ()
minItems count = Herald (OpenApi.minItems ?~ fromIntegral count) \_ input ->
  unless (length input >= fromIntegral count) do
    fail ("array must have at least " ++ show count ++ " items")

-- | Declare that all array values must be unique.
--
-- @
-- uniqueSchema = 'array' do
--   _ <- 'title' "Lottery numbers"
--   _ <- 'items' numberSchema
--
--   _ <- 'maxItems' 8
--   _ <- 'minItems' 8
--   _ <- 'unique'
--
--   'value'
-- @
unique :: Herald Array ()
unique = Herald (OpenApi.uniqueItems ?~ True) \_ input ->
  unless (length (foldMap Set.singleton input) == length input) do
    fail "all array values must be unique"

-- | Declare that a numeric value be integral.
--
-- @
-- integralSchema = 'number' do
--   _ <- 'title' "Age Input"
--   _ <- 'minimum_' 0
--
--   'integral'
-- @
integral :: forall output. (Bounded output, Integral output) => Herald Scientific output
integral = Herald (OpenApi.type_ ?~ OpenApi.OpenApiInteger) \_ input ->
  case toBoundedInteger input of
    Just checked -> pure checked
    Nothing -> fail "expected integer value"

-- | Declare that a numeric value must not exceed a maximum.
--
-- @
-- maximumSchema = 'number' do
--   _ <- 'title' "Negative number"
--   _ <- 'maximum_' 0
--   _ <- 'exclusiveMaximum'
--
--   'value'
-- @
maximum_ :: Scientific -> Herald Scientific ()
maximum_ limit = Herald (OpenApi.maximum_ ?~ limit) \spec input -> do
  let exclusive :: Bool
      exclusive = anyOf OpenApi.exclusiveMaximum isJust spec

  case compare input limit of
    EQ
      | exclusive -> fail ("value should be less than " ++ show limit)
      | otherwise -> pure ()
    GT -> fail ("value should not exceed " ++ show limit)
    LT -> pure ()

-- | Declare that a 'maximum' be exclusive.
--
-- @
-- exclusiveMaximumSchema = 'number' do
--   _ <- 'title' "Negative number"
--   _ <- 'maximum_' 0
--   _ <- 'exclusiveMaximum'
--
--   'value'
-- @
exclusiveMaximum :: Herald Scientific ()
exclusiveMaximum = Herald (OpenApi.exclusiveMaximum ?~ True) \_ _ -> pure ()

-- | Declare that a numeric value must not fall below a minimum.
--
-- @
-- minimumSchema = 'number' do
--   _ <- 'title' "Positive number"
--   _ <- 'minimum_' 0
--   _ <- 'exclusiveMinimum'
--
--   'value'
-- @
minimum_ :: Scientific -> Herald Scientific ()
minimum_ limit = Herald (OpenApi.minimum_ ?~ limit) \spec input -> do
  let exclusive :: Bool
      exclusive = anyOf OpenApi.exclusiveMinimum isJust spec

  case compare input limit of
    EQ
      | exclusive -> fail ("value should be more than " ++ show limit)
      | otherwise -> pure ()
    LT -> fail ("value should not be less than " ++ show limit)
    GT -> pure ()

-- | Declare that a 'minimum' be exclusive.
--
-- @
-- minimumSchema = 'number' do
--   _ <- 'title' "Positive number"
--   _ <- 'minimum_' 0
--   _ <- 'exclusiveMinimum'
--
--   'value'
-- @
exclusiveMinimum :: Herald Scientific ()
exclusiveMinimum = Herald (OpenApi.exclusiveMinimum ?~ True) \_ _ -> pure ()

-- | Declare that a number be a multiple of the given value.
--
-- @
-- multipleOfSchema = 'number' do
--   _ <- 'title' "Even number"
--   _ <- 'description' "An even number is a multiple of two"
--
--   'multipleOf' 2
--   'value'
-- @
multipleOf :: Scientific -> Herald Scientific ()
multipleOf factor = Herald (OpenApi.multipleOf ?~ factor) \_ input ->
  unless (isInteger (input / factor)) do
    fail ("input should be a multiple of " ++ show factor)

-- | Declare an optional key within a given object.
--
-- @
-- optionalSchema = 'object' do
--   'optional' "myspace_url" $ 'string' do
--   pure ()
-- @
optional :: forall output. Key -> Herald Value output -> Herald Object (Maybe output)
optional key herald = Herald update \_ obj -> obj .:? key >>= traverse (parser herald)
  where
    update :: OpenApi.Schema -> OpenApi.Schema
    update specification =
      specification
        & OpenApi.properties . at (Key.toText key) ?~ OpenApi.Inline (schema herald)
        & OpenApi.nullable ?~ True

-- | Declare an optional key within a given object, as well as a fallback.
--
-- @
-- optional'Schema = 'object' do
--   'optional' "myspace_url" "I can't remember" $ 'string' do
--   pure ()
-- @
optional' :: forall output. (ToJSON output) => Key -> output -> Herald Value output -> Herald Object output
optional' key fallback herald = Herald update \_ obj -> obj .:? key >>= maybe (pure fallback) (parser herald)
  where
    update :: OpenApi.Schema -> OpenApi.Schema
    update specification =
      specification
        & OpenApi.properties . at (Key.toText key) ?~ OpenApi.Inline (schema herald)
        & OpenApi.nullable ?~ True
        & OpenApi.default_ ?~ JSON.toJSON fallback

-- | Declare a required key within a given object.
--
-- @
-- requiredSchema = 'object' do
--   _ <- 'title' "User Input"
--
--   name      <- 'required' "name" stringSchema
--   age       <- 'required' "age" numberSchema
--   likesDogs <- 'required' "likes_dogs" booleanSchema
--
--   pure (name, age, likesDogs)
-- @
required :: forall output. Key -> Herald Value output -> Herald Object output
required key_ herald = Herald update \_ obj -> obj .: key_ >>= parser herald
  where
    key :: Text
    key = Key.toText key_

    update :: OpenApi.Schema -> OpenApi.Schema
    update spec =
      spec
        & OpenApi.properties . at key ?~ OpenApi.Inline (schema herald)
        & OpenApi.required %~ cons key

-- | Declare that all properties /not/ declared as 'required' or 'optional'
-- adhere to a given schema.
--
-- @
-- additionalPropertiesSchema = 'object' do
--   _ <- 'title' "Phone Book"
--   'additionalProperties' stringSchema
-- @
additionalProperties :: forall output. Herald Value output -> Herald Object (KeyMap output)
additionalProperties (Herald k p) = Herald update \spec obj ->
  KeyMap.traverse (p spec) (KeyMap.filterWithKey (const . not . isDefined spec) obj)
  where
    isDefined :: OpenApi.Schema -> Key -> Bool
    isDefined spec i = has (OpenApi.properties . ix (Key.toText i)) spec

    update =
      OpenApi.additionalProperties
        ?~ OpenApi.AdditionalPropertiesSchema (OpenApi.Inline (k mempty))

-- | Declare that a string has a given maximum length.
--
-- @
-- maxLengthSchema = 'string' do
--   _ <- 'title' "Username Input"
--
--   _ <- 'maxLength' 12
--   _ <- 'minLength' 4
--
--   'value'
-- @
maxLength :: Natural -> Herald Text ()
maxLength count = Herald (OpenApi.maxLength ?~ fromIntegral count) \_ input ->
  unless (Text.length input <= fromIntegral count) do
    fail ("input should at most be " ++ show count ++ " characters")

-- | Declare that a string has a given minimum length.
--
-- @
-- maxLengthSchema = 'string' do
--   _ <- 'title' "Username Input"
--
--   _ <- 'maxLength' 12
--   _ <- 'minLength' 4
--
--   'value'
-- @
minLength :: Natural -> Herald Text ()
minLength count = Herald (OpenApi.minLength ?~ fromIntegral count) \_ input ->
  unless (Text.length input >= fromIntegral count) do
    fail ("input should at least be " ++ show count ++ " characters")

-- | Declare that a string match a given regex.
--
-- @
-- patternSchema = 'string' do
--   _ <- 'title' "DNA Input"
--   _ <- 'pattern_' "[AGCT]*"
--
--   'value'
-- @
pattern_ :: OpenApi.Pattern -> Herald Text ()
pattern_ regex = Herald (OpenApi.pattern ?~ regex) \_ input ->
  unless (Text.unpack input =~ Text.unpack regex) do
    fail ("input should match the regular expression " ++ Text.unpack regex)

-- | A prism for making the 'items' definition tidier.
_Array :: Prism' OpenApiItems [OpenApi.Referenced OpenApi.Schema]
_Array = prism OpenApiItemsArray \case
  OpenApiItemsArray obj -> Right obj
  OpenApiItemsObject arr -> Left (OpenApiItemsObject arr)

-- | A prism for making the 'element' definition tidier.
_Object :: Prism' OpenApiItems (OpenApi.Referenced OpenApi.Schema)
_Object = prism OpenApiItemsObject \case
  OpenApiItemsObject obj -> Right obj
  OpenApiItemsArray arr -> Left (OpenApiItemsArray arr)
