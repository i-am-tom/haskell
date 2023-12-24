-- |
-- A quasi-quoter for defining and pattern-matching on variants.
--
-- Imagine we have @'Variant' '[ x, y, z ]@. To lift some value @a :: x@ into
-- this variant, we can use the quasi-quoter: @[qq|0|a|]@, where @0@ designates
-- its index in the 'Variant' list. Similarly, @[qq|1|b|]@ and @[qq|2|c|]@ do
-- what you'd imagine. Note that this quasi-quoter can be used both for
-- expressions /and/ patterns.
--
-- The need for the index is unfortunate, but there are two reasons why we
-- can't avoid it:
--
-- * From within a quasi-quoter, we can't infer the type of the expression it
--   needs to output from context. In other words, imagine we have the
--   expression @[qq|True|] :: Variant '[Bool, Int]@. There's no way for @qq@
--   to know that it is being used to build a variant of two types whose first
--   is @Bool@. Typed Template Haskell could potentially help here, but it
--   doesn't support patterns.
--
-- * Even if we could, we'd have all the usual problems with type inference and
--   polymorphic values. @[qq|1|] :: Variant '[Int, Float, Natural, Double]@
--   should work, but what type do we assume for @1@?
module Data.Variant.QQ (qq) where

import Data.Attoparsec.Text (Parser, anyChar, char, digit, endOfInput, many1, parseOnly)
import Data.String (fromString)
import Language.Haskell.Meta (parseExp, parsePat)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp (AppE, ConE), Pat (ConP), Q, mkName)

-- | A quasi-quoter for generating the 'Variant' wrapper for a given value,
-- supporting both patterns and expressions. See the module summary for
-- "Data.Variant.QQ" for more details.
--
-- >>> [qq|0|"Hello"|]
-- Here "Hello"
--
-- >>> [qq|1|"Hello"|]
-- There (Here "Hello")
--
-- >>> [qq|2|"Hello"|]
-- There (There (Here "Hello"))
qq :: QuasiQuoter
qq = do
  let parse :: Parser x -> String -> Q x
      parse p = either fail pure . parseOnly (p <* endOfInput) . fromString

  QuasiQuoter
    { quoteDec = \_ -> error "No quoteDec defined for evp"
    , quoteType = \_ -> error "No quoteDec defined for exp"

    , quoteExp = \input -> do
        (count, expression) <- parse (parser parseExp) input

        let here :: Exp
            here = AppE (ConE (mkName "Here")) expression

            there :: Exp -> Exp
            there = AppE (ConE (mkName "There"))

        pure (iterate there here !! count)

    , quotePat = \input -> do
        (count, pattern) <- parse (parser parsePat) input

        let here :: Pat
            here = ConP (mkName "Here") [] [pattern]

            there :: Pat -> Pat
            there more = ConP (mkName "There") [] [more]

        pure (iterate there here !! count)
    }

-- | A parser to extract the index of the quasiquoter. We expect the format to
-- be a natural number, followed by a pipe, followed by a Haskell pattern or
-- expression. We forbid whitespace until after the pipe. Examples:
parser :: (String -> Either String x) -> Parser (Int, x)
parser constructor = do
  index <- fmap read (many1 digit)
  _____ <- char '|'
  other <- many1 anyChar

  case constructor other of
    Right rest -> pure (index, rest)
    Left message -> fail message

