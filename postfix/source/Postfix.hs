-- |
-- An interface for postfix constructors.
module Postfix where

import Postfix.Aliases qualified as Alias
import Postfix.Instances qualified as Instances
import Language.Haskell.TH qualified as TH

-- | Generate a full suite of postfix boilerplate for the given types.
--
-- Consider the following types:
--
-- >>> newtype Hours   = Hours   Double
-- >>> newtype Minutes = Minutes Double
-- >>> newtype Seconds = Seconds Double
--
-- Assuming 'Witch.From' instances between all these types, we can make the
-- following call:
--
--     'postfix' [''Hours, ''Minutes, ''Seconds]
--
-- This will run all the other functions in this module.
postfix :: [TH.Name] -> TH.DecsQ
postfix names
  = mconcat
      [ makeLowercaseAliases names
      , makeLowercaseSingularPatterns names
      , makePostfixFloatings names
      , makePostfixFractionals names
      , makePostfixNums names
      , makeSingularPatterns names
      ]

-- | Generate postfix 'Num' instances for a set of names.
makePostfixNums :: [TH.Name] -> TH.DecsQ
makePostfixNums names = mconcat (fmap (uncurry Instances.num) pairs)
  where pairs = [(x, y) | x <- names, y <- names, x /= y]

-- | Generate postfix 'Fractional' instances for a set of names.
makePostfixFractionals :: [TH.Name] -> TH.DecsQ
makePostfixFractionals names = foldMap (uncurry Instances.fractional) pairs
  where pairs = [(x, y) | x <- names, y <- names, x /= y]

-- | Generate postfix 'Floating' instances for a set of names.
makePostfixFloatings :: [TH.Name] -> TH.DecsQ
makePostfixFloatings names = foldMap (uncurry Instances.floating) pairs
  where pairs = [(x, y) | x <- names, y <- names, x /= y]

-- | Make a lowercase pattern for each of the given types.
--
--     data Minutes = Minutes Double
--     makeLowercaseAliases [''Minutes]
--
--     -- minutes :: Double -> Minutes
--     -- minutes = Minutes
-- 
-- This is nice when you want to write a constant like @30 minutes@.
makeLowercaseAliases :: [TH.Name] -> TH.DecsQ
makeLowercaseAliases = foldMap Alias.lowercase

-- | Make a singular pattern for each of the given types.
--
--     data Minutes = Minutes Double
--     makeSingularPatterns [''Minutes]
--
--     -- pattern Minute :: Double -> Minutes
--     -- pattern Minute x = Minutes x
makeSingularPatterns :: [TH.Name] -> TH.DecsQ
makeSingularPatterns = foldMap Alias.singular

-- | Make a lowercase, singular pattern for each of the given types.
--
--     data Minutes = Minutes Double
--     makeLowercaseSingularPatterns [''Minutes]
--
--     -- minute :: Double -> Minutes
--     -- minute = Minutes
-- 
-- This is nice when you want to write a constant like @1 minute@.
makeLowercaseSingularPatterns :: [TH.Name] -> TH.DecsQ
makeLowercaseSingularPatterns = foldMap Alias.lowercaseSingular
