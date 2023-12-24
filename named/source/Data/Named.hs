{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

-- |
-- Values that have been named. Stably.
module Data.Named
  ( Named (key, value),
    name,

    liftN1,
    liftN2
  ) where

import Data.Function (on)
import Data.Hashable (Hashable (..))
import Data.Kind (Type)
import System.Mem.StableName (StableName, makeStableName)

-- | A value paired with its 'StableName'. Regardless of the value's type, this
-- permits us an 'Eq' and a 'Hashable' instance, which is very handy for things
-- like functions.
type Named :: Type -> Type
data Named x = Named { key :: StableName x, value :: x }

-- | Transform a 'Named' value, and name the result.
liftN1 :: (x -> y) -> Named x -> IO (Named y)
liftN1 f Named{ value = x } = name (f x)

-- | Merge two 'Named' values together and name the result.
liftN2 :: (x -> y -> z) -> Named x -> Named y -> IO (Named z)
liftN2 f Named{ value = x } Named{ value = y } = name (f x y)

-- | Make a value 'Named'.
name :: forall x. x -> IO (Named x)
name !value = do
  key <- makeStableName value
  pure Named{..}

instance Eq (Named x) where
  (==) = (==) `on` key

instance Hashable (Named x) where
  hash = hash . key
  hashWithSalt d = hashWithSalt d . key

instance Show x => Show (Named x) where
  show = show . value
