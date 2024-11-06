{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A number of units for measuring angles and conversions between them.
module Measurement.Angle where

import Postfix (postfix)
import Postfix.Instances.Orphans ()
import Data.Approx (Approx (..))
import Data.Fixed (mod')
import Data.Kind (Type)
import Data.Monoid (Sum (Sum))
import Witch (From (..))

-- | An angle expressed as a number of degrees.
type Degrees :: Type
newtype Degrees = Degrees { _dDouble :: Double }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Floating, Fractional, Num, Real, RealFrac)
  deriving (Semigroup, Monoid) via (Sum Double)

instance Approx Degrees where
  Degrees x ~= Degrees y = min diff (360 - diff) ~= 0
    where diff = abs (mod' x 360 - mod' y 360)

instance From Degrees Grades where
  from (Degrees x) = Grades (x / 0.9)

instance From Degrees Radians where
  from (Degrees x) = Radians (pi * x / 180)

instance From Degrees Turns where
  from (Degrees x) = Turns (x / 360)

-- | An angle expressed as a number of grades.
type Grades :: Type
newtype Grades = Grades { _gDouble :: Double }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Floating, Fractional, Num, Real, RealFrac)
  deriving (Semigroup, Monoid) via (Sum Double)

instance Approx Grades where
  Grades x ~= Grades y = min diff (400 - diff) ~= 0
    where diff = abs (mod' x 400 - mod' y 400)

instance From Grades Degrees where
  from (Grades x) = Degrees (x * 0.9)

instance From Grades Radians where
  from (Grades x) = Radians (x * pi / 200)

instance From Grades Turns where
  from (Grades x) = Turns (x / 400)

-- | An angle expressed as a number of radians.
type Radians :: Type
newtype Radians = Radians { _rDouble :: Double }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Floating, Fractional, Num, Real, RealFrac)
  deriving (Semigroup, Monoid) via (Sum Double)

instance Approx Radians where
  Radians x ~= Radians y = min diff (2 * pi - diff) ~= 0
    where diff = abs (mod' x (2 * pi) - mod' y (2 * pi))

instance From Radians Degrees where
  from (Radians x) = Degrees (x * 180 / pi)

instance From Radians Grades where
  from (Radians x) = Grades (x * 200 / pi)

instance From Radians Turns where
  from (Radians x) = Turns (x / (2 * pi))

-- | A Number of full turns of a circle.
type Turns :: Type
newtype Turns = Turns { _tDouble :: Double }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Floating, Fractional, Num, Real, RealFrac)
  deriving (Semigroup, Monoid) via (Sum Double)

instance Approx Turns where
  Turns x ~= Turns y = min diff (1 - diff) ~= 0
    where diff = abs (mod' x 1 - mod' y 1)

instance From Turns Degrees where
  from (Turns x) = Degrees (x * 360)

instance From Turns Grades where
  from (Turns x) = Grades (x * 400)

instance From Turns Radians where
  from (Turns x) = Radians (x * 2 * pi)

postfix [ ''Degrees, ''Grades, ''Radians, ''Turns ]
