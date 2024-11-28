{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitNamespaces #-}

-- |
-- Coordinates in 2D space.
module Measurement.Coordinate.Plane where

import Data.Approx (Approx (..))
import Data.Function (on)
import Data.Kind (Constraint, Type)
import Data.Via (Exchange, In (..), Out (..), type (+) (..))
import GHC.Generics (Generic)
import Measurement.Angle (Radians (..))
import Witch (From (..))

-- | A type for expressing values of the 2D cartesian coordinate system.
-- https://en.wikipedia.org/wiki/Cartesian_coordinate_system
type Cartesian :: Type
data Cartesian = Cartesian {x :: Double, y :: Double}
  deriving stock (Eq, Generic, Ord, Show)

instance Approx Cartesian where
  this ~= that = distance this that ~= 0

instance Coordinate Cartesian where
  distance :: Cartesian -> Cartesian -> Double
  distance (Cartesian x1 y1) (Cartesian x2 y2) =
    sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

  origin :: Cartesian
  origin = Cartesian 0 0

-- | A type for expressing values of the polar coordinate system.
-- https://en.wikipedia.org/wiki/Polar_coordinate_system
type Polar :: Type
data Polar = Polar {r :: Double, θ :: Radians}
  deriving stock (Eq, Generic, Ord, Show)
  deriving
    (Approx, Coordinate)
    via (Cartesian + From) Polar

instance From Polar Cartesian where
  from (Polar r (Radians θ)) = Cartesian x y
    where
      (x, y) = (r * cos θ, r * sin θ)

instance From Cartesian Polar where
  from this@(Cartesian x y) = Polar r (Radians θ)
    where
      (r, θ) = (distance this origin, atan2 y x)

-- | A class of operations we might want for coordinates in general.
type Coordinate :: Type -> Constraint
class Coordinate x where
  -- | Calculate the distance between two points in terms of distance unit of
  -- the coordinate type.
  distance :: x -> x -> Double

  -- | The origin as expressed in this coordinate system.
  origin :: x

instance
  (Coordinate x, Exchange c x y, c x y, c y x) =>
  Coordinate ((x + c) y)
  where
  distance = distance @x `on` outbound @c . getWith
  origin = With (inbound @c (origin @x))
