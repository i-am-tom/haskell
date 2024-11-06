{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitNamespaces #-}

-- |
-- Coordinates in 3D space.
module Measurement.Coordinate.Space where

import Data.Approx (Approx (..))
import Data.Function (on)
import Data.Kind (Constraint, Type)
import GHC.Generics (Generic)
import Measurement.Angle (Radians (..))
import Witch (From (..))
import Data.Via (type (+) (..), Exchange, In (..), Out (..))
import Measurement.Coordinate.Plane qualified as Plane

-- | A type for expressing values of the 3D cartesian coordinate system.
-- https://en.wikipedia.org/wiki/Cartesian_coordinate_system
type Cartesian :: Type
data Cartesian = Cartesian { x :: Double, y :: Double, z :: Double }
  deriving stock (Eq, Generic, Ord, Show)

instance Approx Cartesian where
  this ~= that = distance this that ~= 0

instance Coordinate Cartesian where
  distance :: Cartesian -> Cartesian -> Double
  distance (Cartesian x1 y1 z1) (Cartesian x2 y2 z2)
    = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2 + (z2 - z1) ** 2)

  origin :: Cartesian
  origin = Cartesian 0 0 0

-- | A type for expressing values of the cylindrical coordinate system.
-- https://en.wikipedia.org/wiki/Cylindrical_coordinate_system
type Cylindrical :: Type
data Cylindrical = Cylindrical { r :: Double, θ :: Radians, z :: Double }
  deriving stock (Eq, Generic, Ord, Show)
  deriving (Approx, Coordinate) via (Cartesian + From) Cylindrical

-- | A type for expressing values of the spherical coordinate system.
-- https://en.wikipedia.org/wiki/Spherical_coordinate_system
type Spherical :: Type
data Spherical = Spherical { ρ :: Double, θ :: Radians, φ :: Radians }
  deriving stock (Eq, Generic, Ord, Show)
  deriving (Approx, Coordinate) via (Cartesian + From) Spherical

instance From Cartesian Cylindrical where
  from Cartesian{ x, y, z } = Cylindrical r θ z
    where Plane.Polar{ r, θ } = from (Plane.Cartesian x y)

instance From Cartesian Spherical where
  from = from @Cylindrical . from @Cartesian

instance From Cylindrical Cartesian where
  from Cylindrical{ r, θ, z } = Cartesian x y z
    where Plane.Cartesian{ x, y } = from (Plane.Polar r θ)

instance From Cylindrical Spherical where
  from Cylindrical{ r, θ, z } = Spherical ρ θ φ
    where Plane.Polar ρ φ = from (Plane.Cartesian r z)

instance From Spherical Cartesian where
  from = from @Cylindrical . from @Spherical

instance From Spherical Cylindrical where
  from Spherical{ ρ, θ, φ } = Cylindrical{ r, θ, z }
    where Plane.Cartesian r z = from (Plane.Polar ρ φ)

-- | A class of operations we might want for coordinates in general.
type Coordinate :: Type -> Constraint
class Coordinate x where

  -- | Calculate the distance between two points in terms of distance unit of
  -- the coordinate type.
  distance :: x -> x -> Double

  -- | The origin as expressed in this coordinate system.
  origin :: x

instance (Coordinate x, Exchange c x y, c x y, c y x)
    => Coordinate ((x + c) y) where
  distance = distance @x `on` outbound @c . getWith
  origin = With (inbound @c (origin @x))
