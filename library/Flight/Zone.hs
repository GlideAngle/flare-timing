{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Flight.Zone
    ( Lat(..)
    , Lng(..)
    , LatLng(..)
    , Radius(..)
    , Incline(..)
    , Bearing(..)
    , Zone(..)
    , Deadline(..)
    , TimeOfDay(..)
    , Interval(..)
    , StartGates(..)
    , Task(..)
    , center
    , radius
    ) where

import Data.UnitsOfMeasure
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units (Length)
import Flight.Geo (Lat(..), Lng(..), LatLng(..))

-- | The radius component of zones that are cylinder-like, and most are in some
-- way.
newtype Radius = Radius (Length [u| m |]) deriving (Eq, Ord, Show)

-- | The incline component of a conical zone.
newtype Incline = Incline Rational deriving (Eq, Ord, Show)

-- | The bearing component of a vector zone.
newtype Bearing = Bearing Rational deriving (Eq, Ord, Show)

-- | A control zone of the task. Taken together these make up the course to fly
-- with start enter and exit cylinders, turnpoint cylinders, goal lines and
-- cylinders.
data Zone u
    = Point (LatLng u)
    -- ^ Used to mark the exact turnpoints in the optimized task distance.
    | Vector Bearing (LatLng u)
    -- ^ Used only in open distance tasks these mark the start and direction of
    -- the open distance.
    | Cylinder Radius (LatLng u)
    -- ^ The turnpoint cylinder.
    | Conical Incline Radius (LatLng u)
    -- ^ Only used in paragliding, this is the conical end of speed section
    -- used to discourage too low an end to final glides.
    | Line Radius (LatLng u)
    -- ^ A goal line perpendicular to the course line.
    | SemiCircle Radius (LatLng u)
    -- ^ This control zone is only ever used as a goal for paragliding. It is
    -- a goal line perpendicular to the course line followed by half
    -- a cylinder.
    deriving (Eq)

deriving instance (KnownUnit (Unpack u)) => Show (Zone u)

-- | The effective center point of a zone.
center :: Zone u -> LatLng u
center (Point x) = x
center (Vector _ x) = x
center (Cylinder _ x) = x
center (Conical _ _ x) = x
center (Line _ x) = x
center (SemiCircle _ x) = x

-- | The effective radius of a zone.
radius :: Zone u -> Radius
radius (Point _) = Radius zero
radius (Vector _ _) = Radius zero
radius (Cylinder r _) = r
radius (Conical _ r _) = r
radius (Line r _) = r
radius (SemiCircle r _) = r

newtype Deadline = Deadline Integer deriving (Eq, Ord, Show)
newtype TimeOfDay = TimeOfDay Rational deriving (Eq, Ord, Show)
newtype Interval = Interval Rational deriving (Eq, Ord, Show)

data StartGates
    = StartGates
        { open :: TimeOfDay
        , intervals :: [Interval]
        } deriving Show

data Task u
    = Task
        { zones :: [Zone u]
        , startZone :: Int
        , endZone :: Int
        , startGates :: StartGates
        , deadline :: Maybe Deadline
        }

deriving instance (KnownUnit (Unpack u)) => Show (Task u)
