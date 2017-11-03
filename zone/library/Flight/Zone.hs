{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
    , fromRationalRadius
    , fromRationalZone
    , toRationalRadius
    , toRationalZone
    , realToFracRadius
    , realToFracZone
    ) where

import Data.UnitsOfMeasure (u, zero, toRational', fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Number.RoundingFunctions (dpRound)
import Data.Bifunctor.Flip (Flip(..))

import Flight.Units (showRadian, realToFrac')
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))

-- | The radius component of zones that are cylinder-like, and most are in some
-- way.
newtype Radius a = Radius (Quantity a [u| m |]) deriving (Eq, Ord)

instance Real a => Show (Radius a) where
    show (Radius d) = "r = " ++ show dbl
        where
            d' = toRational' d
            Flip rounded = dpRound 3 <$> Flip d'
            MkQuantity dbl = fromRational' rounded :: Quantity Double [u| m |]

-- | The incline component of a conical zone.
newtype Incline = Incline (Quantity Rational [u| rad |]) deriving (Eq, Ord)

-- | The bearing component of a vector zone.
newtype Bearing = Bearing (Quantity Rational [u| rad |]) deriving (Eq, Ord)

instance Show Incline where
    show (Incline angle) = "i = " ++ showRadian angle

instance Show Bearing where
    show (Bearing b) = "r = " ++ showRadian b 

-- | A control zone of the task. Taken together these make up the course to fly
-- with start enter and exit cylinders, turnpoint cylinders, goal lines and
-- cylinders.
data Zone a
    = Point (LatLng [u| rad |])
    -- ^ Used to mark the exact turnpoints in the optimized task distance.
    | Vector Bearing (LatLng [u| rad |])
    -- ^ Used only in open distance tasks these mark the start and direction of
    -- the open distance.
    | Cylinder (Radius a) (LatLng [u| rad |])
    -- ^ The turnpoint cylinder.
    | Conical Incline (Radius a) (LatLng [u| rad |])
    -- ^ Only used in paragliding, this is the conical end of speed section
    -- used to discourage too low an end to final glides.
    | Line (Radius a) (LatLng [u| rad |])
    -- ^ A goal line perpendicular to the course line.
    | SemiCircle (Radius a) (LatLng [u| rad |])
    -- ^ This control zone is only ever used as a goal for paragliding. It is
    -- a goal line perpendicular to the course line followed by half
    -- a cylinder.
    deriving (Eq, Show)

fromRationalRadius :: Fractional a => Radius Rational -> Radius a
fromRationalRadius (Radius r) = Radius $ fromRational' r

toRationalRadius :: Real a => Radius a -> Radius Rational
toRationalRadius (Radius r) = Radius $ toRational' r

realToFracRadius :: (Real a, Fractional b) => Radius a -> Radius b
realToFracRadius (Radius r) = Radius $ realToFrac' r

fromRationalZone :: Fractional a => Zone Rational -> Zone a
fromRationalZone (Point x) = Point x
fromRationalZone (Vector b x) = Vector b x
fromRationalZone (Cylinder r x) = Cylinder (fromRationalRadius r) x
fromRationalZone (Conical i r x) = Conical i (fromRationalRadius r) x
fromRationalZone (Line r x) = Line (fromRationalRadius r) x
fromRationalZone (SemiCircle r x) = SemiCircle (fromRationalRadius r) x

toRationalZone :: Real a => Zone a -> Zone Rational
toRationalZone (Point x) = Point x
toRationalZone (Vector b x) = Vector b x
toRationalZone (Cylinder r x) = Cylinder (toRationalRadius r) x
toRationalZone (Conical i r x) = Conical i (toRationalRadius r) x
toRationalZone (Line r x) = Line (toRationalRadius r) x
toRationalZone (SemiCircle r x) = SemiCircle (toRationalRadius r) x

realToFracZone :: (Real a, Fractional b) => Zone a -> Zone b
realToFracZone (Point x) = Point x
realToFracZone (Vector b x) = Vector b x
realToFracZone (Cylinder r x) = Cylinder (realToFracRadius r) x
realToFracZone (Conical i r x) = Conical i (realToFracRadius r) x
realToFracZone (Line r x) = Line (realToFracRadius r) x
realToFracZone (SemiCircle r x) = SemiCircle (realToFracRadius r) x

-- | The effective center point of a zone.
center :: Zone a -> LatLng [u| rad |]
center (Point x) = x
center (Vector _ x) = x
center (Cylinder _ x) = x
center (Conical _ _ x) = x
center (Line _ x) = x
center (SemiCircle _ x) = x

-- | The effective radius of a zone.
radius :: Num a => Zone a -> Radius a
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

data Task a
    = Task
        { zones :: [Zone a]
        , startZone :: Int
        , endZone :: Int
        , startGates :: StartGates
        , deadline :: Maybe Deadline
        }
