{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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

import Data.UnitsOfMeasure (u, convert, zero, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Number.RoundingFunctions (dpRound)
import Data.Bifunctor.Flip (Flip(..))

import Flight.Units (Length)
import Flight.Geo (Lat(..), Lng(..), LatLng(..))

-- | The radius component of zones that are cylinder-like, and most are in some
-- way.
newtype Radius = Radius (Length [u| m |]) deriving (Eq, Ord)

instance Show Radius where
    show (Radius d) = "r = " ++ show dbl
        where
            Flip rounded = dpRound 3 <$> Flip d
            MkQuantity dbl = fromRational' rounded :: Quantity Double [u| m |]

-- | The incline component of a conical zone.
newtype Incline = Incline (Quantity Rational [u| rad |]) deriving (Eq, Ord)

-- | The bearing component of a vector zone.
newtype Bearing = Bearing (Quantity Rational [u| rad |]) deriving (Eq, Ord)

showRadian :: Quantity Rational [u| rad |] -> String
showRadian b = show dbl
    where
        deg = convert b :: Quantity Rational [u| deg |]
        Flip rounded = dpRound 3 <$> Flip deg
        MkQuantity dbl = fromRational' rounded :: Quantity Double [u| deg |]

instance Show Incline where
    show (Incline angle) = "i = " ++ showRadian angle

instance Show Bearing where
    show (Bearing b) = "r = " ++ showRadian b 

-- | A control zone of the task. Taken together these make up the course to fly
-- with start enter and exit cylinders, turnpoint cylinders, goal lines and
-- cylinders.
data Zone
    = Point (LatLng [u| rad |])
    -- ^ Used to mark the exact turnpoints in the optimized task distance.
    | Vector Bearing (LatLng [u| rad |])
    -- ^ Used only in open distance tasks these mark the start and direction of
    -- the open distance.
    | Cylinder Radius (LatLng [u| rad |])
    -- ^ The turnpoint cylinder.
    | Conical Incline Radius (LatLng [u| rad |])
    -- ^ Only used in paragliding, this is the conical end of speed section
    -- used to discourage too low an end to final glides.
    | Line Radius (LatLng [u| rad |])
    -- ^ A goal line perpendicular to the course line.
    | SemiCircle Radius (LatLng [u| rad |])
    -- ^ This control zone is only ever used as a goal for paragliding. It is
    -- a goal line perpendicular to the course line followed by half
    -- a cylinder.
    deriving (Eq, Show)

-- | The effective center point of a zone.
center :: Zone -> LatLng [u| rad |]
center (Point x) = x
center (Vector _ x) = x
center (Cylinder _ x) = x
center (Conical _ _ x) = x
center (Line _ x) = x
center (SemiCircle _ x) = x

-- | The effective radius of a zone.
radius :: Zone -> Radius
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

data Task
    = Task
        { zones :: [Zone]
        , startZone :: Int
        , endZone :: Int
        , startGates :: StartGates
        , deadline :: Maybe Deadline
        }
