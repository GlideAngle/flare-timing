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
    ( ShowAngle(..)
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

import Data.List (intercalate)
import Data.UnitsOfMeasure
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Defs ()

import Flight.Units (Length)
import Flight.Geo (LatLng(..), radToDegLL, defEps)

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
data Zone
    = Point LatLng
    -- ^ Used to mark the exact turnpoints in the optimized task distance.
    | Vector Bearing LatLng
    -- ^ Used only in open distance tasks these mark the start and direction of
    -- the open distance.
    | Cylinder Radius LatLng
    -- ^ The turnpoint cylinder.
    | Conical Incline Radius LatLng
    -- ^ Only used in paragliding, this is the conical end of speed section
    -- used to discourage too low an end to final glides.
    | Line Radius LatLng
    -- ^ A goal line perpendicular to the course line.
    | SemiCircle Radius LatLng
    -- ^ This control zone is only ever used as a goal for paragliding. It is
    -- a goal line perpendicular to the course line followed by half
    -- a cylinder.
    deriving (Eq, Show)

-- | The effective center point of a zone.
center :: Zone -> LatLng
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

class ShowAngle a where
    showRadian :: a -> String
    showDegree :: a -> String

instance {-# OVERLAPPING #-} ShowAngle [Zone] where
    showRadian = showZones showRadian
    showDegree = showZones showDegree

showZones :: (Zone -> String) -> [Zone] -> String
showZones f xs = intercalate ", " $ f <$> xs

instance ShowAngle Rational where
    showRadian = show
    showDegree x = show x ++ "°"

showLatLng :: (Rational -> String) -> LatLng -> String
showLatLng f (LatLng (lat, lng))= "(" ++ f lat ++ ", " ++ f lng ++ ")"

instance ShowAngle LatLng where
    showRadian = showLatLng showRadian
    showDegree = showLatLng showDegree

instance ShowAngle Zone where
    showRadian (Point x) =
        "Point " ++ showRadian x
    showRadian (Vector _ x) =
        "Vector " ++ showRadian x
    showRadian (Cylinder (Radius r) x) =
        "Cylinder r=" ++ show r ++ " " ++ showRadian x
    showRadian (Conical _ (Radius r) x) =
        "Conical r=" ++ show r ++ " " ++ showRadian x
    showRadian (Line (Radius r) x) =
        "Line r=" ++ show r ++ " " ++ showRadian x
    showRadian (SemiCircle (Radius r) x) =
        "Semicircle r=" ++ show r ++ " " ++ showRadian x

    showDegree (Point x) =
        "Point " ++ showDegree (radToDegLL defEps x)
    showDegree (Vector _ x) =
        "Vector " ++ showDegree (radToDegLL defEps x)
    showDegree (Cylinder (Radius r) x) =
        "Cylinder r=" ++ show r ++ " " ++ showDegree (radToDegLL defEps x)
    showDegree (Conical _ (Radius r) x) =
        "Conical r=" ++ show r ++ " " ++ showDegree (radToDegLL defEps x)
    showDegree (Line (Radius r) x) =
        "Line r=" ++ show r ++ " " ++ showDegree (radToDegLL defEps x)
    showDegree (SemiCircle (Radius r) x) =
        "Semicircle r=" ++ show r ++ " " ++ showDegree (radToDegLL defEps x)

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
        { zones :: [Zone]
        , startZone :: Int
        , endZone :: Int
        , startGates :: StartGates
        , deadline :: Maybe Deadline
        } deriving Show
