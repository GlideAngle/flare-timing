{-# LANGUAGE FlexibleInstances #-}

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

import Flight.Geo (LatLng(..), radToDegLL, defEps)

newtype Radius = Radius Rational deriving (Eq, Ord, Show)
newtype Incline = Incline Rational deriving (Eq, Ord, Show)
newtype Bearing = Bearing Rational deriving (Eq, Ord, Show)

data Zone
    = Point LatLng
    | Vector Bearing LatLng
    | Cylinder Radius LatLng
    | Conical Incline Radius LatLng
    | Line Radius LatLng
    | SemiCircle Radius LatLng
    deriving (Eq, Show)

center :: Zone -> LatLng
center (Point x) = x
center (Vector _ x) = x
center (Cylinder _ x) = x
center (Conical _ _ x) = x
center (Line _ x) = x
center (SemiCircle _ x) = x

radius :: Zone -> Radius
radius (Point _) = Radius 0
radius (Vector _ _) = Radius 0
radius (Cylinder r _) = r
radius (Conical _ r _) = r
radius (Line r _) = r
radius (SemiCircle r _) = r

class ShowAngle a where
    showRadian :: a -> String
    showDegree :: a -> String

instance {-# OVERLAPPING #-} ShowAngle [ Zone ] where
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
        "Cylinder r=" ++ showRadian r ++ " " ++ showRadian x
    showRadian (Conical _ (Radius r) x) =
        "Conical r=" ++ showRadian r ++ " " ++ showRadian x
    showRadian (Line (Radius r) x) =
        "Line r=" ++ showRadian r ++ " " ++ showRadian x
    showRadian (SemiCircle (Radius r) x) =
        "Semicircle r=" ++ showRadian r ++ " " ++ showRadian x

    showDegree (Point x) =
        "Point " ++ showDegree (radToDegLL defEps x)
    showDegree (Vector _ x) =
        "Vector " ++ showDegree (radToDegLL defEps x)
    showDegree (Cylinder (Radius r) x) =
        "Cylinder r=" ++ showDegree r ++ " " ++ showDegree (radToDegLL defEps x)
    showDegree (Conical _ (Radius r) x) =
        "Conical r=" ++ showDegree r ++ " " ++ showDegree (radToDegLL defEps x)
    showDegree (Line (Radius r) x) =
        "Line r=" ++ showDegree r ++ " " ++ showDegree (radToDegLL defEps x)
    showDegree (SemiCircle (Radius r) x) =
        "Semicircle r=" ++ showDegree r ++ " " ++ showDegree (radToDegLL defEps x)

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
        } deriving Show
