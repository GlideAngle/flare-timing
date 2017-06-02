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
    , TaskDistance(..)
    , earthRadius
    , distanceEdgeToEdge
    , distancePointToPoint
    , distanceHaversine
    , distanceHaversineF
    , separatedZones
    ) where

import Data.List (intercalate)
import Data.Ratio((%))
import qualified Data.Number.FixedFunctions as F

newtype LatLng = LatLng (Rational, Rational) deriving (Eq, Ord, Show)
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

class ShowAngle a where
    showRadian :: a -> String
    showDegree :: a -> String

instance {-# OVERLAPPING #-} ShowAngle [ Zone ] where
    showRadian = showZones showRadian
    showDegree = showZones showDegree

showZones :: (Zone -> String) -> [Zone] -> String
showZones f xs = intercalate ", " $ f <$> xs

instance ShowAngle Rational where
    showRadian x = show (fromRational x :: Double)
    showDegree x = show (fromRational x :: Double) ++ "°"

showLatLng :: (Rational -> String) -> LatLng -> String
showLatLng f (LatLng (lat, lng))= "(" ++ f lat ++ ", " ++ f lng ++ ")"

radToDeg :: Epsilon -> Rational -> Rational
radToDeg (Epsilon eps) x = x * (180 % 1) / F.pi eps

radToDegLL :: Epsilon -> LatLng -> LatLng
radToDegLL e (LatLng (lat, lng)) =
    LatLng (radToDeg e lat, radToDeg e lng)

defEps :: Epsilon
defEps = Epsilon $ 1 % 1000000000

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

newtype TaskDistance = TaskDistance Rational deriving (Eq, Ord, Show)
newtype Epsilon = Epsilon Rational deriving (Eq, Ord, Show)

center :: Zone -> LatLng
center (Point x) = x
center (Vector _ x) = x
center (Cylinder _ x) = x
center (Conical _ _ x) = x
center (Line _ x) = x
center (SemiCircle _ x) = x

earthRadius :: Rational
earthRadius = 6371000

distanceHaversineF :: LatLng -> LatLng -> TaskDistance
distanceHaversineF (LatLng (xLat, xLng)) (LatLng (yLat, yLng)) =
    TaskDistance $ earthRadius * toRational radDist 
    where
        distLat :: Rational
        distLat = yLat - xLat
         
        distLng :: Rational
        distLng = yLng - xLng

        haversine :: Rational -> Double
        haversine x =
            y * y
            where
                y :: Double
                y = sin $ fromRational (x * (1 % 2))

        a :: Double
        a =
            haversine distLat
            + cos (fromRational xLat)
            * cos (fromRational yLat)
            * haversine distLng

        radDist :: Double
        radDist = 2 * atan2 (sqrt a) (sqrt $ 1 - a)

distanceHaversine :: Epsilon -> LatLng -> LatLng -> TaskDistance
distanceHaversine (Epsilon eps) (LatLng (xLat, xLng)) (LatLng (yLat, yLng)) =
    TaskDistance $ 6371000 * radDist 
    where
        distLat :: Rational
        distLat = yLat - xLat
         
        distLng :: Rational
        distLng = yLng - xLng

        haversine :: Rational -> Rational
        haversine x =
            y * y
            where
                y :: Rational
                y = F.sin eps (x * (1 % 2))

        a :: Rational
        a =
            haversine distLat
            + F.cos eps xLat
            * F.cos eps yLat
            * haversine distLng

        radDist :: Rational
        radDist = 2 * F.atan eps ((F.sqrt eps a) / (F.sqrt eps $ 1 - a))

distancePointToPoint :: [Zone] -> TaskDistance
distancePointToPoint xs =
    TaskDistance $ sum $ zipWith f ys (tail ys)
    where
        ys = center <$> xs
        unwrap (TaskDistance x) = x
        f = (unwrap .) . distanceHaversine defEps

distanceEdgeToEdge :: [Zone] -> TaskDistance
distanceEdgeToEdge _ = TaskDistance 0

radius :: Zone -> Radius
radius (Point _) = Radius 0
radius (Vector _ _) = Radius 0
radius (Cylinder x _) = x
radius (Conical _ x _) = x
radius (Line x _) = x
radius (SemiCircle x _) = x

separated :: Zone -> Zone -> Bool
separated x y =
    d > rxy
    where
        (Radius rx) = radius x
        (Radius ry) = radius y
        rxy = rx + ry
        (TaskDistance d) = distancePointToPoint [x, y]

separatedZones :: [Zone] -> Bool
separatedZones xs =
    and $ zipWith separated xs (tail xs)
