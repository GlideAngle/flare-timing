module Flight.Zone
    ( HasArea(..)
    , QRadius
    , Radius(..)
    , QIncline
    , Incline(..)
    , QBearing
    , Bearing(..)
    , Zone(..)
    , Deadline(..)
    , TimeOfDay(..)
    , Interval(..)
    , StartGates(..)
    , Task(..)
    , showZoneDMS
    , center
    , radius
    , fromRationalRadius
    , fromRationalZone
    , toRationalRadius
    , toRationalZone
    , realToFracRadius
    , realToFracZone
    , fromRationalLatLng
    , toRationalLatLng
    , realToFracLatLng
    ) where

import Data.UnitsOfMeasure (u, toRational', fromRational', zero)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units (showRadian, realToFrac')
import Flight.Units.DegMinSec (fromQ)
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.Zone.Radius (Radius(..), QRadius)
import Flight.Zone.Bearing (Bearing(..), QBearing)
import Flight.Zone.Incline (Incline(..), QIncline)

-- | Does it have area?
class HasArea a where
    hasArea :: a -> Bool

-- | A control zone of the task. Taken together these make up the course to fly
-- with start enter and exit cylinders, turnpoint cylinders, goal lines and
-- cylinders.
data Zone a where
    -- | Used to mark the exact turnpoints in the optimized task distance.
    Point
        :: Eq a
        => LatLng a [u| rad |]
        -> Zone a

    -- | Used only in open distance tasks these mark the start and direction of
    -- the open distance.
    Vector
        :: Eq a
        => QBearing a [u| rad |]
        -> LatLng a [u| rad |]
        -> Zone a

    -- | The turnpoint cylinder.
    Cylinder
        :: Eq a
        => QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> Zone a

    -- | Only used in paragliding, this is the conical end of speed section
    -- used to discourage too low an end to final glides.
    Conical
        :: Eq a
        => QIncline a [u| rad |]
        -> QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> Zone a

    -- | A goal line perpendicular to the course line.
    Line 
        :: Eq a
        => QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> Zone a

    -- | This control zone is only ever used as a goal for paragliding. It is
    -- a goal line perpendicular to the course line followed by half
    -- a cylinder.
    SemiCircle
        :: Eq a
        => QRadius a [u| m |]
        -> LatLng a [u| rad |]
        -> Zone a

deriving instance Eq (Zone a)
deriving instance
    ( Show (QIncline a [u| rad |])
    , Show (QBearing a [u| rad |])
    , Show (QRadius a [u| m |])
    , Show (LatLng a [u| rad |])
    )
    => Show (Zone a)

instance (Ord a, Num a) => HasArea (Zone a) where
    hasArea (Point _) = False
    hasArea (Vector _ _) = False
    hasArea (Cylinder (Radius x) _) = x > zero
    hasArea (Conical _ (Radius x) _) = x > zero
    hasArea (Line (Radius x) _) = x > zero
    hasArea (SemiCircle (Radius x) _) = x > zero

showZoneDMS :: Zone Double -> String
showZoneDMS (Point (LatLng (Lat x, Lng y))) =
    "Point " ++ show (fromQ x, fromQ y)

showZoneDMS (Vector (Bearing b) (LatLng (Lat x, Lng y))) =
    "Vector " ++ show (fromQ b) ++ " " ++ show (fromQ x, fromQ y)

showZoneDMS (Cylinder r (LatLng (Lat x, Lng y))) =
    "Cylinder " ++ show r ++ " " ++ show (fromQ x, fromQ y)

showZoneDMS (Conical (Incline i) r (LatLng (Lat x, Lng y))) =
    "Conical "
    ++ show (fromQ i)
    ++ " "
    ++ show r
    ++ " "
    ++ show (fromQ x, fromQ y)

showZoneDMS (Line r (LatLng (Lat x, Lng y))) =
    "Line " ++ show r ++ " " ++ show (fromQ x, fromQ y)

showZoneDMS (SemiCircle r (LatLng (Lat x, Lng y))) =
    "SemiCircle " ++ show r ++ " " ++ show (fromQ x, fromQ y)

fromRationalRadius :: Fractional a => QRadius Rational u -> QRadius a u
fromRationalRadius (Radius r) =
    Radius $ fromRational' r

toRationalRadius :: Real a => QRadius a u -> QRadius Rational u
toRationalRadius (Radius r) =
    Radius $ toRational' r

realToFracRadius :: (Real a, Fractional b) => QRadius a u -> QRadius b u
realToFracRadius (Radius r) =
    Radius $ realToFrac' r

fromRationalLat :: Fractional a => Lat Rational u -> Lat a u
fromRationalLat (Lat x) =
    Lat $ fromRational' x

fromRationalLng :: Fractional a => Lng Rational u -> Lng a u
fromRationalLng (Lng x) =
    Lng $ fromRational' x

fromRationalLatLng :: Fractional a => LatLng Rational u -> LatLng a u
fromRationalLatLng (LatLng (lat, lng)) =
    LatLng (fromRationalLat lat, fromRationalLng lng)

toRationalLat :: Real a => Lat a u -> Lat Rational u
toRationalLat (Lat x) =
    Lat $ toRational' x

toRationalLng :: Real a => Lng a u -> Lng Rational u
toRationalLng (Lng x) =
    Lng $ toRational' x

toRationalLatLng :: Real a => LatLng a u -> LatLng Rational u
toRationalLatLng (LatLng (lat, lng)) =
    LatLng (toRationalLat lat, toRationalLng lng)

realToFracLat :: (Real a, Fractional b) => Lat a u -> Lat b u
realToFracLat (Lat x) =
    Lat $ realToFrac' x

realToFracLng :: (Real a, Fractional b) => Lng a u -> Lng b u
realToFracLng (Lng x) =
    Lng $ realToFrac' x

realToFracLatLng :: (Real a, Fractional b) => LatLng a u -> LatLng b u
realToFracLatLng (LatLng (lat, lng)) =
    LatLng (realToFracLat lat, realToFracLng lng)

fromRationalZone :: (Eq a, Fractional a) => Zone Rational -> Zone a
fromRationalZone (Point x) =
    Point $ fromRationalLatLng x

fromRationalZone (Vector (Bearing b) x) =
    Vector (Bearing $ fromRational' b) (fromRationalLatLng x)

fromRationalZone (Cylinder r x) =
    Cylinder (fromRationalRadius r) (fromRationalLatLng x)

fromRationalZone (Conical (Incline i) r x) =
    Conical
        (Incline $ fromRational' i)
        (fromRationalRadius r)
        (fromRationalLatLng x)

fromRationalZone (Line r x) =
    Line (fromRationalRadius r) (fromRationalLatLng x)

fromRationalZone (SemiCircle r x) =
    SemiCircle (fromRationalRadius r) (fromRationalLatLng x)

toRationalZone :: Real a => Zone a -> Zone Rational
toRationalZone (Point x) =
    Point $ toRationalLatLng x

toRationalZone (Vector (Bearing b) x) =
    Vector (Bearing $ toRational' b) (toRationalLatLng x)

toRationalZone (Cylinder r x) =
    Cylinder (toRationalRadius r) (toRationalLatLng x)

toRationalZone (Conical (Incline i) r x) =
    Conical
        (Incline $ toRational' i)
        (toRationalRadius r)
        (toRationalLatLng x)

toRationalZone (Line r x) =
    Line (toRationalRadius r) (toRationalLatLng x)

toRationalZone (SemiCircle r x) =
    SemiCircle (toRationalRadius r) (toRationalLatLng x)

realToFracZone :: (Real a, Eq b, Fractional b) => Zone a -> Zone b
realToFracZone (Point x) =
    Point $ realToFracLatLng x

realToFracZone (Vector (Bearing (MkQuantity b)) x) =
    Vector (Bearing (MkQuantity $ realToFrac b)) (realToFracLatLng x)

realToFracZone (Cylinder r x) =
    Cylinder (realToFracRadius r) (realToFracLatLng x)

realToFracZone (Conical (Incline (MkQuantity i)) r x) =
    Conical
        (Incline (MkQuantity $ realToFrac i))
        (realToFracRadius r)
        (realToFracLatLng x)

realToFracZone (Line r x) =
    Line (realToFracRadius r) (realToFracLatLng x)

realToFracZone (SemiCircle r x) =
    SemiCircle (realToFracRadius r) (realToFracLatLng x)

-- | The effective center point of a zone.
center :: Zone a -> LatLng a [u| rad |]
center (Point x) = x
center (Vector _ x) = x
center (Cylinder _ x) = x
center (Conical _ _ x) = x
center (Line _ x) = x
center (SemiCircle _ x) = x

-- | The effective radius of a zone.
radius :: Num a => Zone a -> QRadius a [u| m |]
radius (Point _) = Radius [u| 0m |]
radius (Vector _ _) = Radius [u| 0m |]
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
