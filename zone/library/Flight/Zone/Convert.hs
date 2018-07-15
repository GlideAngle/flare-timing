module Flight.Zone.Convert
    ( fromRationalLatLng, toRationalLatLng
    , fromRationalRadius, toRationalRadius
    , fromRationalZone, toRationalZone
    , realToFracLatLng, realToFracZone, realToFracRadius
    ) where

import Data.UnitsOfMeasure (toRational', fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units (realToFrac')
import Flight.LatLng (QLat, Lat(..), QLng, Lng(..), LatLng(..))
import Flight.Zone.Radius (Radius(..), QRadius)
import Flight.Zone.Bearing (Bearing(..))
import Flight.Zone.Incline (Incline(..))
import Flight.Zone.Zone (Zone(..))

fromRationalRadius :: Fractional a => QRadius Rational u -> QRadius a u
fromRationalRadius (Radius r) =
    Radius $ fromRational' r

toRationalRadius :: Real a => QRadius a u -> QRadius Rational u
toRationalRadius (Radius r) =
    Radius $ toRational' r

realToFracRadius :: (Real a, Fractional b) => QRadius a u -> QRadius b u
realToFracRadius (Radius r) =
    Radius $ realToFrac' r

fromRationalLat :: Fractional a => QLat Rational u -> QLat a u
fromRationalLat (Lat x) =
    Lat $ fromRational' x

fromRationalLng :: Fractional a => QLng Rational u -> QLng a u
fromRationalLng (Lng x) =
    Lng $ fromRational' x

fromRationalLatLng :: Fractional a => LatLng Rational u -> LatLng a u
fromRationalLatLng (LatLng (lat, lng)) =
    LatLng (fromRationalLat lat, fromRationalLng lng)

toRationalLat :: Real a => QLat a u -> QLat Rational u
toRationalLat (Lat x) =
    Lat $ toRational' x

toRationalLng :: Real a => QLng a u -> QLng Rational u
toRationalLng (Lng x) =
    Lng $ toRational' x

toRationalLatLng :: Real a => LatLng a u -> LatLng Rational u
toRationalLatLng (LatLng (lat, lng)) =
    LatLng (toRationalLat lat, toRationalLng lng)

realToFracLat :: (Real a, Fractional b) => QLat a u -> QLat b u
realToFracLat (Lat x) =
    Lat $ realToFrac' x

realToFracLng :: (Real a, Fractional b) => QLng a u -> QLng b u
realToFracLng (Lng x) =
    Lng $ realToFrac' x

realToFracLatLng :: (Real a, Fractional b) => LatLng a u -> LatLng b u
realToFracLatLng (LatLng (lat, lng)) =
    LatLng (realToFracLat lat, realToFracLng lng)

fromRationalZone
    :: (Eq a, Ord a, Fractional a)
    => Zone Rational -> Zone a
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

fromRationalZone (Circle r x) =
    Circle (fromRationalRadius r) (fromRationalLatLng x)

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

toRationalZone (Circle r x) =
    Circle (toRationalRadius r) (toRationalLatLng x)

toRationalZone (SemiCircle r x) =
    SemiCircle (toRationalRadius r) (toRationalLatLng x)

realToFracZone
    :: (Real a, Eq b, Ord b, Fractional b)
    => Zone a -> Zone b
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

realToFracZone (Circle r x) =
    Circle (realToFracRadius r) (realToFracLatLng x)

realToFracZone (SemiCircle r x) =
    SemiCircle (realToFracRadius r) (realToFracLatLng x)
