module Internal.Flat.Projected.Rational
    ( costEastNorth
    , azimuths
    ) where

import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone (Zone(..), center, toRationalZone, realToFracLatLng)
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.Units (realToFrac')
import Internal.Flat.Projected.Internal
    (DistanceAzimuth(..), pythagorean, zoneToProjectedEastNorth, tooFar)

-- | The task distance returned is for the projected UTM plane with eastings
-- and northings. If you need to calculate the distance in spherical
-- coordinates, the latitude and longitude of each vertex of the path can be
-- used to work that out.
costEastNorth
    :: (Real a, Fractional a)
    => Zone a
    -> Zone a
    -> PathDistance a

costEastNorth x@(Point _) y@(Point _) =
    PathDistance
        { edgesSum = d'
        , vertices = realToFracLatLng . center . toRationalZone <$> [x, y]
        }
    where
        d' =
            case (zoneToProjectedEastNorth x, zoneToProjectedEastNorth y) of
                (Right xEN, Right yEN) ->
                    TaskDistance $ realToFrac' dm
                    where
                        distAz :: DistanceAzimuth Double
                        distAz = pythagorean xEN yEN

                        MkQuantity d = dist distAz

                        dm :: Quantity Rational [u| m |]
                        dm = MkQuantity $ toRational d

                _ -> tooFar

costEastNorth x y =
    PathDistance
        { edgesSum = tooFar
        , vertices = realToFracLatLng . center . toRationalZone <$> [x, y]
        }

azimuths
    :: (Real a, Eq b, Fractional b)
    => Zone a
    -> Zone a
    -> Maybe (Quantity b [u| rad |], Quantity b [u| rad |])
azimuths x@(Point _) y@(Point _) =
    case (zoneToProjectedEastNorth x, zoneToProjectedEastNorth y) of
        (Right xEN, Right yEN) ->
            Just (f . azFwd $ distAz, f . azRev $ distAz)
            where
                distAz :: DistanceAzimuth Double
                distAz = pythagorean xEN yEN

        _ -> Nothing

    where
        f (MkQuantity q) = MkQuantity $ realToFrac q

azimuths x y =
    let f = Point . center in azimuths (f x) (f y)
