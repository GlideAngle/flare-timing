{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Earth.Flat.Projected.Double
    ( costEastNorth
    , azimuths
    ) where

import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone (Zone(..), center, realToFracZone)
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.Units ()
import Flight.Earth.Flat.Projected.Internal
    (DistanceAzimuth(..), pythagorean, zoneToProjectedEastNorth, tooFar)

-- | The task distance returned is for the projected UTM plane with eastings
-- and northings. If you need to calculate the distance in spherical
-- coordinates, the latitude and longitude of each vertex of the path can be
-- used to work that out.
costEastNorth
    :: (Real a, Eq b, Ord b, Fractional b)
    => Zone a
    -> Zone a
    -> PathDistance b
costEastNorth x@(Point _) y@(Point _) =
    PathDistance { edgesSum = d'
                 , vertices = center . realToFracZone <$> [x, y]
                 }
    where
        d' =
            case (zoneToProjectedEastNorth x, zoneToProjectedEastNorth y) of
                (Right xEN, Right yEN) ->
                    TaskDistance dm
                    where
                        distAz :: DistanceAzimuth Double
                        distAz = pythagorean xEN yEN

                        MkQuantity d = dist distAz

                        dm :: Quantity _ [u| m |]
                        dm = MkQuantity $ realToFrac d

                _ -> tooFar

costEastNorth x y =
    PathDistance { edgesSum = tooFar
                 , vertices = center . realToFracZone <$> [x, y]
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
    azimuths (f x) (f y)
    where
        f = Point . center
