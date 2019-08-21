{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Internal.Flat.Projected.Double
    ( costEastNorth
    , azimuths
    ) where

import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone (Zone(..), center, realToFracZone)
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.Units ()
import Internal.Flat.Projected.Internal
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
    PathDistance
        { edgesSum = d'
        , vertices = center . realToFracZone <$> [x, y]
        }
    where
        d' =
            case (zoneToProjectedEastNorth x, zoneToProjectedEastNorth y) of
                (Right xEN, Right yEN) ->
                    either
                        (const tooFar)
                        (\dAz ->
                            let MkQuantity d = dist dAz

                                dm :: Quantity _ [u| m |]
                                dm = MkQuantity $ realToFrac d

                            in TaskDistance dm)
                        (pythagorean xEN yEN)

                _ -> tooFar

costEastNorth x y =
    PathDistance
        { edgesSum = tooFar
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
            either
                (const Nothing)
                (\dAz -> Just (f . azFwd $ dAz, f . azRev $ dAz))
                (pythagorean xEN yEN)

        _ -> Nothing

    where
        f (MkQuantity q) = MkQuantity $ realToFrac q

azimuths x y =
    let f = Point . center in azimuths (f x) (f y)
