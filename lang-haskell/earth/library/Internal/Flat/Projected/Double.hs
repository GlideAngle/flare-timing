{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Internal.Flat.Projected.Double
    ( costEastNorth
    , azimuths
    ) where

import Control.Error.Util (hush)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone (Zone(..), center, realToFracZone)
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.Units ()
import Internal.Flat.Projected.Internal
    (DistanceAzimuth(..), pythagorean, zoneToProjectedEastNorth)

-- | The task distance returned is for the projected UTM plane with eastings
-- and northings. If you need to calculate the distance in spherical
-- coordinates, the latitude and longitude of each vertex of the path can be
-- used to work that out.
costEastNorth
    :: (Real a, Eq a, Ord a, Fractional a)
    => Zone a
    -> Zone a
    -> Maybe (PathDistance a)
costEastNorth x@(Point _) y@(Point _) = do
    xEN <- hush $ zoneToProjectedEastNorth x
    yEN <- hush $ zoneToProjectedEastNorth y
    dAz <- hush $ pythagorean xEN yEN

    let MkQuantity d = dist dAz

    let dm :: Quantity _ [u| m |]
        dm = MkQuantity $ realToFrac d

    return $
        PathDistance
            { edgesSum = TaskDistance dm
            , vertices = center . realToFracZone <$> [x, y]
            }

costEastNorth _ _ = Nothing

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
