{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Ellipsoid.Projected.Double (costEastNorth) where

import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone (Zone(..), center, realToFracZone)
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.Units ()
import Flight.Ellipsoid.Projected.Internal
    (pythagorean , zoneToProjectedEastNorth, tooFar)

-- | The task distance returned is for the projected UTM plane with
-- eastings and northings. If you need to calculate the distance in sperical
-- coordinates, the latitude and longitude of each vertex of the path can be
-- used to work that out.
costEastNorth :: (Real a, Eq b, Fractional b)
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
                (Right xLL, Right yLL) ->
                    TaskDistance dm
                    where
                        d :: Double
                        d = pythagorean xLL yLL

                        dm :: Quantity _ [u| m |]
                        dm = MkQuantity $ realToFrac d

                _ -> tooFar

costEastNorth x y =
    PathDistance { edgesSum = tooFar
                 , vertices = center . realToFracZone <$> [x, y]
                 }
