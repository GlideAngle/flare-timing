{-# LANGUAGE NamedFieldPuns #-}

module Flight.Span.Double
    ( zoneToCylF
    , spanF
    , csF
    , cutF
    , nextCutF
    , dppF
    , csegF
    ) where

import Data.UnitsOfMeasure ((/:))
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Distance (PathDistance, SpanLatLng)
import Flight.Zone (Zone, Bearing(..))
import Flight.Zone.Path (distancePointToPoint, costSegment)
import Flight.Zone.Raw (RawZone)
import Flight.Zone.Cylinder (CircumSample)
import qualified Flight.Earth.Sphere.PointToPoint.Double as Dbl (distanceHaversine)
import qualified Flight.Earth.Sphere.Cylinder.Double as Dbl (circumSample)
import Flight.Task (AngleCut(..))
import Flight.Mask.Internal.Zone (TaskZone, zoneToCylinder)

zoneToCylF :: RawZone -> TaskZone Double
zoneToCylF = zoneToCylinder

spanF :: SpanLatLng Double
spanF = Dbl.distanceHaversine

csF :: CircumSample Double
csF = Dbl.circumSample

cutF :: AngleCut Double
cutF =
    AngleCut
        { sweep = Bearing $ MkQuantity pi
        , nextSweep = nextCutF
        }

nextCutF :: AngleCut Double -> AngleCut Double
nextCutF x@AngleCut{sweep} =
    let (Bearing b) = sweep in x{sweep = Bearing $ b /: 2}

dppF :: SpanLatLng Double -> [Zone Double] -> PathDistance Double
dppF = distancePointToPoint

csegF :: Zone Double -> Zone Double -> PathDistance Double
csegF = costSegment spanF
