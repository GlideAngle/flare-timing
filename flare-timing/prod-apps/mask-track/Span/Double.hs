{-# LANGUAGE NamedFieldPuns #-}

module Span.Double
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

import qualified Flight.PointToPoint.Double as Dbl
    (distanceHaversine, distancePointToPoint, costSegment)
import qualified Flight.Cylinder.Double as Dbl (circumSample)
import Flight.Distance (PathDistance)
import Flight.Zone (Zone, Bearing(..))
import Flight.Zone.Raw (RawZone)
import Flight.Task (SpanLatLng, CircumSample, AngleCut(..))
import Flight.Mask (TaskZone, zoneToCylinder)

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
dppF = Dbl.distancePointToPoint

csegF :: Zone Double -> Zone Double -> PathDistance Double
csegF = Dbl.costSegment spanF
