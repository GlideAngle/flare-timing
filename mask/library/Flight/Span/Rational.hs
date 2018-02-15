{-# LANGUAGE NamedFieldPuns #-}

module Flight.Span.Rational
    ( zoneToCylR
    , spanR
    , csR
    , cutR
    , nextCutR
    , dppR
    , csegR
    ) where

import Data.UnitsOfMeasure ((/:))
import Data.UnitsOfMeasure.Internal (Quantity(..))
import qualified Data.Number.FixedFunctions as F

import Flight.Distance (PathDistance, SpanLatLng)
import Flight.Zone (Zone, Bearing(..))
import Flight.Zone.Path (distancePointToPoint, costSegment)
import Flight.Zone.Raw (RawZone)
import Flight.Zone.Cylinder (CircumSample)
import qualified Flight.Earth.Sphere.PointToPoint.Rational as Rat (distanceHaversine)
import qualified Flight.Earth.Sphere.Cylinder.Rational as Rat (circumSample)
import Flight.Task (AngleCut(..))
import Flight.Mask.Internal.Zone (TaskZone, zoneToCylinder)
import Flight.LatLng.Rational (Epsilon(..), defEps)

zoneToCylR :: RawZone -> TaskZone Rational
zoneToCylR = zoneToCylinder

spanR :: SpanLatLng Rational
spanR = Rat.distanceHaversine defEps

csR :: CircumSample Rational
csR = Rat.circumSample

cutR :: AngleCut Rational
cutR =
    AngleCut
        { sweep = let (Epsilon e) = defEps in Bearing . MkQuantity $ F.pi e
        , nextSweep = nextCutR
        }

nextCutR :: AngleCut Rational -> AngleCut Rational
nextCutR x@AngleCut{sweep} =
    let (Bearing b) = sweep in x{sweep = Bearing $ b /: 2}

dppR :: SpanLatLng Rational -> [Zone Rational] -> PathDistance Rational
dppR = distancePointToPoint

csegR :: Zone Rational -> Zone Rational -> PathDistance Rational
csegR = costSegment spanR
