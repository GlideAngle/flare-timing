module Flight.Span.Rational
    ( sliver
    , fromZones
    , nextCutR
    , fromR
    ) where

import Data.UnitsOfMeasure ((/:), u, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import qualified Data.Number.FixedFunctions as F

import Flight.LatLng (AzimuthFwd)
import Flight.Distance (PathDistance, SpanLatLng)
import Flight.Zone (Zone, Bearing(..), ArcSweep(..))
import Flight.Zone.MkZones (Zones)
import Flight.Zone.Path (distancePointToPoint, costSegment)
import Flight.Zone.Cylinder (CircumSample)
import qualified Flight.Earth.Sphere.PointToPoint.Rational as Rat
    (azimuthFwd, distanceHaversine)
import qualified Flight.Earth.Sphere.Cylinder.Rational as Rat (circumSample)
import Flight.Task (AngleCut(..))
import Flight.Mask.Internal.Zone (TaskZone, zonesToTaskZones)
import Flight.LatLng.Rational (Epsilon(..), defEps)
import Flight.Comp (EarthMath(..))
import Flight.Span.Sliver (Sliver(..))
import Flight.Distance (QTaskDistance, TaskDistance(..))

fromR :: QTaskDistance Rational [u| m |] -> QTaskDistance Double [u| m |]
fromR (TaskDistance d) = TaskDistance . fromRational' $ d

sliver :: EarthMath -> Sliver Rational
sliver _earthMath =
    Sliver
        { az = azimuthR
        , span = spanR
        , dpp = dppR
        , cseg = csegR
        , cs = csR
        , angleCut = cutR
        }

fromZones :: EarthMath -> Zones -> [TaskZone Rational]
fromZones _earthMath =
    fromZonesR azimuthR

fromZonesR :: AzimuthFwd Rational -> Zones -> [TaskZone Rational]
fromZonesR = zonesToTaskZones

azimuthR :: AzimuthFwd Rational
azimuthR = Rat.azimuthFwd defEps

spanR :: SpanLatLng Rational
spanR = Rat.distanceHaversine defEps

csR :: CircumSample Rational
csR = Rat.circumSample

cutR :: AngleCut Rational
cutR =
    AngleCut
        { sweep =
            let (Epsilon e) = defEps in
            ArcSweep . Bearing . MkQuantity $ 2 * F.pi e
        , nextSweep = nextCutR
        }

nextCutR :: AngleCut Rational -> AngleCut Rational
nextCutR x@AngleCut{sweep = ArcSweep (Bearing b)} =
    x{sweep = ArcSweep . Bearing $ b /: 2}

dppR :: SpanLatLng Rational -> [Zone Rational] -> PathDistance Rational
dppR = distancePointToPoint

csegR :: Zone Rational -> Zone Rational -> PathDistance Rational
csegR = costSegment spanR
