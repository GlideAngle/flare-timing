module Flight.Span.Double
    ( sliver
    , fromZones
    , nextCutF
    ) where

import Data.UnitsOfMeasure ((/:))
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (AzimuthFwd)
import Flight.Distance (PathDistance, SpanLatLng)
import Flight.Zone (Zone, Bearing(..), ArcSweep(..))
import Flight.Zone.MkZones (Zones)
import Flight.Zone.Path (distancePointToPoint, costSegment)
import Flight.Zone.Cylinder (CircumSample)
import qualified Flight.Earth.Sphere.PointToPoint.Double as Dbl
    (azimuthFwd, distanceHaversine)
import qualified Flight.Earth.Sphere.Cylinder.Double as Dbl (circumSample)
import Flight.Task (AngleCut(..))
import Flight.Mask.Internal.Zone (TaskZone, zonesToTaskZones)
import Flight.Comp (EarthMath(..))
import Flight.Span.Sliver (Sliver(..))

sliver :: EarthMath -> Sliver Double
sliver _earthMath =
    Sliver
        { az = azimuthF
        , span = spanF
        , dpp = dppF
        , cseg = csegF
        , cs = csF
        , angleCut = cutF
        }

fromZones :: EarthMath -> Zones -> [TaskZone Double]
fromZones _earthMath =
    fromZonesF azimuthF

fromZonesF :: AzimuthFwd Double -> Zones -> [TaskZone Double]
fromZonesF = zonesToTaskZones

azimuthF :: AzimuthFwd Double
azimuthF = Dbl.azimuthFwd

spanF :: SpanLatLng Double
spanF = Dbl.distanceHaversine

csF :: CircumSample Double
csF = Dbl.circumSample

cutF :: AngleCut Double
cutF =
    AngleCut
        { sweep = ArcSweep . Bearing . MkQuantity $ 2 * pi
        , nextSweep = nextCutF
        }

nextCutF :: AngleCut Double -> AngleCut Double
nextCutF x@AngleCut{sweep = ArcSweep (Bearing b)} =
    x{sweep = ArcSweep . Bearing $ b /: 2}

dppF :: SpanLatLng Double -> [Zone Double] -> PathDistance Double
dppF = distancePointToPoint

csegF :: Zone Double -> Zone Double -> PathDistance Double
csegF = costSegment spanF
