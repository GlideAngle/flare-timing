module Flight.Span.Double
    ( sliver
    , fromZones
    , nextCutF
    ) where

import Data.UnitsOfMeasure ((/:))
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone (Bearing(..), ArcSweep(..))
import Flight.Zone.MkZones (Zones)
import Flight.Zone.Path (distancePointToPoint, costSegment)
import qualified Flight.Earth.Sphere.PointToPoint.Double as DblS
    (azimuthFwd, distanceHaversine)
import qualified Flight.Earth.Sphere.Cylinder.Double as DblS
    (circumSample)
import qualified Flight.Earth.Ellipsoid.PointToPoint.Double as DblE
    (azimuthFwd, distance)
import qualified Flight.Earth.Ellipsoid.Cylinder.Double as DblE
    (circumSample)
import Flight.Earth.Geodesy (EarthMath(..))
import Flight.Earth.Ellipsoid (wgs84)
import Flight.Task (AngleCut(..))
import Flight.Mask.Internal.Zone (TaskZone, zonesToTaskZones)
import Flight.Span.Sliver (Sliver(..))

sliver :: EarthMath -> Sliver Double

sliver Pythagorus = sliver Haversines

sliver Haversines =
    Sliver
        { az = DblS.azimuthFwd
        , span = DblS.distanceHaversine
        , dpp = distancePointToPoint
        , cseg = costSegment $ DblS.distanceHaversine
        , cs = DblS.circumSample
        , angleCut = cutF
        }

sliver em@Vincenty =
    Sliver
        { az = DblE.azimuthFwd em wgs84
        , span = DblE.distance em wgs84
        , dpp = distancePointToPoint
        , cseg = costSegment $ DblE.distance Vincenty wgs84
        , cs = DblE.circumSample
        , angleCut = cutF
        }

sliver em@AndoyerLambert =
    Sliver
        { az = DblE.azimuthFwd em wgs84
        , span = DblE.distance em wgs84
        , dpp = distancePointToPoint
        , cseg = costSegment $ DblE.distance Vincenty wgs84
        , cs = DblE.circumSample
        , angleCut = cutF
        }

sliver em@ForsytheAndoyerLambert =
    Sliver
        { az = DblE.azimuthFwd em wgs84
        , span = DblE.distance em wgs84
        , dpp = distancePointToPoint
        , cseg = costSegment $ DblE.distance Vincenty wgs84
        , cs = DblE.circumSample
        , angleCut = cutF
        }

fromZones :: EarthMath -> Zones -> [TaskZone Double]
fromZones Pythagorus = fromZones Haversines
fromZones Haversines = zonesToTaskZones $ DblS.azimuthFwd
fromZones em@Vincenty = zonesToTaskZones $ DblE.azimuthFwd em wgs84
fromZones em@AndoyerLambert = zonesToTaskZones $ DblE.azimuthFwd em wgs84
fromZones em@ForsytheAndoyerLambert = zonesToTaskZones $ DblE.azimuthFwd em wgs84

cutF :: AngleCut Double
cutF =
    AngleCut
        { sweep = ArcSweep . Bearing . MkQuantity $ 2 * pi
        , nextSweep = nextCutF
        }

nextCutF :: AngleCut Double -> AngleCut Double
nextCutF x@AngleCut{sweep = ArcSweep (Bearing b)} =
    x{sweep = ArcSweep . Bearing $ b /: 2}
