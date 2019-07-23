module Flight.Span.Rational
    ( sliver
    , fromZones
    , nextCutR
    , fromR
    ) where

import Data.UnitsOfMeasure ((/:), u, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import qualified Data.Number.FixedFunctions as F

import Flight.Zone (Bearing(..), ArcSweep(..))
import Flight.Zone.MkZones (Zones)
import Flight.Zone.Path (distancePointToPoint, costSegment)
import qualified Flight.Earth.Sphere.PointToPoint.Rational as RatS
    (azimuthFwd, distanceHaversine)
import qualified Flight.Earth.Sphere.Cylinder.Rational as RatS
    (circumSample)
import qualified Flight.Earth.Ellipsoid.PointToPoint.Rational as RatE
    (azimuthFwd, distanceVincenty)
import qualified Flight.Earth.Ellipsoid.Cylinder.Rational as RatE
    (circumSample)
import Flight.Earth.Ellipsoid (wgs84)
import Flight.Task (AngleCut(..))
import Flight.Mask.Internal.Zone (TaskZone, zonesToTaskZones)
import Flight.LatLng.Rational (Epsilon(..), defEps)
import Flight.Comp (EarthMath(..))
import Flight.Span.Sliver (Sliver(..))
import Flight.Distance (QTaskDistance, TaskDistance(..))

fromR :: QTaskDistance Rational [u| m |] -> QTaskDistance Double [u| m |]
fromR (TaskDistance d) = TaskDistance . fromRational' $ d

sliver :: EarthMath -> Sliver Rational

sliver Vincenty =
    Sliver
        { az = RatE.azimuthFwd defEps wgs84
        , span = RatE.distanceVincenty defEps wgs84
        , dpp = distancePointToPoint
        , cseg = costSegment $ RatE.distanceVincenty defEps wgs84
        , cs = RatE.circumSample
        , angleCut = cutR
        }

sliver Haversines =
    Sliver
        { az = RatS.azimuthFwd defEps
        , span = RatS.distanceHaversine defEps
        , dpp = distancePointToPoint
        , cseg = costSegment $ RatS.distanceHaversine defEps
        , cs = RatS.circumSample
        , angleCut = cutR
        }

sliver _ = sliver Haversines

fromZones :: EarthMath -> Zones -> [TaskZone Rational]
fromZones Vincenty = zonesToTaskZones $ RatE.azimuthFwd defEps wgs84
fromZones Haversines = zonesToTaskZones $ RatS.azimuthFwd defEps
fromZones _ = fromZones Haversines

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
