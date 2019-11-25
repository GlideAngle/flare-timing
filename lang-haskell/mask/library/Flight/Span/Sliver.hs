module Flight.Span.Sliver (Sliver(..), GeoSliver(..)) where

import Flight.LatLng (AzimuthFwd)
import Flight.Distance (SpanLatLng)
import Flight.Zone.MkZones (Zones)
import Flight.Zone.Raw (Give)
import Flight.Zone.Path (distancePointToPoint, costSegment)
import Flight.Zone.Cylinder (CircumSample)
import Flight.Task (CostSegment, DistancePointToPoint, AngleCut(..))
import Flight.Geodesy.Solution (Trig, GeodesySolutions(..), GeoZones(..))
import Flight.ShortestPath (GeoPath(..))

import Flight.Mask.Internal.Zone (TaskZone)

data Sliver a =
    Sliver
        { az :: AzimuthFwd a
        , span :: SpanLatLng a
        , dpp :: DistancePointToPoint a
        , cseg :: CostSegment a
        , cs :: CircumSample a
        , cut :: AngleCut a
        }

class (Real a, Fractional a, Trig g a, GeoPath g a) => GeoSliver g a where
    angleCut :: Trig g a => Earth g -> AngleCut g
    fromZones :: Trig g a => Earth g -> Maybe Give -> Zones -> [TaskZone g]

    sliver :: (Real g, Trig g a) => Earth g -> Sliver g
    sliver x =
        Sliver
            { az = azimuthFwd @g @a x
            , span = arcLength @g @a x
            , dpp = distancePointToPoint
            , cseg = costSegment $ arcLength @g @a x
            , cs = circumSample @g @a x
            , cut = angleCut @g @a x
            }
