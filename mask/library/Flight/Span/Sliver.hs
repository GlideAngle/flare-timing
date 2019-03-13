module Flight.Span.Sliver (Sliver(..)) where

import Flight.Distance (SpanLatLng)
import Flight.Zone.Cylinder (CircumSample)
import Flight.Task (CostSegment, DistancePointToPoint, AngleCut(..))

data Sliver a =
    Sliver
        { span :: SpanLatLng a
        , dpp :: DistancePointToPoint a
        , cseg :: CostSegment a
        , cs :: CircumSample a
        , angleCut :: AngleCut a
        }
