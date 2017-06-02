module Flight.EdgeToEdge where

import Flight.Zone (Zone(..))
import Flight.PointToPoint (TaskDistance(..), distancePointToPoint)

distanceEdgeToEdge :: [Zone] -> TaskDistance
distanceEdgeToEdge [] = TaskDistance 0
distanceEdgeToEdge [_] = TaskDistance 0
distanceEdgeToEdge xs = distancePointToPoint xs
