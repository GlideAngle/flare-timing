module Flight.EdgeToEdge (distanceEdgeToEdge) where

import Prelude hiding (span)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Graph.Inductive.Graph (Node, LEdge)

import Flight.Zone (Zone(..))
import Flight.CylinderEdge (Tolerance, ZonePoint(..))
import Flight.Units ()
import Flight.ShortestPath
    ( PathCost(..)
    , NodeConnector
    , CostSegment
    , shortestPath
    , buildGraph
    )
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.PointToPoint (SpanLatLng)

distanceEdgeToEdge :: SpanLatLng
                   -> CostSegment
                   -> Tolerance
                   -> [Zone]
                   -> PathDistance
distanceEdgeToEdge span = (shortestPath span) . buildGraph . connectNodes

-- | NOTE: The shortest path may traverse a cylinder so I include
-- edges within a cylinder as well as edges to the next cylinder.
connectNodes :: CostSegment -> NodeConnector
connectNodes cost xs ys =
    [ f x1 x2 | x1 <- xs, x2 <- xs ]
    ++
    [ f x y | x <- xs, y <- ys ]
    where
        f :: (Node, ZonePoint) -> (Node, ZonePoint) -> LEdge PathCost
        f (i, x) (j, y) = (i, j, PathCost d)
            where
                (TaskDistance (MkQuantity d)) =
                    edgesSum $ cost (Point $ point x) (Point $ point y)
