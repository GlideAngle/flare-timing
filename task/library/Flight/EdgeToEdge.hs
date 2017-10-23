module Flight.EdgeToEdge (distanceEdgeToEdge) where

import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Graph.Inductive.Graph (Node, LEdge)

import Flight.Zone (Zone(..))
import Flight.PointToPoint (distancePointToPoint)
import Flight.CylinderEdge (Tolerance, ZonePoint(..))
import Flight.Units ()
import Flight.ShortestPath
    ( PathCost(..)
    , NodeConnector
    , shortestPath
    , buildGraph
    )
import Flight.Distance (TaskDistance(..), PathDistance(..))

distanceEdgeToEdge :: Tolerance -> [Zone] -> PathDistance
distanceEdgeToEdge = shortestPath $ buildGraph connectNodes

-- | NOTE: The shortest path may traverse a cylinder so I include
-- edges within a cylinder as well as edges to the next cylinder.
connectNodes :: NodeConnector
connectNodes xs ys =
    [ f x1 x2 | x1 <- xs, x2 <- xs ]
    ++
    [ f x y | x <- xs, y <- ys ]
    where
        f :: (Node, ZonePoint) -> (Node, ZonePoint) -> LEdge PathCost
        f (i, x) (j, y) = (i, j, PathCost d)
            where
                (TaskDistance (MkQuantity d)) =
                    edgesSum
                    $distancePointToPoint [Point $ point x, Point $ point y]
