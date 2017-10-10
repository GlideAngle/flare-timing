module Flight.EdgeToEdge (distanceEdgeToEdge) where

import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Graph.Inductive.Graph (Node, LEdge)

import Flight.Zone (Zone(..))
import Flight.PointToPoint (TaskDistance(..), distancePointToPoint)
import Flight.CylinderEdge (Tolerance, ZonePoint(..))
import Flight.Units ()
import Flight.ShortestPath
    ( DistancePath(..)
    , EdgeDistance(..)
    , PathCost(..)
    , NodeConnector
    , shortestPath
    , buildGraph
    )

distanceEdgeToEdge :: DistancePath
                   -> Tolerance
                   -> [Zone]
                   -> EdgeDistance
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
                    distancePointToPoint [Point $ point x, Point $ point y]
