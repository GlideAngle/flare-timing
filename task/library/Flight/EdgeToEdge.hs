{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.EdgeToEdge (distanceEdgeToEdge) where

import Prelude hiding (span)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Graph.Inductive.Graph (Node, LEdge)

import Flight.LatLng (AzimuthFwd)
import Flight.Zone (Zone(..))
import Flight.Zone.Cylinder (Tolerance, ZonePoint(..), CircumSample)
import Flight.Units ()
import Flight.ShortestPath
    ( Zs(..)
    , PathCost(..)
    , NodeConnector
    , CostSegment
    , DistancePointToPoint
    , AngleCut
    , GraphBuilder
    , shortestPath
    , buildGraph
    )
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng)

distanceEdgeToEdge
    :: (Real a, Fractional a)
    => AzimuthFwd a
    -> SpanLatLng a
    -> DistancePointToPoint a
    -> CostSegment a
    -> CircumSample a
    -> AngleCut a
    -> Tolerance a
    -> [Zone a]
    -> Zs (PathDistance a)
distanceEdgeToEdge az span distancePointToPoint cseg cs =
    shortestPath az span distancePointToPoint cs builder
    where
        connector :: NodeConnector _
        connector = connectNodes cseg

        builder :: GraphBuilder _
        builder = buildGraph connector

connectNodes :: (Eq a, Ord a) => CostSegment a -> NodeConnector a
connectNodes cost xs ys =
    -- NOTE: The shortest path may traverse a cylinder so I include edges
    -- within a cylinder as well as edges to the next cylinder.
    [ f x1 x2 | x1 <- xs, x2 <- xs, x1 /= x2 ]
    ++
    [ f x y | x <- xs, y <- ys ]
    where
        f :: (Node, ZonePoint _) -> (Node, ZonePoint _) -> LEdge (PathCost _)
        f (i, x) (j, y) = (i, j, PathCost d)
            where
                (TaskDistance (MkQuantity d)) =
                    edgesSum $ cost (Point $ point x) (Point $ point y)
