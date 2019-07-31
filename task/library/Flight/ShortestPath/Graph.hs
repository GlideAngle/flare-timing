{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.ShortestPath.Graph
    ( dedup
    , pad
    , unpad
    , loop
    , buildGraph
    , connectNodes
    ) where

import Prelude hiding (span)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Maybe (catMaybes)
import Control.Arrow (first)
import Data.Graph.Inductive.Query.SP (LRTree, spTree)
import Data.Graph.Inductive.Internal.RootPath (getDistance, getLPathNodes)
import Data.Graph.Inductive.Graph
    (Graph(nodeRange, mkGraph), Node, Path, LEdge, match)
import Data.Graph.Inductive.PatriciaTree (Gr)

import Flight.Zone (Zone(..), ArcSweep(..), center)
import Flight.Zone.Cylinder
    ( SampleParams(..)
    , ZonePoint(..)
    , CircumSample
    , sample
    )
import Flight.Units ()
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng)
import Flight.Geodesy ()
import Flight.ShortestPath.Cost

dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup [x] = [x]
dedup (x : y : ys)
    | x == y = dedup (y : ys)
    | otherwise = x : dedup (y : ys)

pad :: Ord a => [Zone a] -> [Zone a]
pad xs =
    z0 : (xs ++ [zN])
    where
        -- TODO: This is a guarded function. Use liquid haskell to prove
        -- this is safe.
        x0 : _ = xs
        xN : _ = reverse xs

        z0 = Point $ center x0
        zN = Point $ center xN

unpad
    :: (Real a, Fractional a)
    => SpanLatLng a
    -> DistancePointToPoint a
    -> [ZonePoint a]
    -> (Maybe (PathCost a), [ZonePoint a])
unpad span distancePointToPoint xs =
    (Just . PathCost $ d, ys)
    where
        ys = reverse . drop 1 . reverse . drop 1 $ xs
        zs = Point . point <$> ys

        TaskDistance (MkQuantity d) = edgesSum $ distancePointToPoint span zs

loop
    :: Real a
    => GraphBuilder a
    -> CircumSample a
    -> SampleParams a
    -> AngleCut a
    -> Int
    -> Maybe (PathCost a)
    -> Maybe [ZonePoint a]
    -> [Zone a]
    -> (Maybe (PathCost a), [ZonePoint a])
loop _ _ _ _ 0 d zs _ =
    case zs of
      Nothing -> (Nothing, [])
      Just zs' -> (d, zs')

loop builder cs sp cut@AngleCut{sweep, nextSweep} n _ zs xs =
    loop builder cs sp (nextSweep cut) (n - 1) dist (Just zs') xs
    where
        gr :: Gr (ZonePoint _) (PathCost _)
        gr = builder cs sp sweep zs xs

        (startNode, endNode) = nodeRange gr

        spt :: LRTree (PathCost _)
        spt = spTree startNode gr

        dist :: Maybe (PathCost _)
        dist = getDistance endNode spt

        ps :: Path
        ps = getLPathNodes endNode spt

        zs' :: [ZonePoint _]
        zs' =
            catMaybes $
            (\p ->
                case match p gr of
                     (Nothing, _) -> Nothing
                     (Just (_, _, zonePoint, _), _) -> Just zonePoint)
            <$> ps

buildGraph
    :: (Real a, Fractional a)
    => NodeConnector a
    -> CircumSample a
    -> SampleParams a
    -> ArcSweep a [u| rad |]
    -> Maybe [ZonePoint a]
    -> [Zone a]
    -> Gr (ZonePoint a) (PathCost a)
buildGraph f cs sp b zs xs =
    mkGraph flatNodes flatEdges
    where
        nodes' :: [[ZonePoint _]]
        nodes' =
            case zs of
                Nothing ->
                    [ sample cs sp b Nothing xM xN
                    | xM <- Nothing : (Just <$> xs)
                    | xN <- xs
                    ]

                Just zs' ->
                    [ sample cs sp b (Just zN) xM (sourceZone zN)
                    | xM <- Nothing : (Just <$> xs)
                    | zN <- zs'
                    ]

        len :: Int
        len = sum $ map length nodes'

        iiNodes :: [[(Node, ZonePoint _)]]
        iiNodes = zip [1 .. ] <$> nodes'

        iNodes :: [[(Node, ZonePoint _)]]
        iNodes =
            zipWith
            (\i ys -> first (\y -> y + i * len) <$> ys)
            [1 .. ]
            iiNodes

        edges' :: [[LEdge (PathCost _)]]
        edges' = zipWith f iNodes (tail iNodes)

        flatEdges :: [LEdge (PathCost _)]
        flatEdges = concat edges'

        flatNodes :: [(Node, ZonePoint _)]
        flatNodes = concat iNodes

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
