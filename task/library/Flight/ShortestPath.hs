{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.ShortestPath
    ( Zs(..)
    , PathCost(..)
    , GraphBuilder
    , NodeConnector
    , CostSegment
    , DistancePointToPoint
    , AngleCut(..)
    , buildGraph
    , shortestPath 
    , fromZs
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

import Flight.LatLng (LatLng(..))
import Flight.Zone (Zone(..), ArcSweep(..), center)
import Flight.Zone.Cylinder
    ( Tolerance(..)
    , Samples(..)
    , SampleParams(..)
    , ZonePoint(..)
    , CircumSample
    , sample
    )
import Flight.Units ()
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng)
import Flight.Earth.Sphere.Separated (separatedZones)

type CostSegment a = Zone a -> Zone a -> PathDistance a

type NodeConnector a =
    [(Node, ZonePoint a)] -> [(Node, ZonePoint a)] -> [LEdge (PathCost a)]

type GraphBuilder a =
    CircumSample a
    -> SampleParams a
    -> ArcSweep a [u| rad |]
    -> Maybe [ZonePoint a]
    -> [Zone a]
    -> Gr (ZonePoint a) (PathCost a)

newtype PathCost a =
    PathCost a
    deriving (Eq, Ord)
    deriving newtype (Num, Real)

data Zs a
    = Zs a -- ^ All good, here's the wrapped value.
    | Z0 -- ^ No items when 2+ required.
    | Z1 -- ^ Only 1 item when 2+ required.
    | ZxNotSeparated -- ^ Zones are not separated.
    deriving (Eq, Ord, Functor)

deriving instance Show a => Show (Zs a)

fromZs :: Zs a -> Maybe a
fromZs (Zs a) = Just a
fromZs _ = Nothing

-- | A point to point distance with path function.
type DistancePointToPoint a = SpanLatLng a -> [Zone a] -> PathDistance a

-- | When searching some angles can be excluded. These are not in the initial
-- sweep. During the search the sweep angle is reduced by the next sweep
-- function.
data AngleCut a =
    AngleCut
        { sweep :: ArcSweep a [u| rad |]
        , nextSweep :: AngleCut a -> AngleCut a
        }

shortestPath
    :: (Real a, Fractional a)
    => SpanLatLng a
    -> DistancePointToPoint a
    -> CircumSample a
    -> GraphBuilder a
    -> AngleCut a
    -> Tolerance a
    -> [Zone a]
    -> Zs (PathDistance a)
shortestPath _ _ _ _ _ _ [] = Z0
shortestPath _ _ _ _ _ _ [_] = Z1
shortestPath span distancePointToPoint cs builder angleCut tolerance xs =
    case xs of
        [] -> Z0
        [_] -> Z1
        (_ : _) ->
            case zd of
                (Z0, _) -> Z0
                (Z1, _) -> Z1
                (ZxNotSeparated, _) -> ZxNotSeparated
                (Zs (PathCost pcd), ptsCenterLine) ->
                    Zs PathDistance
                        { edgesSum = TaskDistance $ MkQuantity pcd
                        , vertices = ptsCenterLine
                        }
    where
        zd =
            distance
                span distancePointToPoint cs builder angleCut tolerance xs


distance
    :: (Real a, Fractional a)
    => SpanLatLng a
    -> DistancePointToPoint a
    -> CircumSample a
    -> GraphBuilder a
    -> AngleCut a
    -> Tolerance a
    -> [Zone a]
    -> (Zs (PathCost a), [LatLng a [u| rad |]])
distance _ _ _ _ _ _ [] = (Z0, [])
distance _ _ _ _ _ _ [_] = (Z1, [])
distance span distancePointToPoint cs builder cut tolerance xs
    | not $ separatedZones span xs = (ZxNotSeparated, [])
    | otherwise =
        first Zs $
        case dist of
            Nothing -> (PathCost pointwise, edgesSum')
            Just d@(PathCost pcd) ->
                if pcd < pointwise
                    then (d, point <$> zs)
                    else (PathCost pointwise, edgesSum')
        where
            (TaskDistance (MkQuantity pointwise)) =
                edgesSum $ distancePointToPoint span xs

            edgesSum' = center <$> xs
            sp = SampleParams { spSamples = Samples 5, spTolerance = tolerance }

            -- NOTE: I need to add a zone at each end to define the start and
            -- end for the shortest path. Once the shortest path is found
            -- I then need to undo the padding.
            (_, ys) = loop builder cs sp cut 6 Nothing Nothing $ pad xs
            (dist, zs) = unpad span distancePointToPoint ys

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
                     (Nothing, _) ->
                         Nothing

                     (Just (_, _, zonePoint, _), _) ->
                         Just zonePoint
            )
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
                    sample cs sp b Nothing <$> xs

                Just zs' ->
                    (\z -> sample cs sp b (Just z) (sourceZone z)) <$> zs'

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
