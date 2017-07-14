module Flight.EdgeToEdge
    ( EdgeDistance(..)
    , DistancePath(..)
    , distanceEdgeToEdge
    , buildGraph
    ) where

import Data.Maybe (catMaybes)
import Control.Arrow (first)
import Data.Graph.Inductive.Query.SP (LRTree, spTree) 
import Data.Graph.Inductive.Internal.RootPath (getDistance, getLPathNodes)
import Data.Graph.Inductive.Graph (Graph(..), Node, Path, LEdge, mkGraph, match)
import Data.Graph.Inductive.PatriciaTree (Gr)

import Flight.Geo (LatLng(..))
import Flight.Zone (Zone(..), center)
import Flight.PointToPoint (TaskDistance(..), distancePointToPoint)
import Flight.Separated (separatedZones)
import Flight.CylinderEdge (Tolerance, Samples(..), SampleParams(..), sample)

data DistancePath
    = PathPointToPoint
    | PathPointToZone
    deriving (Eq, Show)

data EdgeDistance =
    EdgeDistance
        { centers :: TaskDistance
        -- ^ The distance from the center of the first zone to the center of
        -- the last zone.
        , edges :: TaskDistance
        -- ^ The distance from the edge of the first zone to the edge of
        -- the last zone.
        , centerLine :: [LatLng]
        -- ^ The points of the 'centers' distance.
        , edgeLine :: [LatLng]
        -- ^ The points of the 'edges' distance.
        } deriving Show

zero :: EdgeDistance
zero =
    EdgeDistance { centers = TaskDistance 0
                 , edges = TaskDistance 0
                 , centerLine = []
                 , edgeLine = []
                 }

distanceEdgeToEdge :: Tolerance -> DistancePath -> [Zone] -> EdgeDistance
distanceEdgeToEdge _ _ [] = zero
distanceEdgeToEdge _ _ [_] = zero
distanceEdgeToEdge tolerance dPath xs =
    case xs of
        [] ->
            zero

        [_] ->
            zero

        [_, _] ->
            EdgeDistance { centers = d
                         , edges = d
                         , centerLine = ptsCenterLine
                         , edgeLine = ptsCenterLine
                         }

        (_ : _) ->
            EdgeDistance { centers = d
                         , edges = d'
                         , centerLine = ptsCenterLine
                         , edgeLine = ptsEdgeLine
                         }

            where
                ptsEdgeLine =
                    drop 1
                    $ reverse
                    $ drop 1
                    $ reverse ptsCenterLine

                d' = distancePointToPoint (Point <$> ptsEdgeLine)

    where
        sp = SampleParams { samples = Samples 100, tolerance = tolerance }
        (d, ptsCenterLine) = distance sp dPath xs

distance :: SampleParams -> DistancePath -> [Zone] -> (TaskDistance, [LatLng])
distance _ _ [] = (TaskDistance 0, [])
distance _ _ [_] = (TaskDistance 0, [])
distance sp dPath xs
    | not $ separatedZones xs = (TaskDistance 0, [])
    | dPath == PathPointToPoint && length xs < 3 = (pointwise, centers')
    | otherwise =
        case dist of
            Nothing -> (pointwise, centers')
            Just y ->
                if dPath == PathPointToZone || y < pointwise
                    then (y, ys)
                    else (pointwise, centers')
        where
            pointwise = distancePointToPoint xs
            centers' = center <$> xs

            gr :: Gr LatLng TaskDistance
            gr = buildGraph sp xs

            (startNode, endNode) = nodeRange gr

            spt :: LRTree TaskDistance
            spt = spTree startNode gr

            dist :: Maybe TaskDistance
            dist = getDistance endNode spt

            ps :: Path
            ps = getLPathNodes endNode spt

            ys :: [LatLng]
            ys =
                catMaybes $
                (\p ->
                    case match p gr of
                         (Nothing, _) -> Nothing
                         (Just (_, _, latlng, _), _) -> Just latlng
                )
                <$> ps

buildGraph :: SampleParams -> [Zone] -> Gr LatLng TaskDistance
buildGraph sp zones =
    mkGraph flatNodes flatEdges
    where
        nodes' :: [[LatLng]]
        nodes' = sample sp <$> zones

        len :: Int
        len = sum $ map length nodes'

        iiNodes :: [[(Node, LatLng)]]
        iiNodes = zip [1 .. ] <$> nodes'

        iNodes :: [[(Node, LatLng)]]
        iNodes = zipWith (\i xs -> first (\x -> x + i * len) <$> xs) [1 .. ] iiNodes

        edges' :: [[LEdge TaskDistance]]
        edges' = zipWith g iNodes (tail iNodes)

        flatEdges :: [LEdge TaskDistance]
        flatEdges = concat edges'

        flatNodes :: [(Node, LatLng)]
        flatNodes = concat iNodes

        f :: (Node, LatLng) -> (Node, LatLng) -> LEdge TaskDistance
        f (i, x) (j, y) = (i, j, distancePointToPoint [Point x, Point y])

        -- | NOTE: The shortest path may traverse a cylinder so I include
        -- edges within a cylinder as well as edges to the next cylinder.
        g :: [(Node, LatLng)] -> [(Node, LatLng)] -> [LEdge TaskDistance]
        g xs ys =
            [ f x1 x2 | x1 <- xs, x2 <- xs ] ++ [ f x y | x <- xs, y <- ys]
