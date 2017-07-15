module Flight.EdgeToEdge
    ( EdgeDistance(..)
    , DistancePath(..)
    , distanceEdgeToEdge
    , buildGraph
    ) where

import qualified Data.Number.FixedFunctions as F
import Data.Maybe (catMaybes)
import Control.Arrow (first)
import Data.Graph.Inductive.Query.SP (LRTree, spTree) 
import Data.Graph.Inductive.Internal.RootPath (getDistance, getLPathNodes)
import Data.Graph.Inductive.Graph (Graph(..), Node, Path, LEdge, mkGraph, match)
import Data.Graph.Inductive.PatriciaTree (Gr)

import Flight.Geo (LatLng(..), Epsilon(..), defEps)
import Flight.Zone (Zone(..), Bearing(..), center)
import Flight.PointToPoint (TaskDistance(..), distancePointToPoint)
import Flight.Separated (separatedZones)
import Flight.CylinderEdge
    ( Tolerance
    , Samples(..)
    , SampleParams(..)
    , ZonePoint(point)
    , sample
    )

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

distanceEdgeToEdge :: DistancePath -> Tolerance -> [Zone] -> EdgeDistance
distanceEdgeToEdge _ _ [] = zero
distanceEdgeToEdge _ _ [_] = zero
distanceEdgeToEdge dPath tolerance xs =
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
        (d, ptsCenterLine) = distance dPath tolerance xs

distance :: DistancePath
         -> Tolerance
         -> [Zone]
         -> (TaskDistance, [LatLng])
distance _ _ [] = (TaskDistance 0, [])
distance _ _ [_] = (TaskDistance 0, [])
distance dPath tolerance xs
    | not $ separatedZones xs = (TaskDistance 0, [])
    | dPath == PathPointToPoint && length xs < 3 = (pointwise, centers')
    | otherwise =
        case dist of
            Nothing -> (pointwise, centers')
            Just d ->
                if dPath == PathPointToZone || d < pointwise
                    then (d, point <$> zs)
                    else (pointwise, centers')
        where
            pointwise = distancePointToPoint xs
            centers' = center <$> xs
            sp = SampleParams { spSamples = Samples 8, spTolerance = tolerance }
            (Epsilon eps) = defEps
            (dist, zs) = loop sp 4 (Bearing $ F.pi eps) Nothing Nothing xs

loop :: SampleParams
     -> Int
     -> Bearing
     -> Maybe TaskDistance
     -> Maybe [ZonePoint]
     -> [Zone]
     -> (Maybe TaskDistance, [ZonePoint])
loop sp 0 _ d zs _ =
    case zs of
      Nothing -> (Nothing, [])
      Just zs' -> (d, zs')

loop sp n br@(Bearing b) d zs xs =
    loop sp (n - 1) (Bearing $ b / 2) dist (Just zs') xs
    where
        gr :: Gr ZonePoint TaskDistance
        gr = buildGraph sp br zs xs

        (startNode, endNode) = nodeRange gr

        spt :: LRTree TaskDistance
        spt = spTree startNode gr

        dist :: Maybe TaskDistance
        dist = getDistance endNode spt

        ps :: Path
        ps = getLPathNodes endNode spt

        zs' :: [ZonePoint]
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

buildGraph :: SampleParams
           -> Bearing
           -> Maybe [ZonePoint]
           -> [Zone]
           -> Gr ZonePoint TaskDistance
buildGraph sp b zs xs =
    mkGraph flatNodes flatEdges
    where
        nodes' :: [[ZonePoint]]
        nodes' =
            case zs of
              Nothing ->
                  sample sp b Nothing <$> xs

              Just zs' ->
                  zipWith
                      (\z x -> sample sp b (Just z) x)
                      zs'
                      xs

        len :: Int
        len = sum $ map length nodes'

        iiNodes :: [[(Node, ZonePoint)]]
        iiNodes = zip [1 .. ] <$> nodes'

        iNodes :: [[(Node, ZonePoint)]]
        iNodes = zipWith (\i xs -> first (\x -> x + i * len) <$> xs) [1 .. ] iiNodes

        edges' :: [[LEdge TaskDistance]]
        edges' = zipWith g iNodes (tail iNodes)

        flatEdges :: [LEdge TaskDistance]
        flatEdges = concat edges'

        flatNodes :: [(Node, ZonePoint)]
        flatNodes = concat iNodes

        f :: (Node, ZonePoint) -> (Node, ZonePoint) -> LEdge TaskDistance
        f (i, x) (j, y) =
            (i, j, distancePointToPoint [Point $ point x, Point $ point y])

        -- | NOTE: The shortest path may traverse a cylinder so I include
        -- edges within a cylinder as well as edges to the next cylinder.
        g :: [(Node, ZonePoint)] -> [(Node, ZonePoint)] -> [LEdge TaskDistance]
        g xs ys =
            [ f x1 x2 | x1 <- xs, x2 <- xs ] ++ [ f x y | x <- xs, y <- ys]
