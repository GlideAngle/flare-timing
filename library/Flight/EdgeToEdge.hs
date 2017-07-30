{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Flight.EdgeToEdge
    ( EdgeDistance(..)
    , DistancePath(..)
    , distanceEdgeToEdge
    , buildGraph
    ) where

import Data.Ratio ((%))
import qualified Data.Number.FixedFunctions as F
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
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
    , ZonePoint(..)
    , sample
    )
import Flight.Units ()

newtype PathCost = PathCost Rational deriving (Eq, Ord, Num, Real)

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
        , centerLine :: [ LatLng [u| rad |] ]
        -- ^ The points of the 'centers' distance.
        , edgeLine :: [ LatLng [u| rad |] ]
        -- ^ The points of the 'edges' distance.
        }

zeroDistance :: EdgeDistance
zeroDistance =
    EdgeDistance { centers = TaskDistance $ MkQuantity 0
                 , edges = TaskDistance $ MkQuantity 0
                 , centerLine = []
                 , edgeLine = []
                 }

distanceEdgeToEdge :: DistancePath
                   -> Tolerance
                   -> [Zone]
                   -> EdgeDistance
distanceEdgeToEdge _ _ [] = zeroDistance
distanceEdgeToEdge _ _ [_] = zeroDistance
distanceEdgeToEdge dPath tolerance xs =
    case xs of
        [] ->
            zeroDistance

        [_] ->
            zeroDistance

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
        (PathCost pcd, ptsCenterLine) = distance dPath tolerance xs
        d = TaskDistance $ MkQuantity pcd 

distance :: DistancePath
         -> Tolerance
         -> [Zone]
         -> (PathCost, [ LatLng [u| rad |] ])
distance _ _ [] = (PathCost 0, [])
distance _ _ [_] = (PathCost 0, [])
distance dPath tolerance xs
    | not $ separatedZones xs = (PathCost 0, [])
    | dPath == PathPointToPoint && length xs < 3 = (PathCost pointwise, centers')
    | otherwise =
        case dist of
            Nothing -> (PathCost pointwise, centers')
            Just d@(PathCost pcd) ->
                if dPath == PathPointToZone || pcd < pointwise
                    then (d, point <$> zs)
                    else (PathCost pointwise, centers')
        where
            (TaskDistance (MkQuantity pointwise)) = distancePointToPoint xs
            centers' = center <$> xs
            sp = SampleParams { spSamples = Samples 5, spTolerance = tolerance }
            (Epsilon eps) = defEps
            (dist, zs) = loop sp 6 (Bearing . MkQuantity $ F.pi eps) Nothing Nothing xs

loop :: SampleParams
     -> Int
     -> Bearing
     -> Maybe PathCost
     -> Maybe [ZonePoint]
     -> [Zone]
     -> (Maybe PathCost, [ZonePoint])
loop _ 0 _ d zs _ =
    case zs of
      Nothing -> (Nothing, [])
      Just zs' -> (d, zs')

loop sp n br@(Bearing (MkQuantity b)) _ zs xs =
    loop sp (n - 1) (Bearing . MkQuantity $ b * (3 % 4)) dist (Just zs') xs
    where
        gr :: Gr ZonePoint PathCost
        gr = buildGraph sp br zs xs

        (startNode, endNode) = nodeRange gr

        spt :: LRTree PathCost
        spt = spTree startNode gr

        dist :: Maybe PathCost
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
           -> Gr ZonePoint PathCost
buildGraph sp b zs xs =
    mkGraph flatNodes flatEdges
    where
        nodes' :: [[ZonePoint]]
        nodes' =
            case zs of
              Nothing ->
                  sample sp b Nothing <$> xs

              Just zs' ->
                  (\z -> sample sp b (Just z) (sourceZone z)) <$> zs'

        len :: Int
        len = sum $ map length nodes'

        iiNodes :: [[(Node, ZonePoint)]]
        iiNodes = zip [1 .. ] <$> nodes'

        iNodes :: [[(Node, ZonePoint)]]
        iNodes =
            zipWith
            (\i ys -> first (\y -> y + i * len) <$> ys)
            [1 .. ]
            iiNodes

        edges' :: [[LEdge PathCost]]
        edges' = zipWith connectNodes iNodes (tail iNodes)

        flatEdges :: [LEdge PathCost]
        flatEdges = concat edges'

        flatNodes :: [(Node, ZonePoint)]
        flatNodes = concat iNodes

-- | NOTE: The shortest path may traverse a cylinder so I include
-- edges within a cylinder as well as edges to the next cylinder.
connectNodes :: [(Node, ZonePoint)]
             -> [(Node, ZonePoint)]
             -> [LEdge PathCost]
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
