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

module Flight.ShortestPath
    ( EdgeDistance(..)
    , PathCost(..)
    , GraphBuilder
    , NodeConnector
    , buildGraph
    , shortestPath 
    ) where

import Data.Ratio ((%))
import qualified Data.Number.FixedFunctions as F
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Maybe (catMaybes)
import Control.Arrow (first)
import Data.Graph.Inductive.Query.SP (LRTree, spTree) 
import Data.Graph.Inductive.Internal.RootPath (getDistance, getLPathNodes)
import Data.Graph.Inductive.Graph (Graph(..), Node, Path, LEdge, match)
import Data.Graph.Inductive.PatriciaTree (Gr)

import Flight.LatLng (LatLng(..), Epsilon(..), defEps)
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

type NodeConnector =
    [(Node, ZonePoint)] -> [(Node, ZonePoint)] -> [LEdge PathCost]

type GraphBuilder =
    SampleParams
    -> Bearing
    -> Maybe [ZonePoint]
    -> [Zone]
    -> Gr ZonePoint PathCost

newtype PathCost = PathCost Rational deriving (Eq, Ord, Num, Real)

data EdgeDistance =
    EdgeDistance
        { centers :: TaskDistance
        -- ^ The distance from the center of the first zone to the center of
        -- the last zone.
        , centerLine :: [ LatLng [u| rad |] ]
        -- ^ The points of the 'centers' distance.
        }

zeroDistance :: EdgeDistance
zeroDistance =
    EdgeDistance { centers = TaskDistance $ MkQuantity 0
                 , centerLine = []
                 }

shortestPath :: GraphBuilder
             -> Tolerance
             -> [Zone]
             -> EdgeDistance
shortestPath _ _ [] = zeroDistance
shortestPath _ _ [_] = zeroDistance
shortestPath builder tolerance xs =
    case xs of
        [] ->
            zeroDistance

        [_] ->
            zeroDistance

        (_ : _) ->
            EdgeDistance { centers = d
                         , centerLine = ptsCenterLine
                         }
    where
        (PathCost pcd, ptsCenterLine) = distance builder tolerance xs
        d = TaskDistance $ MkQuantity pcd 

distance :: GraphBuilder
         -> Tolerance
         -> [Zone]
         -> (PathCost, [ LatLng [u| rad |] ])
distance _ _ [] = (PathCost 0, [])
distance _ _ [_] = (PathCost 0, [])
distance builder tolerance xs
    | not $ separatedZones xs = (PathCost 0, [])
    | otherwise =
        case dist of
            Nothing -> (PathCost pointwise, centers')
            Just d@(PathCost pcd) ->
                if pcd < pointwise
                    then (d, point <$> zs)
                    else (PathCost pointwise, centers')
        where
            (TaskDistance (MkQuantity pointwise)) = distancePointToPoint xs
            centers' = center <$> xs
            sp = SampleParams { spSamples = Samples 5, spTolerance = tolerance }
            (Epsilon eps) = defEps
            (dist, zs) =
                loop builder sp 6 (Bearing . MkQuantity $ F.pi eps) Nothing Nothing xs

loop :: GraphBuilder
     -> SampleParams
     -> Int
     -> Bearing
     -> Maybe PathCost
     -> Maybe [ZonePoint]
     -> [Zone]
     -> (Maybe PathCost, [ZonePoint])
loop _ _ 0 _ d zs _ =
    case zs of
      Nothing -> (Nothing, [])
      Just zs' -> (d, zs')

loop builder sp n br@(Bearing (MkQuantity b)) _ zs xs =
    loop builder sp (n - 1) (Bearing . MkQuantity $ b * (3 % 4)) dist (Just zs') xs
    where
        gr :: Gr ZonePoint PathCost
        gr = builder sp br zs xs

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

buildGraph :: NodeConnector
           -> SampleParams
           -> Bearing
           -> Maybe [ZonePoint]
           -> [Zone]
           -> Gr ZonePoint PathCost
buildGraph f sp b zs xs =
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
        edges' = zipWith f iNodes (tail iNodes)

        flatEdges :: [LEdge PathCost]
        flatEdges = concat edges'

        flatNodes :: [(Node, ZonePoint)]
        flatNodes = concat iNodes
