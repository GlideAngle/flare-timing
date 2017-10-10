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

module Flight.Projected (distanceProjected) where

import Data.Functor.Identity (runIdentity)
import Control.Monad.Except (runExceptT)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Control.Arrow (first)
import Data.Graph.Inductive.Graph (Graph(..), Node, LEdge, mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified UTMRef as HC (UTMRef(..), toUTMRef)
import qualified LatLng as HC (mkLatLng)
import qualified Datum as HC (wgs84Datum)

import Flight.Zone (Zone(..), Bearing(..))
import Flight.PointToPoint (TaskDistance(..))
import Flight.CylinderEdge
    ( Tolerance
    , SampleParams(..)
    , ZonePoint(..)
    , sample
    )
import Flight.Units ()
import Flight.LatLng (LatLng(..), Lat(..), Lng(..), radToDegLL, defEps)
import Flight.ShortestPath
    (DistancePath(..), EdgeDistance(..), PathCost(..), shortestPath)

distanceProjected :: DistancePath
                  -> Tolerance
                  -> [Zone]
                  -> EdgeDistance
distanceProjected = shortestPath buildGraph

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
                    projectedPythagorean (Point $ point x) (Point $ point y)

zoneToProjectedLatLng :: Zone -> Either String HC.UTMRef

zoneToProjectedLatLng (Point x) = do
    xLL <- runIdentity . runExceptT $ HC.mkLatLng xLat' xLng' 0 HC.wgs84Datum
    xUTM <- runIdentity . runExceptT $ HC.toUTMRef xLL
    return xUTM
    where
        (LatLng (Lat (MkQuantity xLat), Lng (MkQuantity xLng))) =
            radToDegLL defEps x

        xLat' :: Double
        xLat' = fromRational xLat :: Double

        xLng' :: Double
        xLng' = fromRational xLng :: Double

zoneToProjectedLatLng _ = Left ""

tooFar :: TaskDistance
tooFar = TaskDistance [u| 20000000 m |]

projectedPythagorean :: Zone -> Zone -> TaskDistance

projectedPythagorean x@(Point _) y@(Point _) =
    case (zoneToProjectedLatLng x, zoneToProjectedLatLng y) of
        (Right xLL, Right yLL) ->
            TaskDistance dm
            where
                d :: Double
                d = pythagorean xLL yLL

                dm :: Quantity Rational [u| m |]
                dm = MkQuantity $ toRational d

        _ -> tooFar

projectedPythagorean _ _ = tooFar

pythagorean :: HC.UTMRef -> HC.UTMRef -> Double
pythagorean x y =
    sqrt $ dN * dN + dE * dE
    where
        dN = yN - xN
        dE = yE - xE

        xN = HC.northing x
        yN = HC.northing y

        xE = HC.easting x
        yE = HC.easting y
