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
import Data.Graph.Inductive.Graph (Node, LEdge)
import qualified UTMRef as HC (UTMRef(..), toUTMRef)
import qualified LatLng as HC (mkLatLng)
import qualified Datum as HC (wgs84Datum)

import Flight.Zone (Zone(..))
import Flight.PointToPoint (TaskDistance(..))
import Flight.CylinderEdge (Tolerance, ZonePoint(..))
import Flight.Units ()
import Flight.LatLng (LatLng(..), Lat(..), Lng(..), radToDegLL, defEps)
import Flight.ShortestPath
    ( DistancePath(..)
    , EdgeDistance(..)
    , PathCost(..)
    , NodeConnector
    , shortestPath
    , buildGraph
    )

distanceProjected :: DistancePath
                  -> Tolerance
                  -> [Zone]
                  -> EdgeDistance
distanceProjected = shortestPath $ buildGraph connectNodes


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
                    projectedPythagorean (Point $ point x) (Point $ point y)

zoneToProjectedLatLng :: Zone -> Either String HC.UTMRef

zoneToProjectedLatLng (Point x) = do
    xLL <- runIdentity . runExceptT $ HC.mkLatLng xLat' xLng' 0 HC.wgs84Datum
    runIdentity . runExceptT $ HC.toUTMRef xLL
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
