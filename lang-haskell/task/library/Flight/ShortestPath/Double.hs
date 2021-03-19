{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Flight.ShortestPath.Double () where

import Prelude hiding (span)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone (Zone(..), center)
import Flight.Zone.Cylinder (SampleParams(..), ZonePoint(..), CircumSample)
import Flight.Units ()
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng)
import Flight.Geodesy.Solution (Trig, GeoZones(..), GeodesySolutions(..))
import Flight.Geodesy.Double ()
import Flight.ShortestPath.Cost
    ( GraphBuilder, NodeConnector, DistancePointToPoint, PathCost(..)
    , Zs(..), AngleCut(..), CostSegment, OptimalCostedPath
    )
import Flight.ShortestPath.Graph (pad, unpad, dedup, loop, buildGraph, connectNodes)
import Flight.ShortestPath (GeoPath(..), OptimalPath)

instance RealFloat a => GeoPath Double a where
    shortestPath
        :: Trig Double a
        => Earth Double
        -> CostSegment Double
        -> CircumSample Double
        -> AngleCut Double
        -> SampleParams Double
        -> Maybe [ZonePoint Double]
        -> [Zone Double]
        -> Zs (OptimalPath Double)
    shortestPath _ _ _ _ _ _ [] = Z0
    shortestPath _ _ _ _ _ _ [_] = Z1
    shortestPath e cseg cs angleCut sp hints xs =
        case xs of
            [] -> Z0
            [_] -> Z1
            (_ : _) ->
                case zd of
                    Z0 -> Z0
                    Z1 -> Z1
                    ZxNotSeparated -> ZxNotSeparated
                    Zs (zs, (PathCost pcd, ptsCenterLine)) ->
                        let d =
                                PathDistance
                                    { edgesSum = TaskDistance $ MkQuantity pcd
                                    , vertices = ptsCenterLine
                                    }

                        in Zs (zs, d)
        where
            connector :: NodeConnector Double
            connector = connectNodes cseg

            builder :: GraphBuilder Double
            builder = buildGraph connector

            span :: SpanLatLng Double
            span = arcLength e

            dpp :: DistancePointToPoint Double
            dpp = const $ pathDistance e

            zd :: Zs (OptimalCostedPath Double)
            zd =
                distance
                    e
                    span
                    dpp
                    cs
                    builder
                    angleCut
                    hints
                    xs

            distanceUnchecked
                :: SpanLatLng Double
                -> DistancePointToPoint Double
                -> CircumSample Double
                -> GraphBuilder Double
                -> AngleCut Double
                -> Maybe [ZonePoint Double]
                -> [Zone Double]
                -> Zs (OptimalCostedPath Double)
            distanceUnchecked span distancePointToPoint cs builder cut hints xs =
                Zs . (zs',) $
                case dist of
                    Nothing -> (PathCost pointwise, edgesSum')
                    Just d@(PathCost pcd) ->
                        if pcd < pointwise
                            then (d, point <$> zs')
                            else (PathCost pointwise, edgesSum')
                where
                    (TaskDistance (MkQuantity pointwise)) =
                        edgesSum $ distancePointToPoint span xs

                    edgesSum' = center <$> xs

                    f = loop builder cs sp cut Nothing hints
                    g = unpad span distancePointToPoint

                    -- NOTE: I need to add a zone at each end to define the start and
                    -- end for the shortest path. Once the shortest path is found
                    -- I then need to undo the padding.
                    (_, ys) = f $ pad xs
                    pass1@(_, zs) = g ys

                    -- NOTE: I need another pass for when the last zone is a line so that
                    -- I can reuse the penultimate point on the optimal path. This way
                    -- I won't always be selecting the center of the line zone as the last
                    -- point.
                    (dist, zs') =
                        case reverse xs of
                            ((Line Nothing _ _) : _) ->
                                error "Need a line with azimuth or normal set."
                            (xN@(Line (Just _) _ _) : _) ->
                                let zPts = Point . point <$> zs in
                                case (zPts, reverse zPts) of
                                    (v : _, _ : ws@(w : _)) ->
                                        let vs = v : (reverse (w : xN : ws))
                                            (_, ys') = f $ pad vs
                                        in
                                            g . snd . g $ ys'

                                    _ -> pass1
                            _ -> pass1

            distance
                :: Earth Double
                -> SpanLatLng Double
                -> DistancePointToPoint Double
                -> CircumSample Double
                -> GraphBuilder Double
                -> AngleCut Double
                -> Maybe [ZonePoint Double]
                -> [Zone Double]
                -> Zs (OptimalCostedPath Double)
            distance _ _ _ _ _ _ _ [] = Z0
            distance _ _ _ _ _ _ _ [_] = Z1

            -- NOTE: Drop the separation requirement when working out the distance from
            -- point to point tagging one intervening zone as this is used in interpolating
            -- the exact tagging point and time between fixes.
            distance _ span distancePointToPoint cs builder cut hints xs@[Point _, _, Point _] =
                distanceUnchecked span distancePointToPoint cs builder cut hints xs

            distance e span distancePointToPoint cs builder cut hints xs
                -- NOTE: Allow duplicates as some tasks are set that way but
                -- remove them before working out the shortest path. I used to
                -- require that zones be separated but that seems to have been
                -- overly cautious. It was returning ZxNotSeparated in that
                -- case.
                | ys <- dedup xs
                , not $ separatedZones e ys =
                    distanceUnchecked span distancePointToPoint cs builder cut hints ys

                | otherwise =
                    distanceUnchecked span distancePointToPoint cs builder cut hints xs
