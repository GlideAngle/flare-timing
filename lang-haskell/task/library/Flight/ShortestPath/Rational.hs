{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.ShortestPath.Rational () where

import Prelude hiding (span)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Control.Arrow (first)

import Flight.Zone (Zone(..), center)
import Flight.Zone.Cylinder (SampleParams(..), ZonePoint(..), CircumSample)
import Flight.Units ()
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng)
import Flight.Geodesy.Solution (Trig, GeoZones(..), GeodesySolutions(..))
import Flight.Geodesy.Rational ()
import Flight.ShortestPath.Cost
    ( GraphBuilder, NodeConnector, DistancePointToPoint, PathCost(..)
    , Zs(..), AngleCut(..), CostSegment
    )
import Flight.ShortestPath.Graph (pad, unpad, dedup, loop, buildGraph, connectNodes)
import Flight.ShortestPath (GeoPath(..))

instance (Real a, Fractional a) => GeoPath Rational a where
    shortestPath
        :: (Trig Rational a, Real a, Fractional a)
        => Earth Rational
        -> CostSegment Rational
        -> CircumSample Rational
        -> AngleCut Rational
        -> SampleParams Rational
        -> [Zone Rational]
        -> Zs (PathDistance Rational)
    shortestPath _ _ _ _ _ [] = Z0
    shortestPath _ _ _ _ _ [_] = Z1
    shortestPath e cseg cs angleCut sp xs =
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
            connector :: NodeConnector _
            connector = connectNodes cseg

            builder :: GraphBuilder _
            builder = buildGraph connector

            span :: Trig Rational a => SpanLatLng Rational
            span = arcLength @_ @Rational e

            dpp :: Trig Rational a => DistancePointToPoint Rational
            dpp = const $ pathDistance @_ @Rational e

            zd =
                distance
                    e
                    span
                    dpp
                    cs
                    builder
                    angleCut
                    xs

            distanceUnchecked span distancePointToPoint cs builder cut xs =
                first Zs $
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

                    f = loop builder cs sp cut Nothing Nothing
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

            distance _ _ _ _ _ _ [] = (Z0, [])
            distance _ _ _ _ _ _ [_] = (Z1, [])

            -- NOTE: Drop the separation requirement when working out the distance from
            -- point to point tagging one intervening zone as this is used in interpolating
            -- the exact tagging point and time between fixes.
            distance _ span distancePointToPoint cs builder cut xs@[Point _, _, Point _] =
                distanceUnchecked span distancePointToPoint cs builder cut xs

            distance e span distancePointToPoint cs builder cut xs
                -- NOTE: Allow duplicates as some tasks are set that way but
                -- remove them before working out the shortest path. I used to
                -- require that zones be separated but that seems to have been
                -- overly cautious. It was returning ZxNotSeparated in that
                -- case.
                | ys <- dedup xs
                , not $ separatedZones @Rational @Rational e ys =
                    distanceUnchecked span distancePointToPoint cs builder cut ys

                | otherwise =
                    distanceUnchecked span distancePointToPoint cs builder cut xs
