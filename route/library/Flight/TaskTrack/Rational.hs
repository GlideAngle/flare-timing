{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.TaskTrack.Rational (taskTracks) where

import Prelude hiding (span)
import qualified Data.Number.FixedFunctions as F
import Data.Either (partitionEithers)
import Data.List (nub)
import Data.UnitsOfMeasure ((/:), u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (LatLng(..))
import Flight.LatLng.Raw (RawLatLng(..))
import Flight.LatLng.Rational (Epsilon(..), defEps)
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng, toKm)
import Flight.Zone (Zone(..), Bearing(..), center)
import Flight.Zone.Path (distancePointToPoint, costSegment)
import Flight.Zone.Raw (RawZone(..))
import Flight.Zone.Cylinder (CircumSample)
import Flight.Sphere.Cylinder.Rational (circumSample)
import Flight.Sphere.PointToPoint.Rational (distanceHaversine)
import Flight.Flat (zoneToProjectedEastNorth)
import Flight.Flat.Projected.Rational (costEastNorth)
import Flight.Route
    ( TaskDistanceMeasure(..)
    , TaskTrack(..)
    , TrackLine(..)
    , ProjectedTrackLine(..)
    , PlanarTrackLine(..)
    )
import Flight.TaskTrack.Internal
    ( mm30
    , roundEastNorth
    , fromUTMRefEastNorth
    , fromUTMRefZone
    , legDistances
    , addTaskDistance
    , convertLatLng
    , toPoint
    , toCylinder
    )
import Flight.Task (Zs(..), CostSegment, AngleCut(..), fromZs, distanceEdgeToEdge)

taskTracks :: Bool
           -> (Int -> Bool)
           -> TaskDistanceMeasure
           -> [[RawZone]] -- ^ Zones of each task.
           -> [Maybe TaskTrack]
taskTracks excludeWaypoints b tdm =
    zipWith
        (\ i t -> if b i then Just $ taskTrack excludeWaypoints tdm t else Nothing)
        [1 .. ]

taskTrack :: Bool
          -> TaskDistanceMeasure
          -> [RawZone] -- ^ A single task is a sequence of control zones.
          -> TaskTrack
taskTrack excludeWaypoints tdm zsRaw =
    case tdm of
        TaskDistanceByAllMethods ->
            TaskTrack
                { ellipsoidPointToPoint = Just pointTrackline
                , ellipsoidEdgeToEdge = edgeTrackline
                , sphericalPointToPoint = Just pointTrackline
                , sphericalEdgeToEdge = edgeTrackline
                , projection = projTrackline
                }
        TaskDistanceByPoints ->
            TaskTrack
                { ellipsoidPointToPoint = Just pointTrackline
                , ellipsoidEdgeToEdge = Nothing
                , sphericalPointToPoint = Just pointTrackline
                , sphericalEdgeToEdge = Nothing
                , projection = Nothing
                }
        TaskDistanceByEdges ->
            TaskTrack
                { ellipsoidPointToPoint = Nothing
                , ellipsoidEdgeToEdge = edgeTrackline
                , sphericalPointToPoint = Nothing
                , sphericalEdgeToEdge = edgeTrackline
                , projection = Nothing
                }
        TaskDistanceByProjection ->
            TaskTrack
                { ellipsoidPointToPoint = Nothing
                , ellipsoidEdgeToEdge = Nothing
                , sphericalPointToPoint = Nothing
                , sphericalEdgeToEdge = Nothing
                , projection = projTrackline
                }
    where
        zs :: [Zone Rational]
        zs = toCylinder <$> zsRaw

        pointTrackline = goByPoint excludeWaypoints zs

        edgeTrackline =
            fromZs
            $ goByEdge excludeWaypoints
            <$> distanceEdgeToEdge' (costSegment span) zs

        projTrackline = goByProj excludeWaypoints zs

-- NOTE: The projected distance is worked out from easting and northing, in the
-- projected plane but he distance for each leg is measured on the sphere.
goByProj :: Bool -> [Zone Rational] -> Maybe ProjectedTrackLine
goByProj excludeWaypoints zs = do
    dEE <- fromZs $ distanceEdgeToEdge' costEastNorth zs

    let projected = goByEdge excludeWaypoints dEE
    let ps = toPoint <$> waypoints projected
    let (_, es) = partitionEithers $ zoneToProjectedEastNorth <$> ps 

    -- NOTE: Workout the distance for each leg projected.
    let legs'' :: [Zs (TaskDistance Rational)] =
            zipWith
                (\ a b ->
                    edgesSum
                    <$> distanceEdgeToEdge' costEastNorth [a, b])
                ps
                (tail ps)

    legs' :: [TaskDistance Rational] <- sequence $ fromZs <$> legs''

    let spherical =
            projected
                { distance =
                    toKm . edgesSum <$> distancePointToPoint span $ ps
                } :: TrackLine

    let planar =
            PlanarTrackLine
                { distance = distance (projected :: TrackLine)
                , mappedZones =
                    let us = fromUTMRefZone <$> es
                        us' = nub us
                    in if length us' == 1 then us' else us
                , mappedPoints =
                    -- NOTE: Round to millimetres when easting and
                    -- northing are in units of metres.
                    roundEastNorth 3 . fromUTMRefEastNorth <$> es
                , legs = toKm <$> legs'
                , legsSum = toKm <$> scanl1 addTaskDistance legs'
                } :: PlanarTrackLine

    return
        ProjectedTrackLine
            { planar = planar
            , spherical = spherical
            , ellipsoid = spherical
            }

goByPoint :: Bool -> [Zone Rational] -> TrackLine
goByPoint excludeWaypoints zs =
    TrackLine
        { distance = toKm d
        , waypoints = if excludeWaypoints then [] else xs
        , legs = toKm <$> ds
        , legsSum = toKm <$> dsSum
        }
    where
        d :: TaskDistance Rational
        d = edgesSum $ distancePointToPoint span zs

        -- NOTE: Concentric zones of different radii can be defined that
        -- share the same center. Remove duplicate edgesSum.
        edgesSum' :: [LatLng Rational [u| rad |]]
        edgesSum' = nub $ center <$> zs

        xs :: [RawLatLng]
        xs = convertLatLng <$> edgesSum'

        ds :: [TaskDistance Rational]
        ds =
            legDistances
                distancePointToPoint
                span
                (Point <$> edgesSum' :: [Zone Rational])

        dsSum :: [TaskDistance Rational]
        dsSum = scanl1 addTaskDistance ds

goByEdge :: Bool -> PathDistance Rational -> TrackLine
goByEdge excludeWaypoints ed =
    TrackLine
        { distance = toKm d
        , waypoints = if excludeWaypoints then [] else xs
        , legs = toKm <$> ds 
        , legsSum = toKm <$> dsSum
        }
    where
        d :: TaskDistance Rational
        d = edgesSum ed

        -- NOTE: The graph of points created for determining the shortest
        -- path can have duplicate points, so the shortest path too can have
        -- duplicate points. Remove these duplicates.
        --
        -- I found that by decreasing defEps, the default epsilon, used for
        -- rational math from 1/10^9 to 1/10^12 these duplicates stopped
        -- occuring.
        vertices' :: [LatLng Rational [u| rad |]]
        vertices' = nub $ vertices ed

        xs :: [RawLatLng]
        xs = convertLatLng <$> vertices'

        ds :: [TaskDistance Rational]
        ds =
            legDistances
                distancePointToPoint
                span
                (Point <$> vertices' :: [Zone Rational])

        dsSum :: [TaskDistance Rational]
        dsSum = scanl1 addTaskDistance ds

distanceEdgeToEdge' :: CostSegment Rational
                    -> [Zone Rational]
                    -> Zs (PathDistance Rational)
distanceEdgeToEdge' segCost = 
    distanceEdgeToEdge span distancePointToPoint segCost cs cut mm30

cs :: CircumSample Rational
cs = circumSample

span :: SpanLatLng Rational
span = distanceHaversine defEps

cut :: AngleCut Rational
cut =
    AngleCut
        { sweep =
            let (Epsilon eps) = defEps
            in Bearing . MkQuantity $ F.pi eps
        , nextSweep = nextCut
        }

nextCut :: AngleCut Rational -> AngleCut Rational
nextCut x@AngleCut{sweep} =
    let (Bearing b) = sweep in x{sweep = Bearing $ b /: 2}
