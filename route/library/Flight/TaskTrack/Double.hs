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

module Flight.TaskTrack.Double (taskTracks) where

import Prelude hiding (span)
import Data.Either (partitionEithers)
import Data.List (nub)
import Data.UnitsOfMeasure ((/:), u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (LatLng(..))
import Flight.LatLng.Raw (RawLatLng(..))
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng, toKm)
import Flight.Zone (Zone(..), Bearing(..), center)
import Flight.Zone.Path (distancePointToPoint, costSegment)
import Flight.Zone.Raw (RawZone(..))
import Flight.Zone.Cylinder (CircumSample)
import Flight.Sphere.Cylinder.Double (circumSample)
import Flight.Sphere.PointToPoint.Double (distanceHaversine)
import Flight.Flat (zoneToProjectedEastNorth)
import Flight.Flat.Projected.Double (costEastNorth)
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
import Flight.Task (Zs(..), CostSegment, AngleCut(..) , fromZs, distanceEdgeToEdge)
import Flight.Route.TrackLine (ToTrackLine(..))

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
        zs :: [Zone Double]
        zs = toCylinder <$> zsRaw

        pointTrackline = goByPoint excludeWaypoints zs

        edgeTrackline =
            fromZs
            $ toTrackLine excludeWaypoints
            <$> distanceEdgeToEdge' (costSegment span) zs

        projTrackline = goByProj excludeWaypoints zs

-- NOTE: The projected distance is worked out from easting and northing, in the
-- projected plane but he distance for each leg is measured on the sphere.
goByProj :: Bool -> [Zone Double] -> Maybe ProjectedTrackLine
goByProj excludeWaypoints zs = do
    dEE <- fromZs $ distanceEdgeToEdge' costEastNorth zs

    let projected = toTrackLine excludeWaypoints dEE
    let ps = toPoint <$> waypoints projected
    let (_, es) = partitionEithers $ zoneToProjectedEastNorth <$> ps

    -- NOTE: Workout the distance for each leg projected.
    let legs'' :: [Zs (TaskDistance Double)] =
            zipWith
                (\ a b ->
                    edgesSum
                    <$> distanceEdgeToEdge' costEastNorth [a, b])
                ps
                (tail ps)

    legs' :: [TaskDistance Double] <- sequence $ fromZs <$> legs''

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

goByPoint :: Bool -> [Zone Double] -> TrackLine
goByPoint excludeWaypoints zs =
    TrackLine
        { distance = toKm d
        , waypoints = if excludeWaypoints then [] else xs
        , legs = toKm <$> ds
        , legsSum = toKm <$> dsSum
        }
    where
        d :: TaskDistance Double
        d = edgesSum $ distancePointToPoint span zs

        -- NOTE: Concentric zones of different radii can be defined that
        -- share the same center. Remove duplicate edgesSum.
        edgesSum' :: [LatLng Double [u| rad |]]
        edgesSum' = nub $ center <$> zs

        xs :: [RawLatLng]
        xs = convertLatLng <$> edgesSum'

        ds :: [TaskDistance Double]
        ds =
            legDistances
                distancePointToPoint
                span
                (Point <$> edgesSum' :: [Zone Double])

        dsSum :: [TaskDistance Double]
        dsSum = scanl1 addTaskDistance ds

distanceEdgeToEdge' :: CostSegment Double
                    -> [Zone Double]
                    -> Zs (PathDistance Double)
distanceEdgeToEdge' segCost = 
    distanceEdgeToEdge span distancePointToPoint segCost cs cut mm30

cs :: CircumSample Double
cs = circumSample

span :: SpanLatLng Double
span = distanceHaversine

cut :: AngleCut Double
cut =
    AngleCut
        { sweep = Bearing $ MkQuantity pi
        , nextSweep = nextCut
        }

nextCut :: AngleCut Double -> AngleCut Double
nextCut x@AngleCut{sweep} =
    let (Bearing b) = sweep in x{sweep = Bearing $ b /: 2}
