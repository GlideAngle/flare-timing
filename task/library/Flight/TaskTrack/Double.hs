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
import Data.UnitsOfMeasure ((/:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (LatLng(..))
import Flight.LatLng.Raw (RawLatLng(..))
import Data.Number.RoundingFunctions (dpRound)
import Flight.Zone
    (Zone(..), Bearing(..), center)
import Flight.Zone.Raw (RawZone(..))
import Flight.Cylinder.Edge (CircumSample)
import Flight.Cylinder.Double (circumSample)
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.EdgeToEdge (distanceEdgeToEdge)
import Flight.Projected.Double (costEastNorth)
import Flight.Projected.Internal (zoneToProjectedEastNorth)
import Flight.PointToPoint.Segment (SpanLatLng)
import Flight.PointToPoint.Double
    (distancePointToPoint, costSegment, distanceHaversine)
import Flight.TaskTrack
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
import Flight.ShortestPath (AngleCut(..), CostSegment)

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
                { pointToPoint = Just pointTrackline
                , edgeToEdge = Just edgeTrackline
                , projection = Just projTrackline
                }
        TaskDistanceByPoints ->
            TaskTrack
                { pointToPoint = Just pointTrackline
                , edgeToEdge = Nothing
                , projection = Nothing
                }
        TaskDistanceByEdges ->
            TaskTrack
                { pointToPoint = Nothing
                , edgeToEdge = Just edgeTrackline
                , projection = Nothing
                }
        TaskDistanceByProjection ->
            TaskTrack
                { pointToPoint = Nothing
                , edgeToEdge = Nothing
                , projection = Just projTrackline
                }
    where
        zs :: [Zone Double]
        zs = toCylinder <$> zsRaw

        pointTrackline = goByPoint excludeWaypoints zs

        edgeTrackline =
            goByEdge
                excludeWaypoints
                (distanceEdgeToEdge' (costSegment span) zs)

        projTrackline =
            ProjectedTrackLine { planar = planar
                               , spherical = spherical
                               }
            where
                -- NOTE: The projected distance is worked out from easting and
                -- northing, in the projected plane but he distance for each leg
                -- is measured on the sphere.
                projected =
                    goByEdge
                        excludeWaypoints
                        (distanceEdgeToEdge' costEastNorth zs)

                ps :: [Zone Double]
                ps = toPoint <$> waypoints projected

                (_, es) = partitionEithers $ zoneToProjectedEastNorth <$> ps 

                -- NOTE: Workout the distance for each leg projected.
                legs' =
                    zipWith
                        (\ a b ->
                            edgesSum
                            $ distanceEdgeToEdge' costEastNorth [a, b])
                        ps
                        (tail ps)

                spherical =
                    projected
                        { distance = toKm . edgesSum . distancePointToPoint span $ ps
                        } :: TrackLine

                planar =
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

-- | Convert to kilometres with mm accuracy.
toKm :: (Real a, Fractional a) => TaskDistance a -> Double
toKm = toKm' (dpRound 6 . toRational)

toKm' :: Fractional a => (a -> Rational) -> TaskDistance a -> Double
toKm' f (TaskDistance d) =
    fromRational $ f dKm
    where 
        MkQuantity dKm = convert d :: Quantity _ [u| km |]

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

goByEdge :: Bool -> PathDistance Double -> TrackLine
goByEdge excludeWaypoints ed =
    TrackLine
        { distance = toKm d
        , waypoints = if excludeWaypoints then [] else xs
        , legs = toKm <$> ds 
        , legsSum = toKm <$> dsSum
        }
    where
        d :: TaskDistance Double
        d = edgesSum ed

        -- NOTE: The graph of points created for determining the shortest
        -- path can have duplicate points, so the shortest path too can have
        -- duplicate points. Remove these duplicates.
        --
        -- I found that by decreasing defEps, the default epsilon, used for
        -- rational math from 1/10^9 to 1/10^12 these duplicates stopped
        -- occuring.
        vertices' :: [LatLng Double [u| rad |]]
        vertices' = nub $ vertices ed

        xs :: [RawLatLng]
        xs = convertLatLng <$> vertices'

        ds :: [TaskDistance Double]
        ds =
            legDistances
                distancePointToPoint
                span
                (Point <$> vertices' :: [Zone Double])

        dsSum :: [TaskDistance Double]
        dsSum = scanl1 addTaskDistance ds

distanceEdgeToEdge' :: CostSegment Double
                    -> [Zone Double]
                    -> PathDistance Double
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
