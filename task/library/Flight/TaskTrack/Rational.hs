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
import Data.UnitsOfMeasure ((/:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (LatLng(..))
import Flight.LatLng.Raw (RawLatLng(..))
import Flight.LatLng.Rational (Epsilon(..), defEps)
import Data.Number.RoundingFunctions (dpRound)
import Flight.Zone
    (Zone(..), Bearing(..), center)
import Flight.Zone.Raw (RawZone(..))
import Flight.Cylinder.Edge (CircumSample)
import Flight.Cylinder.Rational (circumSample)
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.EdgeToEdge (distanceEdgeToEdge)
import Flight.Projected.Rational (costEastNorth)
import Flight.Projected.Internal (zoneToProjectedEastNorth)
import Flight.PointToPoint.Segment (SpanLatLng)
import Flight.PointToPoint.Rational
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
        zs :: [Zone Rational]
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

                ps :: [Zone Rational]
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
                    -> PathDistance Rational
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
