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
import Data.Ratio ((%))
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
import Flight.Earth.Flat (zoneToProjectedEastNorth)
import Flight.Earth.Flat.Projected.Rational (costEastNorth)
import Flight.Earth.Sphere.Cylinder.Rational (circumSample)
import Flight.Earth.Sphere.PointToPoint.Rational (distanceHaversine)
import Flight.Earth.Ellipsoid.PointToPoint.Rational (distanceVincenty)
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
import Flight.Route.TrackLine (ToTrackLine(..))
import Flight.Earth.Ellipsoid (wgs84)

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
                , ellipsoidEdgeToEdge = ellipsoidTrackline
                , sphericalPointToPoint = Just pointTrackline
                , sphericalEdgeToEdge = sphereTrackline
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
                , ellipsoidEdgeToEdge = ellipsoidTrackline
                , sphericalPointToPoint = Nothing
                , sphericalEdgeToEdge = sphereTrackline
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

        sphereTrackline =
            fromZs
            $ toTrackLine spanS excludeWaypoints
            <$> distanceEdgeSphere (costSegment spanS) zs

        ellipsoidTrackline =
            fromZs
            $ toTrackLine spanE excludeWaypoints
            <$> distanceEdgeEllipsoid (costSegment spanE) zs

        projTrackline = goByProj excludeWaypoints zs

-- NOTE: The projected distance is worked out from easting and northing, in the
-- projected plane but he distance for each leg is measured on the sphere.
goByProj :: Bool -> [Zone Rational] -> Maybe ProjectedTrackLine
goByProj excludeWaypoints zs = do
    dEE <- fromZs $ distanceEdgeSphere costEastNorth zs

    let projected = toTrackLine spanF excludeWaypoints dEE
    let ps = toPoint <$> waypoints projected
    let (_, es) = partitionEithers $ zoneToProjectedEastNorth <$> ps 

    -- NOTE: Workout the distance for each leg projected.
    let legs'' :: [Zs (TaskDistance Rational)] =
            zipWith
                (\ a b ->
                    edgesSum
                    <$> distanceEdgeSphere costEastNorth [a, b])
                ps
                (tail ps)

    legs' :: [TaskDistance Rational] <- sequence $ fromZs <$> legs''

    let spherical =
            projected
                { distance =
                    toKm . edgesSum <$> distancePointToPoint spanS $ ps
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
        d = edgesSum $ distancePointToPoint spanS zs

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
                spanS
                (Point <$> edgesSum' :: [Zone Rational])

        dsSum :: [TaskDistance Rational]
        dsSum = scanl1 addTaskDistance ds

distanceEdgeSphere
    :: CostSegment Rational
    -> [Zone Rational]
    -> Zs (PathDistance Rational)
distanceEdgeSphere segCost = 
    distanceEdgeToEdge spanS distancePointToPoint segCost cs cut mm30

distanceEdgeEllipsoid
    :: CostSegment Rational
    -> [Zone Rational]
    -> Zs (PathDistance Rational)
distanceEdgeEllipsoid segCost = 
    distanceEdgeToEdge spanE distancePointToPoint segCost cs cut mm30

cs :: CircumSample Rational
cs = circumSample

-- | Span on a flat projected plane.
spanF :: SpanLatLng Rational
spanF = distanceHaversine defEps

-- | Span on a sphere using haversines.
spanS :: SpanLatLng Rational
spanS = distanceHaversine defEps

-- | Span on the WGS 84 ellipsoid using Vincenty's solution to the inverse
-- problem.
spanE :: SpanLatLng Rational
spanE =
    distanceVincenty e wgs84
    where
        e = Epsilon $ 1 % 1000000000000000000

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
