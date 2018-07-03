{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.TaskTrack.Double (taskTracks) where

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
import Flight.Earth.Flat (zoneToProjectedEastNorth)
import Flight.Earth.Flat.Projected.Double (costEastNorth)
import Flight.Earth.Sphere.Cylinder.Double (circumSample)
import Flight.Earth.Sphere.PointToPoint.Double (distanceHaversine)
import Flight.Earth.Ellipsoid.PointToPoint.Double (distanceVincenty)
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
        zs :: [Zone Double]
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
goByProj :: Bool -> [Zone Double] -> Maybe ProjectedTrackLine
goByProj excludeWaypoints zs = do
    dEE <- fromZs $ distanceEdgeSphere costEastNorth zs

    let projected = toTrackLine spanF excludeWaypoints dEE
    let ps = toPoint <$> waypoints projected
    let (_, es) = partitionEithers $ zoneToProjectedEastNorth <$> ps

    -- NOTE: Workout the distance for each leg projected.
    let legs'' :: [Zs (TaskDistance Double)] =
            zipWith
                (\ a b ->
                    edgesSum
                    <$> distanceEdgeSphere costEastNorth [a, b])
                ps
                (tail ps)

    legs' :: [TaskDistance Double] <- sequence $ fromZs <$> legs''

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
        d = edgesSum $ distancePointToPoint spanS zs

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
                spanS
                (Point <$> edgesSum' :: [Zone Double])

        dsSum :: [TaskDistance Double]
        dsSum = scanl1 addTaskDistance ds

distanceEdgeSphere
    :: CostSegment Double
    -> [Zone Double]
    -> Zs (PathDistance Double)
distanceEdgeSphere segCost = 
    distanceEdgeToEdge spanS distancePointToPoint segCost cs cut mm30

distanceEdgeEllipsoid
    :: CostSegment Double
    -> [Zone Double]
    -> Zs (PathDistance Double)
distanceEdgeEllipsoid segCost = 
    distanceEdgeToEdge spanE distancePointToPoint segCost cs cut mm30

cs :: CircumSample Double
cs = circumSample

-- | Span on a flat projected plane.
spanF :: SpanLatLng Double
spanF = distanceHaversine

-- | Span on a sphere using haversines.
spanS :: SpanLatLng Double
spanS = distanceHaversine

-- | Span on the WGS 84 ellipsoid using Vincenty's solution to the inverse
-- problem.
spanE :: SpanLatLng Double
spanE = distanceVincenty wgs84

cut :: AngleCut Double
cut =
    AngleCut
        { sweep = Bearing $ MkQuantity pi
        , nextSweep = nextCut
        }

nextCut :: AngleCut Double -> AngleCut Double
nextCut x@AngleCut{sweep} =
    let (Bearing b) = sweep in x{sweep = Bearing $ b /: 2}
