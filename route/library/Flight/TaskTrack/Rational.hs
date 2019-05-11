{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.TaskTrack.Rational (taskTracks) where

import Data.Ratio ((%))
import qualified Data.Number.FixedFunctions as F
import Data.Either (partitionEithers)
import Data.List (nub)
import Data.UnitsOfMeasure ((/:), u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (AzimuthFwd, LatLng(..))
import Flight.LatLng.Raw (RawLatLng(..))
import Flight.LatLng.Rational (Epsilon(..), defEps)
import Flight.Distance (QTaskDistance, PathDistance(..), SpanLatLng)
import Flight.Zone (Zone(..), Bearing(..), ArcSweep(..), center, toCylinder)
import Flight.Zone.Path (distancePointToPoint, costSegment)
import Flight.Zone.Raw (RawZone(..))
import Flight.Zone.Cylinder (CircumSample)
import Flight.Earth.Flat (zoneToProjectedEastNorth)
import Flight.Earth.Flat.Projected.Rational (costEastNorth)
import Flight.Earth.Sphere.Cylinder.Rational (circumSample)
import qualified Flight.Earth.Sphere.PointToPoint.Rational as S (azimuthFwd)
import qualified Flight.Earth.Ellipsoid.PointToPoint.Rational as E (azimuthFwd)
import Flight.Earth.Sphere.PointToPoint.Rational (distanceHaversine)
import Flight.Earth.Ellipsoid.PointToPoint.Rational (distanceVincenty)
import Flight.Route
    ( TaskDistanceMeasure(..)
    , OptimalRoute(..)
    , TaskTrack(..)
    )
import Flight.TaskTrack.Internal
    ( mm30
    , roundEastNorth
    , fromUTMRefEastNorth
    , fromUTMRefZone
    , legDistances
    , convertLatLng
    , toPoint
    , fromR
    )
import Flight.Task (Zs(..), CostSegment, AngleCut(..), fromZs, distanceEdgeToEdge)
import Flight.Route.TrackLine
    ( ToTrackLine(..), GeoLines(..)
    , TrackLine(..), ProjectedTrackLine(..), PlanarTrackLine(..)
    , speedSubset, sumLegs, flipSumLegs
    )
import Flight.Route.Optimal (emptyOptimal)
import Flight.Earth.Ellipsoid (wgs84)
import Flight.Zone.SpeedSection (SpeedSection, sliceZones)

trackLines :: Bool -> [Zone Rational] -> GeoLines
trackLines excludeWaypoints zs =
    GeoLines
        { point = goByPoint excludeWaypoints zs
        , sphere =
            fromZs
            $ toTrackLine spanS excludeWaypoints
            <$> distanceEdgeSphere (costSegment spanS) zs
        , ellipse =
            fromZs
            $ toTrackLine spanE excludeWaypoints
            <$> distanceEdgeEllipsoid (costSegment spanE) zs
        , projected = goByProj excludeWaypoints zs
        }

taskTracks
    :: Bool
    -> (Int -> Bool) -- ^ Process the nth task?
    -> TaskDistanceMeasure
    -> [SpeedSection] -- ^ Speed section of each task.
    -> [[RawZone]] -- ^ Zones of each task.
    -> [Maybe TaskTrack]
taskTracks excludeWaypoints b tdm =
    zipWith3
        (\ i ss zs -> if b i then Just $ taskTrack excludeWaypoints tdm ss zs else Nothing)
        [1 .. ]

taskTrack
    :: Bool
    -> TaskDistanceMeasure
    -> SpeedSection
    -> [RawZone] -- ^ A single task is a sequence of control zones.
    -> TaskTrack
taskTrack excludeWaypoints tdm ss zsRaw =
    case tdm of
        TaskDistanceByAllMethods ->
            TaskTrack
                { ellipsoidPointToPoint =
                    OptimalRoute
                        { taskRoute = Just . point $ taskLines
                        , taskRouteSpeedSubset = Nothing
                        , speedRoute = Just . point $ ssLines
                        , stopRoute = Just . point $ ssLines
                        }
                , ellipsoidEdgeToEdge =
                    let x = ellipse taskLines in
                    OptimalRoute
                        { taskRoute = x
                        , taskRouteSpeedSubset = speedSubset ss <$> x
                        , speedRoute = ellipse ssLines
                        , stopRoute = ellipse ssLines
                        }
                , sphericalPointToPoint =
                    OptimalRoute
                        { taskRoute = Just . point $ taskLines
                        , taskRouteSpeedSubset = Nothing
                        , speedRoute = Just . point $ ssLines
                        , stopRoute = Just . point $ ssLines
                        }
                , sphericalEdgeToEdge =
                    let x = sphere taskLines in
                    OptimalRoute
                        { taskRoute = x
                        , taskRouteSpeedSubset = speedSubset ss <$> x
                        , speedRoute = sphere ssLines
                        , stopRoute = sphere ssLines
                        }
                , projection =
                    OptimalRoute
                        { taskRoute = projected taskLines
                        , taskRouteSpeedSubset = Nothing
                        , speedRoute = projected ssLines
                        , stopRoute = projected ssLines
                        }
                }
        TaskDistanceByPoints ->
            TaskTrack
                { ellipsoidPointToPoint =
                    OptimalRoute
                        { taskRoute = Just . point $ taskLines
                        , taskRouteSpeedSubset = Nothing
                        , speedRoute = Just . point $ ssLines
                        , stopRoute = Just . point $ ssLines
                        }
                , ellipsoidEdgeToEdge = emptyOptimal
                , sphericalPointToPoint =
                    OptimalRoute
                        { taskRoute = Just . point $ taskLines
                        , taskRouteSpeedSubset = Nothing
                        , speedRoute = Just . point $ ssLines
                        , stopRoute = Just . point $ ssLines
                        }
                , sphericalEdgeToEdge = emptyOptimal
                , projection = emptyOptimal
                }
        TaskDistanceByEdges ->
            TaskTrack
                { ellipsoidPointToPoint = emptyOptimal
                , ellipsoidEdgeToEdge =
                    let x = ellipse taskLines in
                    OptimalRoute
                        { taskRoute = x
                        , taskRouteSpeedSubset = speedSubset ss <$> x
                        , speedRoute = ellipse ssLines
                        , stopRoute = ellipse ssLines
                        }
                , sphericalPointToPoint = emptyOptimal
                , sphericalEdgeToEdge =
                    let x = sphere taskLines in
                    OptimalRoute
                        { taskRoute = x
                        , taskRouteSpeedSubset = speedSubset ss <$> x
                        , speedRoute = sphere ssLines
                        , stopRoute = sphere ssLines
                        }
                , projection = emptyOptimal
                }
        TaskDistanceByProjection ->
            TaskTrack
                { ellipsoidPointToPoint = emptyOptimal
                , ellipsoidEdgeToEdge = emptyOptimal
                , sphericalPointToPoint = emptyOptimal
                , sphericalEdgeToEdge = emptyOptimal
                , projection =
                    OptimalRoute
                        { taskRoute = projected taskLines
                        , taskRouteSpeedSubset = Nothing
                        , speedRoute = projected ssLines
                        , stopRoute = projected ssLines
                        }

                }
    where
        zsTask :: [Zone Rational]
        zsTask = toCylinder <$> zsRaw

        zsSpeedSection :: [Zone Rational]
        zsSpeedSection = sliceZones ss zsTask

        taskLines = trackLines excludeWaypoints zsTask
        ssLines = trackLines excludeWaypoints zsSpeedSection

-- NOTE: The projected distance is worked out from easting and northing, in the
-- projected plane but he distance for each leg is measured on the sphere.
goByProj :: Bool -> [Zone Rational] -> Maybe ProjectedTrackLine
goByProj excludeWaypoints zs = do
    dEE <- fromZs $ distanceEdgeSphere costEastNorth zs

    let projected = toTrackLine spanF excludeWaypoints dEE
    let ps = toPoint <$> waypoints projected
    let (_, es) = partitionEithers $ zoneToProjectedEastNorth <$> ps 

    -- NOTE: Workout the distance for each leg projected.
    let legs'' :: [Zs (QTaskDistance Rational [u| m |])] =
            zipWith
                (\ a b ->
                    edgesSum
                    <$> distanceEdgeSphere costEastNorth [a, b])
                ps
                (tail ps)

    legs' :: [QTaskDistance Rational [u| m |]] <- sequence $ fromZs <$> legs''

    let spherical =
            projected
                { distance =
                    fromR . edgesSum <$> distancePointToPoint spanS $ ps
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
                , legs = fromR <$> legs'
                , legsSum = sumLegs $ fromR <$> legs'
                , flipSum = flipSumLegs $ fromR <$> legs'
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
        { distance = fromR d
        , waypoints = if excludeWaypoints then [] else xs
        , legs = fromR <$> ds
        , legsSum = sumLegs $ fromR <$> ds
        , flipSum = flipSumLegs $ fromR <$> ds
        }
    where
        d :: QTaskDistance Rational [u| m |]
        d = edgesSum $ distancePointToPoint spanS zs

        -- NOTE: Concentric zones of different radii can be defined that
        -- share the same center. Remove duplicate edgesSum.
        edgesSum' :: [LatLng Rational [u| rad |]]
        edgesSum' = nub $ center <$> zs

        xs :: [RawLatLng]
        xs = convertLatLng <$> edgesSum'

        ds :: [QTaskDistance Rational [u| m |]]
        ds =
            legDistances
                distancePointToPoint
                spanS
                (Point <$> edgesSum' :: [Zone Rational])

distanceEdgeSphere
    :: CostSegment Rational
    -> [Zone Rational]
    -> Zs (PathDistance Rational)
distanceEdgeSphere segCost =
    distanceEdgeToEdge azimuthS spanS distancePointToPoint segCost cs cut mm30

distanceEdgeEllipsoid
    :: CostSegment Rational
    -> [Zone Rational]
    -> Zs (PathDistance Rational)
distanceEdgeEllipsoid segCost =
    distanceEdgeToEdge azimuthE spanE distancePointToPoint segCost cs cut mm30

cs :: CircumSample Rational
cs = circumSample

azimuthS :: AzimuthFwd Rational
azimuthS = S.azimuthFwd defEps

azimuthE :: AzimuthFwd Rational
azimuthE = E.azimuthFwd defEps wgs84

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
            in ArcSweep . Bearing . MkQuantity $ 2 * F.pi eps
        , nextSweep = nextCut
        }

nextCut :: AngleCut Rational -> AngleCut Rational
nextCut x@AngleCut{sweep = ArcSweep (Bearing b)} =
    x{sweep = ArcSweep . Bearing $ b /: 2}
