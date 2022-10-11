{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.TaskTrack.Rational (taskTracks, geoTrack) where

import Data.Ratio ((%))
import qualified Data.Number.FixedFunctions as F
import Data.Either (partitionEithers)
import Data.List (nub)
import Data.UnitsOfMeasure ((/:), u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (LatLng(..))
import Flight.LatLng.RawLatLng (RawLatLng(..))
import Flight.LatLng.Rational (Epsilon(..), defEps)
import Flight.Distance (QTaskDistance, PathDistance(..), SpanLatLng)
import Flight.Zone (Zone(..), Bearing(..), ArcSweep(..), center, toCylinder)
import Flight.Zone.Path (distancePointToPoint, costSegment)
import Flight.Zone.Raw (RawZone(..))
import Flight.Zone.Cylinder (SampleParams(..))
import Flight.Earth.Sphere (earthRadius)
import Flight.Earth.Flat (zoneToProjectedEastNorth)
import Flight.Earth.Flat.Rational (costEastNorth)
import Flight.Geodesy (EarthMath(..), EarthModel(..), Projection(UTM))
import Flight.Geodesy.Solution as E (GeodesySolutions(..), GeoZones(..))
import qualified Flight.Geodesy.Rational as E ()
import Flight.Route
    ( TaskDistanceMeasure(..)
    , OptimalRoute(..)
    , TaskTrack(..)
    )
import Flight.TaskTrack.Internal
    ( roundEastNorth
    , fromUTMRefEastNorth
    , fromUTMRefZone
    , legDistances
    , convertLatLng
    , toPoint
    , fromR
    )
import Flight.Task (Zs(..), CostSegment, AngleCut(..), fromZs)
import Flight.ShortestPath (GeoPath(..))
import Flight.ShortestPath.Rational()
import Flight.Route.TrackLine
    ( ToTrackLine(..), GeoLines(..)
    , TrackLine(..), ProjectedTrackLine(..), PlanarTrackLine(..)
    , speedSubset, sumLegs, flipSumLegs
    )
import Flight.Route.Optimal (emptyOptimal)
import Flight.Earth.Ellipsoid (wgs84)
import Flight.Zone.SpeedSection (SpeedSection, sliceZones)

trackLines :: SampleParams Rational -> Bool -> [Zone Rational] -> GeoLines
trackLines sp excludeWaypoints zs =
    GeoLines
        { point = goByPoint excludeWaypoints zs
        , sphere =
            fromZs
            $ toTrackLine spanS excludeWaypoints
            <$> distanceEdgeSphere sp (costSegment spanS) zs
        , ellipse =
            fromZs
            $ toTrackLine spanE excludeWaypoints
            <$> distanceEdgeEllipsoid sp (costSegment spanE) zs
        , projected = goByProj sp excludeWaypoints zs
        }

geoTrack :: SampleParams Rational -> Bool -> [RawLatLng] -> GeoLines
geoTrack sp excludeWaypoints xs =
    let zs = toPoint <$> xs in trackLines sp excludeWaypoints zs

taskTracks
    :: SampleParams Rational
    -> Bool
    -> (Int -> Bool) -- ^ Process the nth task?
    -> TaskDistanceMeasure
    -> [SpeedSection] -- ^ Speed section of each task.
    -> [[RawZone]] -- ^ Zones of each task.
    -> [Maybe TaskTrack]
taskTracks sp excludeWaypoints b tdm =
    zipWith3
        (\ i ss zs -> if b i then Just $ taskTrack sp excludeWaypoints tdm ss zs else Nothing)
        [1 .. ]

taskTrack
    :: SampleParams Rational
    -> Bool
    -> TaskDistanceMeasure
    -> SpeedSection
    -> [RawZone] -- ^ A single task is a sequence of control zones.
    -> TaskTrack
taskTrack sp excludeWaypoints tdm ss zsRaw =
    case tdm of
        TaskDistanceByAllMethods ->
            TaskTrack
                { ellipsoidPointToPoint =
                    OptimalRoute
                        { taskRoute = Just . point $ taskLines
                        , taskRouteSpeedSubset = Nothing
                        , speedRoute = Just . point $ ssLines
                        , stopRoute = Just . point $ stopLines
                        , startRoute = Just . point $ startLines
                        }
                , ellipsoidEdgeToEdge =
                    let x = ellipse taskLines in
                    OptimalRoute
                        { taskRoute = x
                        , taskRouteSpeedSubset = speedSubset ss <$> x
                        , speedRoute = ellipse ssLines
                        , stopRoute = ellipse stopLines
                        , startRoute = ellipse startLines
                        }
                , sphericalPointToPoint =
                    OptimalRoute
                        { taskRoute = Just . point $ taskLines
                        , taskRouteSpeedSubset = Nothing
                        , speedRoute = Just . point $ ssLines
                        , stopRoute = Just . point $ stopLines
                        , startRoute = Just . point $ startLines
                        }
                , sphericalEdgeToEdge =
                    let x = sphere taskLines in
                    OptimalRoute
                        { taskRoute = x
                        , taskRouteSpeedSubset = speedSubset ss <$> x
                        , speedRoute = sphere ssLines
                        , stopRoute = sphere stopLines
                        , startRoute = sphere startLines
                        }
                , projection =
                    OptimalRoute
                        { taskRoute = projected taskLines
                        , taskRouteSpeedSubset = Nothing
                        , speedRoute = projected ssLines
                        , stopRoute = projected stopLines
                        , startRoute = projected startLines
                        }
                }
        TaskDistanceByPoints ->
            TaskTrack
                { ellipsoidPointToPoint =
                    OptimalRoute
                        { taskRoute = Just . point $ taskLines
                        , taskRouteSpeedSubset = Nothing
                        , speedRoute = Just . point $ ssLines
                        , stopRoute = Just . point $ stopLines
                        , startRoute = Just . point $ startLines
                        }
                , ellipsoidEdgeToEdge = emptyOptimal
                , sphericalPointToPoint =
                    OptimalRoute
                        { taskRoute = Just . point $ taskLines
                        , taskRouteSpeedSubset = Nothing
                        , speedRoute = Just . point $ ssLines
                        , stopRoute = Just . point $ stopLines
                        , startRoute = Just . point $ startLines
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
                        , stopRoute = ellipse stopLines
                        , startRoute = ellipse startLines
                        }
                , sphericalPointToPoint = emptyOptimal
                , sphericalEdgeToEdge =
                    let x = sphere taskLines in
                    OptimalRoute
                        { taskRoute = x
                        , taskRouteSpeedSubset = speedSubset ss <$> x
                        , speedRoute = sphere ssLines
                        , stopRoute = sphere stopLines
                        , startRoute = sphere startLines
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
                        , stopRoute = projected stopLines
                        , startRoute = projected startLines
                        }

                }
    where
        zsTask :: [Zone Rational]
        zsTask = toCylinder <$> zsRaw

        zsSpeedSection :: [Zone Rational]
        zsSpeedSection = sliceZones ss zsTask

        zsStart :: [Zone Rational]
        zsStart = sliceZones ((\(start, _) -> (1, start)) <$> ss) zsTask

        zsStop :: [Zone Rational]
        zsStop = sliceZones ((\(_, end) -> (1, end)) <$> ss) zsTask

        taskLines = trackLines sp excludeWaypoints zsTask
        ssLines = trackLines sp excludeWaypoints zsSpeedSection
        stopLines = trackLines sp excludeWaypoints zsStop
        startLines = trackLines sp excludeWaypoints zsStart

-- NOTE: The projected distance is worked out from easting and northing, in the
-- projected plane but he distance for each leg is measured on the sphere.
goByProj :: SampleParams Rational -> Bool -> [Zone Rational] -> Maybe ProjectedTrackLine
goByProj sp excludeWaypoints zs = do
    dEE <- fromZs $ distanceEdgeSphere sp costEastNorth zs

    let projected = toTrackLine spanF excludeWaypoints dEE
    let ps = toPoint <$> waypoints projected
    let (_, es) = partitionEithers $ zoneToProjectedEastNorth <$> ps 

    -- NOTE: Workout the distance for each leg projected.
    let legs'' :: [Zs (QTaskDistance Rational [u| m |])] =
            zipWith
                (\ a b ->
                    edgesSum
                    <$> distanceEdgeSphere sp costEastNorth [a, b])
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
    :: SampleParams Rational
    -> CostSegment Rational
    -> [Zone Rational]
    -> Zs (PathDistance Rational)
distanceEdgeSphere sp segCost =
    shortestPath @Rational @Rational e segCost cs cut sp
    where
        e = (Haversines, EarthAsSphere earthRadius, defEps)
        cs = circumSample @Rational @Rational e

distanceEdgeEllipsoid
    :: SampleParams Rational
    -> CostSegment Rational
    -> [Zone Rational]
    -> Zs (PathDistance Rational)
distanceEdgeEllipsoid sp segCost =
    shortestPath @Rational @Rational e segCost cs cut sp
    where
        e = (Vincenty, EarthAsEllipsoid wgs84, defEps)
        cs = circumSample @_ @Rational e

-- | Span on a flat projected plane.
spanF :: SpanLatLng Rational
spanF = E.arcLength @Rational @Rational (Pythagorus, EarthAsFlat UTM, defEps)

-- | Span on a sphere using haversines.
spanS :: SpanLatLng Rational
spanS = E.arcLength @Rational @Rational (Haversines, EarthAsSphere earthRadius, defEps)

-- | Span on the WGS 84 ellipsoid using Vincenty's solution to the inverse
-- problem.
spanE :: SpanLatLng Rational
spanE = E.arcLength @Rational @Rational (Vincenty, EarthAsEllipsoid wgs84, Epsilon $ 1 % 1000000000000000000)

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
