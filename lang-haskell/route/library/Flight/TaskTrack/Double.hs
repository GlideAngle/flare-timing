{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.TaskTrack.Double (taskTracks, geoTrack) where

import Data.Either (partitionEithers)
import Data.List (nub)
import Data.UnitsOfMeasure ((/:), u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (LatLng(..))
import Flight.LatLng.Raw (RawLatLng(..))
import Flight.Distance (QTaskDistance, PathDistance(..), SpanLatLng)
import Flight.Zone (Zone(..), Bearing(..), ArcSweep(..), center)
import Flight.Zone.Path (distancePointToPoint, costSegment)
import Flight.Earth.Sphere (earthRadius)
import Flight.Earth.Flat (zoneToProjectedEastNorth)
import Flight.Earth.Flat.Double (costEastNorth)
import Flight.Geodesy (EarthMath(..), EarthModel(..), Projection(UTM))
import Flight.Geodesy.Solution as E (GeodesySolutions(..), GeoZones(..))
import qualified Flight.Geodesy.Double as E ()
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
    )
import Flight.Task (Zs(..), CostSegment, AngleCut(..), fromZs)
import Flight.ShortestPath (GeoPath(..))
import Flight.ShortestPath.Double ()
import Flight.Route.TrackLine
    ( ToTrackLine(..), GeoLines(..)
    , TrackLine(..), ProjectedTrackLine(..), PlanarTrackLine(..)
    , speedSubset, sumLegs, flipSumLegs
    )
import Flight.Route.Optimal (emptyOptimal)
import Flight.Earth.Ellipsoid (wgs84)
import Flight.Zone.SpeedSection (SpeedSection, sliceZones)

trackLines :: Bool -> [Zone Double] -> GeoLines
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

geoTrack :: Bool -> [RawLatLng] -> GeoLines
geoTrack excludeWaypoints xs =
    let zs = toPoint <$> xs in trackLines excludeWaypoints zs

taskTracks
    :: Bool
    -> (Int -> Bool) -- ^ Process the nth task?
    -> TaskDistanceMeasure
    -> [SpeedSection] -- ^ Speed section of each task.
    -> [[Zone Double]] -- ^ Zones of each task.
    -> [Maybe TaskTrack]
taskTracks excludeWaypoints b tdm =
    zipWith3
        (\ i ss zs -> if b i then Just $ taskTrack excludeWaypoints tdm ss zs else Nothing)
        [1 .. ]

taskTrack
    :: Bool
    -> TaskDistanceMeasure
    -> SpeedSection
    -> [Zone Double] -- ^ A single task is a sequence of control zones.
    -> TaskTrack
taskTrack excludeWaypoints tdm ss zsTask =
    case tdm of
        TaskDistanceByAllMethods ->
            TaskTrack
                { ellipsoidPointToPoint =
                    OptimalRoute
                        { taskRoute = Just . point $ taskLines
                        , taskRouteSpeedSubset = Nothing
                        , speedRoute = Just . point $ ssLines
                        , stopRoute = Just . point $ stopLines
                        }
                , ellipsoidEdgeToEdge =
                    let x = ellipse taskLines in
                    OptimalRoute
                        { taskRoute = x
                        , taskRouteSpeedSubset = speedSubset ss <$> x
                        , speedRoute = ellipse ssLines
                        , stopRoute = ellipse stopLines
                        }
                , sphericalPointToPoint =
                    OptimalRoute
                        { taskRoute = Just . point $ taskLines
                        , taskRouteSpeedSubset = Nothing
                        , speedRoute = Just . point $ ssLines
                        , stopRoute = Just . point $ stopLines
                        }
                , sphericalEdgeToEdge =
                    let x = sphere taskLines in
                    OptimalRoute
                        { taskRoute = x
                        , taskRouteSpeedSubset = speedSubset ss <$> x
                        , speedRoute = sphere ssLines
                        , stopRoute = sphere stopLines
                        }
                , projection =
                    OptimalRoute
                        { taskRoute = projected taskLines
                        , taskRouteSpeedSubset = Nothing
                        , speedRoute = projected ssLines
                        , stopRoute = projected stopLines
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
                        }
                , ellipsoidEdgeToEdge = emptyOptimal
                , sphericalPointToPoint =
                    OptimalRoute
                        { taskRoute = Just . point $ taskLines
                        , taskRouteSpeedSubset = Nothing
                        , speedRoute = Just . point $ ssLines
                        , stopRoute = Just . point $ stopLines
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
                        }
                , sphericalPointToPoint = emptyOptimal
                , sphericalEdgeToEdge =
                    let x = sphere taskLines in
                    OptimalRoute
                        { taskRoute = x
                        , taskRouteSpeedSubset = speedSubset ss <$> x
                        , speedRoute = sphere ssLines
                        , stopRoute = sphere stopLines
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
                        }
                }
    where
        zsSpeedSection :: [Zone Double]
        zsSpeedSection = sliceZones ss zsTask

        zsStop :: [Zone Double]
        zsStop = sliceZones ((\(_, end) -> (1, end)) <$> ss) zsTask

        taskLines = trackLines excludeWaypoints zsTask
        ssLines = trackLines excludeWaypoints zsSpeedSection
        stopLines = trackLines excludeWaypoints zsStop

-- TODO: Propagate Maybe result from costEastNorth.
costEN :: (Real a, Fractional a) => Zone a -> Zone a -> PathDistance a
costEN z0 z1 =
    maybe (error "Can't get east north cost.") id
    $ costEastNorth z0 z1

-- NOTE: The projected distance is worked out from easting and northing, in the
-- projected plane but he distance for each leg is measured on the sphere.
goByProj :: Bool -> [Zone Double] -> Maybe ProjectedTrackLine
goByProj excludeWaypoints zs = do
    dEE <- fromZs $ distanceEdgeSphere costEN zs

    let projected = toTrackLine spanF excludeWaypoints dEE
    let ps = toPoint <$> waypoints projected
    let (_, es) = partitionEithers $ zoneToProjectedEastNorth <$> ps

    -- NOTE: Workout the distance for each leg projected.
    let legs'' :: [Zs (QTaskDistance Double [u| m |])] =
            zipWith
                (\ a b ->
                    edgesSum
                    <$> distanceEdgeSphere costEN [a, b])
                ps
                (tail ps)

    legs' :: [QTaskDistance Double [u| m |]] <- sequence $ fromZs <$> legs''

    let spherical =
            projected
                { distance =
                    edgesSum <$> distancePointToPoint spanS $ ps
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
                , legs = legs'
                , legsSum = sumLegs legs'
                , flipSum = flipSumLegs legs'
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
        { distance = d
        , waypoints = if excludeWaypoints then [] else xs
        , legs = ds
        , legsSum = sumLegs ds
        , flipSum = flipSumLegs ds
        }
    where
        d :: QTaskDistance Double [u| m |]
        d = edgesSum $ distancePointToPoint spanS zs

        -- NOTE: Concentric zones of different radii can be defined that
        -- share the same center. Remove duplicate edgesSum.
        edgesSum' :: [LatLng Double [u| rad |]]
        edgesSum' = nub $ center <$> zs

        xs :: [RawLatLng]
        xs = convertLatLng <$> edgesSum'

        ds :: [QTaskDistance Double [u| m |]]
        ds =
            legDistances
                distancePointToPoint
                spanS
                (Point <$> edgesSum' :: [Zone Double])

distanceEdgeSphere
    :: CostSegment Double
    -> [Zone Double]
    -> Zs (PathDistance Double)
distanceEdgeSphere segCost =
    shortestPath @Double @Double e segCost cs cut mm30
    where
        e = (Haversines, EarthAsSphere earthRadius)
        cs = circumSample @Double @Double e

distanceEdgeEllipsoid
    :: CostSegment Double
    -> [Zone Double]
    -> Zs (PathDistance Double)
distanceEdgeEllipsoid segCost =
    shortestPath @Double @Double e segCost cs cut mm30
    where
        e = (Vincenty, EarthAsEllipsoid wgs84)
        cs = circumSample @Double @Double e

-- | Span on a flat projected plane.
spanF :: SpanLatLng Double
spanF = E.arcLength @Double @Double (Pythagorus, EarthAsFlat UTM)

-- | Span on a sphere using haversines.
spanS :: SpanLatLng Double
spanS = E.arcLength @Double @Double (Haversines, EarthAsSphere earthRadius)

-- | Span on the WGS 84 ellipsoid using Vincenty's solution to the inverse
-- problem.
spanE :: SpanLatLng Double
spanE = E.arcLength @Double @Double (Vincenty, EarthAsEllipsoid wgs84)

cut :: AngleCut Double
cut =
    AngleCut
        { sweep = ArcSweep . Bearing . MkQuantity $ 2 * pi
        , nextSweep = nextCut
        }

nextCut :: AngleCut Double -> AngleCut Double
nextCut x@AngleCut{sweep = ArcSweep (Bearing b)} =
    x{sweep = ArcSweep . Bearing $ b /: 2}
