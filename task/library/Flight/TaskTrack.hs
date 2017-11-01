{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Flight.TaskTrack
    ( TaskDistanceMeasure(..)
    , TaskRoutes(..)
    , TaskTrack(..)
    , TrackLine(..)
    , taskTracks
    ) where

import Prelude hiding (span)
import Data.Ratio ((%))
import Data.Either (partitionEithers)
import Data.List (nub)
import Data.UnitsOfMeasure ((+:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified UTMRef as HC (UTMRef(..))

import Flight.Units ()
import Flight.LatLng (Lat(..), Lng(..), LatLng(..), defEps)
import Flight.LatLng.Raw (RawLat(..), RawLng(..), RawLatLng(..))
import Data.Number.RoundingFunctions (dpRound)
import Flight.Zone (Zone(..), Radius(..), center, fromRationalRadius)
import Flight.Zone.Raw (RawZone(..))
import Flight.CylinderEdge (Tolerance(..))
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.EdgeToEdge (distanceEdgeToEdge)
import Flight.Projected (costEastNorth, zoneToProjectedEastNorth)
import Flight.PointToPoint
    (SpanLatLng, distancePointToPoint, costSegment, distanceHaversine)

-- | The way to measure the task distance.
data TaskDistanceMeasure
    = TaskDistanceByAllMethods
    | TaskDistanceByPoints
    | TaskDistanceByEdges
    | TaskDistanceByProjection
    deriving (Eq, Show)

mm30 :: Tolerance Rational
mm30 = Tolerance $ 30 % 1000

newtype TaskRoutes =
    TaskRoutes { taskRoutes :: [Maybe TaskTrack] }
    deriving (Show, Generic)

instance ToJSON TaskRoutes
instance FromJSON TaskRoutes

data TaskTrack =
    TaskTrack { pointToPoint :: Maybe TrackLine
              , edgeToEdge :: Maybe TrackLine
              , projection :: Maybe ProjectedTrackLine
              }
    deriving (Show, Generic)

instance ToJSON TaskTrack
instance FromJSON TaskTrack

data ProjectedTrackLine =
    ProjectedTrackLine { planar :: PlanarTrackLine
                       , spherical :: TrackLine
                       }
    deriving (Show, Generic)

instance ToJSON ProjectedTrackLine
instance FromJSON ProjectedTrackLine

data TrackLine =
    TrackLine { distance :: Double
              , waypoints :: [RawLatLng]
              , legs :: [Double]
              , legsSum :: [Double]
              }
    deriving (Show, Generic)

instance ToJSON TrackLine
instance FromJSON TrackLine

data PlanarTrackLine =
    PlanarTrackLine { distance :: Double
                    , mappedZones :: [UtmZone]
                    , mappedPoints :: [EastingNorthing]
                    , legs :: [Double]
                    , legsSum :: [Double]
                    }
    deriving (Show, Generic)

instance ToJSON PlanarTrackLine
instance FromJSON PlanarTrackLine

data EastingNorthing =
    EastingNorthing { easting :: Double
                    , northing :: Double
                    }
    deriving (Show, Generic)

instance ToJSON EastingNorthing
instance FromJSON EastingNorthing

data UtmZone =
    UtmZone { latZone :: Char
            , lngZone :: Int
            }
    deriving (Show, Eq, Generic)

instance ToJSON UtmZone
instance FromJSON UtmZone

taskTracks :: Bool
           -> (Int -> Bool)
           -> TaskDistanceMeasure
           -> [[RawZone]] -- ^ Zones of each task.
           -> [Maybe TaskTrack]
taskTracks excludeWaypoints b tdm tasks =
    zipWith
        (\ i t -> if b i then Just $ taskTrack excludeWaypoints tdm t else Nothing)
        [1 .. ]
        tasks

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
                (distanceEdgeToEdge span (costSegment span) mm30 zs)

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
                        (distanceEdgeToEdge span costEastNorth mm30 zs)

                ps :: [Zone Rational]
                ps = toPoint <$> (waypoints projected)

                (_, es) = partitionEithers $ zoneToProjectedEastNorth <$> ps 

                -- NOTE: Workout the distance for each leg projected.
                legs' =
                    zipWith
                        (\ a b ->
                            edgesSum
                            $ distanceEdgeToEdge span costEastNorth mm30 [a, b])
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
toKm :: TaskDistance -> Double
toKm = toKm' (dpRound 6)

toKm' :: (Rational -> Rational) -> TaskDistance -> Double
toKm' f (TaskDistance d) =
    fromRational $ f dKm
    where 
        MkQuantity dKm = convert d :: Quantity Rational [u| km |]

roundEastNorth :: Integer -> EastingNorthing -> EastingNorthing
roundEastNorth dp EastingNorthing{..} =
    EastingNorthing
        { easting = f easting
        , northing = f northing
        }
    where
        f x = fromRational $ (dpRound dp) $ toRational x

fromUTMRefEastNorth :: HC.UTMRef -> EastingNorthing
fromUTMRefEastNorth HC.UTMRef{..} =
    EastingNorthing
        { easting = easting
        , northing = northing
        }

fromUTMRefZone :: HC.UTMRef -> UtmZone
fromUTMRefZone HC.UTMRef{..} =
    UtmZone
        { latZone = latZone
        , lngZone = lngZone
        }

legDistances :: Real a => [Zone a] -> [TaskDistance]
legDistances xs =
    zipWith
        (\x y -> edgesSum $ distancePointToPoint span [x, y])
        xs
        (tail xs)

goByPoint :: Real a => Bool -> [Zone a] -> TrackLine
goByPoint excludeWaypoints zs =
    TrackLine
        { distance = toKm d
        , waypoints = if excludeWaypoints then [] else xs
        , legs = toKm <$> ds
        , legsSum = toKm <$> dsSum
        }
    where
        d :: TaskDistance
        d = edgesSum $ distancePointToPoint span zs

        -- NOTE: Concentric zones of different radii can be defined that
        -- share the same center. Remove duplicate edgesSum.
        edgesSum' :: [LatLng [u| rad |]]
        edgesSum' = nub $ center <$> zs

        xs :: [RawLatLng]
        xs = convertLatLng <$> edgesSum'

        ds :: [TaskDistance]
        ds = legDistances $ (Point <$> edgesSum' :: [Zone Rational])

        dsSum :: [TaskDistance]
        dsSum = scanl1 addTaskDistance ds

goByEdge :: Bool -> PathDistance -> TrackLine
goByEdge excludeWaypoints ed =
    TrackLine
        { distance = toKm d
        , waypoints = if excludeWaypoints then [] else xs
        , legs = toKm <$> ds 
        , legsSum = toKm <$> dsSum
        }
    where
        d :: TaskDistance
        d = edgesSum ed

        -- NOTE: The graph of points created for determining the shortest
        -- path can have duplicate points, so the shortest path too can have
        -- duplicate points. Remove these duplicates.
        --
        -- I found that by decreasing defEps, the default epsilon, used for
        -- rational math from 1/10^9 to 1/10^12 these duplicates stopped
        -- occuring.
        vertices' :: [LatLng [u| rad |]]
        vertices' = nub $ vertices ed

        xs :: [RawLatLng]
        xs = convertLatLng <$> vertices'

        ds :: [TaskDistance]
        ds = legDistances $ (Point <$> vertices' :: [Zone Rational])

        dsSum :: [TaskDistance]
        dsSum = scanl1 addTaskDistance ds

addTaskDistance :: TaskDistance -> TaskDistance -> TaskDistance
addTaskDistance (TaskDistance a) (TaskDistance b) = TaskDistance $ a +: b

convertLatLng :: LatLng [u| rad |] -> RawLatLng
convertLatLng (LatLng (Lat eLat, Lng eLng)) =
    RawLatLng { lat = RawLat eLat'
              , lng = RawLng eLng'
              }
    where
        MkQuantity eLat' =
            convert eLat :: Quantity Rational [u| deg |]

        MkQuantity eLng' =
            convert eLng :: Quantity Rational [u| deg |]

toPoint :: RawLatLng -> Zone Rational
toPoint RawLatLng{..} =
    Point (LatLng (Lat latRad, Lng lngRad))
    where
        RawLat lat' = lat
        RawLng lng' = lng

        latDeg = MkQuantity lat' :: Quantity Rational [u| deg |]
        lngDeg = MkQuantity lng' :: Quantity Rational [u| deg |]

        latRad = convert latDeg :: Quantity Rational [u| rad |]
        lngRad = convert lngDeg :: Quantity Rational [u| rad |]

toCylinder :: Fractional a => RawZone -> Zone a
toCylinder RawZone{..} =
    Cylinder
        (fromRationalRadius $ Radius (MkQuantity $ radius % 1))
        (LatLng (Lat latRad, Lng lngRad))
    where
        RawLat lat' = lat
        RawLng lng' = lng

        latDeg = MkQuantity lat' :: Quantity Rational [u| deg |]
        lngDeg = MkQuantity lng' :: Quantity Rational [u| deg |]

        latRad = convert latDeg :: Quantity Rational [u| rad |]
        lngRad = convert lngDeg :: Quantity Rational [u| rad |]

span :: SpanLatLng
span = distanceHaversine defEps
