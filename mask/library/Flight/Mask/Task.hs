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
{-# LANGUAGE DeriveDataTypeable #-}

{-|
Module      : Data.Flight.Mask.Task
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

What is the optimized track and its turnpoints?
-}
module Flight.Mask.Task (taskTracks, TaskDistanceMeasure(..)) where

import Data.Ratio ((%))
import Data.List (nub)
import System.Console.CmdArgs.Implicit (Default(..), def, Data, Typeable)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Control.Monad.Except (ExceptT(..))

import qualified Data.Flight.Comp as Cmp
    ( CompSettings(..)
    , Task(..)
    , Zone(..)
    , Latitude(..)
    , Longitude(..)
    )
import Flight.Task as Tsk
    ( Lat(..)
    , Lng(..)
    , LatLng(..)
    , Radius(..)
    , Zone(..)
    , TaskDistance(..)
    , EdgeDistance(..)
    , Tolerance(..)
    , DistancePath(..)
    , distancePointToPoint
    , distanceEdgeToEdge
    , center
    )
import qualified Data.Flight.TrackZone as TZ
    ( TaskTrack(..)
    , TrackLine(..)
    , LatLng(..)
    )
import Flight.Units ()
import Flight.Mask.Settings (readCompSettings)
import Data.Number.RoundingFunctions (dpRound)
--
-- | The way to measure the task distance.
data TaskDistanceMeasure
    = TaskDistanceByAllMethods
    | TaskDistanceByPoints
    | TaskDistanceByEdges
    deriving (Eq, Data, Typeable, Show)

instance Default TaskDistanceMeasure where
    def = TaskDistanceByAllMethods

mm30 :: Tolerance
mm30 = Tolerance $ 30 % 1000

followTracks :: Bool
             -> TaskDistanceMeasure
             -> Cmp.CompSettings
             -> ExceptT String IO [TZ.TaskTrack]
followTracks excludeWaypoints tdm Cmp.CompSettings{tasks} =
    ExceptT . return . Right $ taskTrack excludeWaypoints tdm <$> tasks

taskTracks :: Bool
           -> TaskDistanceMeasure
           -> FilePath
           -> ExceptT String IO [TZ.TaskTrack]
taskTracks excludeWaypoints tdm compYamlPath = do
    settings <- readCompSettings compYamlPath
    followTracks excludeWaypoints tdm settings

taskTrack :: Bool -> TaskDistanceMeasure -> Cmp.Task -> TZ.TaskTrack
taskTrack excludeWaypoints tdm Cmp.Task{..} =
    case tdm of
        TaskDistanceByAllMethods ->
            TZ.TaskTrack
                { pointToPoint = Just pointTrackline
                , edgeToEdge = Just edgeTrackline
                }
        TaskDistanceByPoints ->
            TZ.TaskTrack
                { pointToPoint = Just pointTrackline
                , edgeToEdge = Nothing
                }
        TaskDistanceByEdges ->
            TZ.TaskTrack
                { pointToPoint = Nothing
                , edgeToEdge = Just edgeTrackline
                }
    where
        pointTrackline =
            TZ.TrackLine
                { distance = toKm (dpRound 3) ptd
                , waypoints = if excludeWaypoints then [] else wpPoint
                , legs = toKm (dpRound 3) <$> legsPoint
                }

        edgeTrackline =
            TZ.TrackLine
                { distance = toKm (dpRound 3) etd
                , waypoints = if excludeWaypoints then [] else wpEdge
                , legs = toKm (dpRound 3) <$> legsEdge
                }

        zs :: [Zone]
        zs = toCylinder <$> zones

        ptd :: TaskDistance
        ptd = Tsk.distancePointToPoint zs

        -- NOTE: Concentric zones of different radii can be defined that
        -- share the same center. Remove duplicate centers.
        centers :: [LatLng [u| rad |]]
        centers = nub $ center <$> zs

        wpPoint :: [TZ.LatLng]
        wpPoint = convertLatLng <$> centers

        legDistances :: [Zone] -> [TaskDistance]
        legDistances xs =
            zipWith
                (\x y -> Tsk.distancePointToPoint [x, y])
                xs
                (tail xs)

        legsPoint :: [TaskDistance]
        legsPoint = legDistances $ Point <$> centers

        ed :: EdgeDistance
        ed = Tsk.distanceEdgeToEdge PathPointToZone mm30 zs

        etd :: TaskDistance
        etd = edges ed

        -- NOTE: The graph of points created for determining the shortest
        -- path can have duplicate points, so the shortest path too can have
        -- duplicate points. Remove these duplicates.
        --
        -- I found that by decreasing defEps, the default epsilon, used for
        -- rational math from 1/10^9 to 1/10^12 these duplicates stopped
        -- occuring.
        edgeVertices :: [LatLng [u| rad |]]
        edgeVertices = nub $ edgeLine ed

        wpEdge :: [TZ.LatLng]
        wpEdge = convertLatLng <$> edgeVertices

        legsEdge :: [TaskDistance]
        legsEdge = legDistances $ Point <$> edgeVertices

        toKm :: (Rational -> Rational) -> TaskDistance -> Double
        toKm f (TaskDistance d) =
            fromRational $ f dKm
            where 
                MkQuantity dKm = convert d :: Quantity Rational [u| km |]

convertLatLng :: LatLng [u| rad |] -> TZ.LatLng
convertLatLng (LatLng (Lat eLat, Lng eLng)) =
    TZ.LatLng { lat = Cmp.Latitude eLat'
              , lng = Cmp.Longitude eLng'
              }
    where
        MkQuantity eLat' =
            convert eLat :: Quantity Rational [u| deg |]

        MkQuantity eLng' =
            convert eLng :: Quantity Rational [u| deg |]

toCylinder :: Cmp.Zone -> Zone
toCylinder Cmp.Zone{..} =
    Cylinder
        (Radius (MkQuantity $ radius % 1))
        (LatLng (Lat latRad, Lng lngRad))
    where
        Cmp.Latitude lat' = lat
        Cmp.Longitude lng' = lng

        latDeg = MkQuantity lat' :: Quantity Rational [u| deg |]
        lngDeg = MkQuantity lng' :: Quantity Rational [u| deg |]

        latRad = convert latDeg :: Quantity Rational [u| rad |]
        lngRad = convert lngDeg :: Quantity Rational [u| rad |]
