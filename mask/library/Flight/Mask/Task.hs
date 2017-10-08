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
    ExceptT . return . Right $ (taskTrack excludeWaypoints tdm) <$> tasks

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
                { pointToPoint =
                    Just $ TZ.TrackLine
                        { distance = toKm ptd
                        , waypoints = if excludeWaypoints then [] else wpPoint
                        , legs = toKm <$> legsPoint
                        }
                , edgeToEdge =
                    Just $ TZ.TrackLine
                        { distance = toKm etd
                        , waypoints = if excludeWaypoints then [] else wpEdge
                        , legs = toKm <$> legsEdge
                        }
                }
        TaskDistanceByPoints ->
            TZ.TaskTrack
                { pointToPoint =
                    Just $ TZ.TrackLine
                        { distance = toKm ptd
                        , waypoints = if excludeWaypoints then [] else wpPoint
                        , legs = toKm <$> legsPoint
                        }
                , edgeToEdge = Nothing
                }
        TaskDistanceByEdges ->
            TZ.TaskTrack
                { pointToPoint = Nothing
                , edgeToEdge =
                    Just $ TZ.TrackLine
                        { distance = toKm etd
                        , waypoints = if excludeWaypoints then [] else wpEdge
                        , legs = toKm <$> legsEdge
                        }
                }
    where
        zs :: [Zone]
        zs = toCylinder <$> zones

        ptd :: TaskDistance
        ptd = Tsk.distancePointToPoint zs

        centers :: [LatLng [u| rad |]]
        centers = center <$> zs

        wpPoint :: [TZ.LatLng]
        wpPoint = convertLatLng <$> centers

        legsPoint :: [TaskDistance]
        legsPoint =
            zipWith
                (\x y -> Tsk.distancePointToPoint [x, y])
                xs
                (tail xs)
            where
                xs = Point <$> centers

        ed :: EdgeDistance
        ed = Tsk.distanceEdgeToEdge PathPointToZone mm30 zs

        etd :: TaskDistance
        etd = edges ed

        edgeVertices :: [LatLng [u| rad |]]
        edgeVertices = edgeLine ed

        wpEdge :: [TZ.LatLng]
        wpEdge = convertLatLng <$> edgeVertices

        legsEdge :: [TaskDistance]
        legsEdge =
            zipWith
                (\x y -> Tsk.distancePointToPoint [x, y])
                xs
                (tail xs)
            where
                xs = Point <$> edgeVertices

        toKm :: TaskDistance -> Double
        toKm (TaskDistance d) =
            fromRational $ dpRound 3 dKm
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
