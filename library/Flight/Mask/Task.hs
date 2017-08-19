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

{-|
Module      : Data.Flight.Mask.Task
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

What is the optimized track and its turnpoints?
-}
module Flight.Mask.Task (taskTrack) where

import Data.Ratio ((%))
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import qualified Data.Flight.Comp as Cmp
    ( Task(..)
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
import Data.Number.RoundingFunctions (dpRound)

mm30 :: Tolerance
mm30 = Tolerance $ 30 % 1000

taskTrack :: Cmp.Task -> TZ.TaskTrack
taskTrack Cmp.Task{..} =
    TZ.TaskTrack
        { pointToPoint =
            TZ.TrackLine
                { distance = toKm ptd
                , waypoints = wpPoint
                }
        , edgeToEdge =
            TZ.TrackLine
                { distance = toKm etd
                , waypoints = wpEdge
                }
        }
    where
        zs :: [Zone]
        zs = toCylinder <$> zones

        ptd :: TaskDistance
        ptd = Tsk.distancePointToPoint zs

        wpPoint :: [TZ.LatLng]
        wpPoint =
            convertLatLng <$> ps
            where
                ps = center <$> zs

        ed :: EdgeDistance
        ed = Tsk.distanceEdgeToEdge PathPointToZone mm30 zs

        etd :: TaskDistance
        etd = edges ed

        wpEdge :: [TZ.LatLng]
        wpEdge =
            convertLatLng <$> xs
            where
                xs = edgeLine ed

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
