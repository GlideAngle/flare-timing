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
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.TaskTrack.Internal
    ( ToTrackLine(..)
    , mm30
    , roundEastNorth
    , fromUTMRefEastNorth
    , fromUTMRefZone
    , legDistances
    , addTaskDistance
    , convertLatLng
    , rawToLatLng
    , toPoint
    , toCylinder
    ) where

import Prelude hiding (span)
import Data.UnitsOfMeasure ((+:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import qualified UTMRef as HC (UTMRef(..))

import Flight.Units ()
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.LatLng.Raw (RawLat(..), RawLng(..), RawLatLng(..))
import Data.Number.RoundingFunctions (dpRound)
import Flight.Zone (Zone(..), Radius(..), fromRationalZone)
import Flight.Zone.Raw (RawZone(..))
import Flight.Cylinder.Sample (Tolerance(..))
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.PointToPoint.Segment (SpanLatLng)
import Flight.ShortestPath (DistancePointToPoint)
import Flight.EastNorth (UtmZone(..), EastingNorthing(..))
import Flight.Route (TrackLine(..))
import Data.Aeson.ViaScientific (ViaScientific(..))

class ToTrackLine a where
    toTrackLine :: Bool -> a -> TrackLine

mm30 :: Num a => Tolerance a 
mm30 = Tolerance 30

roundEastNorth :: Integer -> EastingNorthing -> EastingNorthing
roundEastNorth dp EastingNorthing{..} =
    EastingNorthing
        { easting = f easting
        , northing = f northing
        }
    where
        f x = fromRational $ dpRound dp $ toRational x

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

legDistances :: Real a
             => DistancePointToPoint a
             -> SpanLatLng a
             -> [Zone a]
             -> [TaskDistance a]
legDistances dpp span xs =
    zipWith
        (\x y -> edgesSum $ dpp span [x, y])
        xs
        (tail xs)

addTaskDistance :: Num a => TaskDistance a -> TaskDistance a -> TaskDistance a
addTaskDistance (TaskDistance a) (TaskDistance b) = TaskDistance $ a +: b

convertLatLng :: (Real a, Fractional a) => LatLng a [u| rad |] -> RawLatLng
convertLatLng (LatLng (Lat eLat, Lng eLng)) =
    RawLatLng { lat = ViaScientific . RawLat $ toRational eLat'
              , lng = ViaScientific . RawLng $ toRational eLng'
              }
    where
        MkQuantity eLat' =
            convert eLat :: Quantity _ [u| deg |]

        MkQuantity eLng' =
            convert eLng :: Quantity _ [u| deg |]

rawToLatLng :: Fractional a
            => ViaScientific RawLat
            -> ViaScientific RawLng
            -> LatLng a [u| rad |]
rawToLatLng (ViaScientific (RawLat lat')) (ViaScientific (RawLng lng')) =
    LatLng (Lat latRad, Lng lngRad)
    where
        latDeg :: Quantity _ [u| deg |]
        latDeg = MkQuantity $ fromRational lat'

        lngDeg :: Quantity _ [u| deg |]
        lngDeg = MkQuantity $ fromRational lng'

        latRad = convert latDeg :: Quantity _ [u| rad |]
        lngRad = convert lngDeg :: Quantity _ [u| rad |]

toPoint :: (Eq a, Fractional a) => RawLatLng -> Zone a
toPoint RawLatLng{..} =
    fromRationalZone . Point $ rawToLatLng lat lng

toCylinder :: (Eq a, Num a, Fractional a) => RawZone -> Zone a
toCylinder RawZone{..} =
    Cylinder
        (Radius (MkQuantity $ fromInteger radius))
        (rawToLatLng lat lng)
