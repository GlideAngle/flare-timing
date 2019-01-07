{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.TaskTrack.Internal
    ( mm30
    , roundEastNorth
    , fromUTMRefEastNorth
    , fromUTMRefZone
    , legDistances
    , addTaskDistance
    , convertLatLng
    , rawToLatLng
    , toPoint
    , toCylinder
    , fromR
    ) where

import Prelude hiding (span)
import Data.UnitsOfMeasure ((+:), u, convert, fromRational', toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import qualified UTMRef as HC (UTMRef(..))

import Flight.Units ()
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.LatLng.Raw (RawLat(..), RawLng(..), RawLatLng(..))
import Data.Ratio.Rounding (dpRound)
import Flight.Zone (Zone(..), QRadius, Radius(..), fromRationalZone)
import Flight.Zone.Raw (RawZone(..))
import Flight.Zone.Cylinder (Tolerance(..))
import Flight.Distance
    (QTaskDistance, TaskDistance(..), PathDistance(..), SpanLatLng)
import Flight.EastNorth (UtmZone(..), EastingNorthing(..))
import Flight.Task (DistancePointToPoint)

fromR :: QTaskDistance Rational [u| m |] -> QTaskDistance Double [u| m |]
fromR (TaskDistance d) = TaskDistance . fromRational' $ d

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
        , lngZone = fromIntegral lngZone
        }

legDistances :: Real a
             => DistancePointToPoint a
             -> SpanLatLng a
             -> [Zone a]
             -> [QTaskDistance a [u| m |]]
legDistances dpp span xs =
    zipWith
        (\x y -> edgesSum $ dpp span [x, y])
        xs
        (tail xs)

addTaskDistance
    :: Num a
    => QTaskDistance a [u| m |]
    -> QTaskDistance a [u| m |]
    -> QTaskDistance a [u| m |]
addTaskDistance (TaskDistance a) (TaskDistance b) =TaskDistance $ a +: b

convertLatLng :: (Real a, Fractional a) => LatLng a [u| rad |] -> RawLatLng
convertLatLng (LatLng (Lat eLat, Lng eLng)) =
    RawLatLng { lat = RawLat $ toRational eLat'
              , lng = RawLng $ toRational eLng'
              }
    where
        MkQuantity eLat' =
            convert eLat :: Quantity _ [u| deg |]

        MkQuantity eLng' =
            convert eLng :: Quantity _ [u| deg |]

rawToLatLng :: Fractional a
            => RawLat
            -> RawLng
            -> LatLng a [u| rad |]
rawToLatLng (RawLat lat') (RawLng lng') =
    LatLng (Lat latRad, Lng lngRad)
    where
        latDeg :: Quantity _ [u| deg |]
        latDeg = MkQuantity $ fromRational lat'

        lngDeg :: Quantity _ [u| deg |]
        lngDeg = MkQuantity $ fromRational lng'

        latRad = convert latDeg :: Quantity _ [u| rad |]
        lngRad = convert lngDeg :: Quantity _ [u| rad |]

rawToRadius
    :: Fractional a
    => Radius (Quantity Double [u| m |])
    -> QRadius a [u| m |]
rawToRadius (Radius r) =
    Radius . fromRational' . toRational' $ r

toPoint :: (Eq a, Ord a, Fractional a) => RawLatLng -> Zone a
toPoint RawLatLng{..} =
    fromRationalZone . Point $ rawToLatLng lat lng

toCylinder :: (Eq a, Ord a, Num a, Fractional a) => RawZone -> Zone a
toCylinder RawZone{..} =
    Cylinder
        (rawToRadius radius)
        (rawToLatLng lat lng)
