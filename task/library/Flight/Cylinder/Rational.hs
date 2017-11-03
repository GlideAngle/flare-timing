{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Flight.Cylinder.Rational (circumSample) where

import Prelude hiding (span)
import Data.Ratio ((%))
import qualified Data.Number.FixedFunctions as F
import Data.Fixed (mod')
import Data.UnitsOfMeasure (u, unQuantity, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng
    ( Lat(..)
    , Lng(..)
    , LatLng(..)
    , Epsilon(..)
    , earthRadius
    , defEps
    )
import Flight.Zone
    ( Zone(..)
    , Radius(..)
    , Bearing(..)
    , center
    , radius
    , toRationalZone
    )
import Flight.PointToPoint (distancePointToPoint, distanceHaversine)
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.Cylinder.Sample
    ( TrueCourse(..)
    , ZonePoint(..)
    , Tolerance(..)
    , Samples(..)
    , SampleParams(..)
    , CircumSample
    , orbit
    , radial
    , point
    , sourceZone
    , fromRationalZonePoint
    )

-- | Using a method from the
-- <http://www.edwilliams.org/avform.htm#LL Aviation Formulary>
-- a point on a cylinder wall is found by going out to the distance of the
-- radius on the given radial true course 'rtc'.
circum :: Real a
       => LatLng [u| rad |]
       -> Epsilon
       -> Radius a
       -> TrueCourse
       -> LatLng [u| rad |]
circum
    (LatLng (Lat (MkQuantity latRadian'), Lng (MkQuantity lngRadian')))
    _
    (Radius (MkQuantity rRadius))
    (TrueCourse rtc) =
    LatLng (Lat lat'', Lng lng'')
    where
        lat :: Double
        lat = fromRational latRadian'

        lng :: Double
        lng = fromRational lngRadian'

        MkQuantity tc = fromRational' rtc :: Quantity Double [u| rad |]

        radius' :: Double
        radius' = fromRational . toRational $ rRadius

        bigR = fromRational $ unQuantity earthRadius

        lat' :: Double
        lat' = asin (sin lat * cos d + cos lat * sin d * cos tc)

        dlng = atan ((sin tc * sin d * cos lat) / (cos d - sin lat * sin lat))

        a = lng - dlng + pi 
        b = 2 * pi 

        lng' :: Double
        lng' = mod' a b - pi

        d = radius' / bigR

        lat'' :: Quantity Rational [u| rad |]
        lat'' = MkQuantity $ toRational lat'

        lng'' :: Quantity Rational [u| rad |]
        lng'' = MkQuantity $ toRational lng'

-- | Generates a pair of lists, the lat/lng of each generated point
-- and its distance from the center. It will generate 'samples' number of such
-- points that should lie close to the circle. The difference between
-- the distance to the origin and the radius should be less han the 'tolerance'.
--
-- The points of the compass are divided by the number of samples requested.
circumSample :: CircumSample a
circumSample SampleParams{..} (Bearing (MkQuantity bearing)) zp zone =
    (fromRationalZonePoint <$> fst ys, snd ys)
    where
        (Epsilon eps) = defEps

        nNum = unSamples spSamples
        half = nNum `div` 2
        pi' = F.pi eps
        halfRange = pi' / bearing

        zone' =
            case zp of
              Nothing -> zone
              Just ZonePoint{..} -> sourceZone

        xs :: [TrueCourse]
        xs =
            TrueCourse . MkQuantity <$>
            case zp of
                Nothing ->
                    [ (2 * n % nNum) * pi' | n <- [0 .. nNum]]

                Just ZonePoint{..} ->
                    [b]
                    ++ 
                    [ b - (n % half) * halfRange | n <- [1 .. half] ]
                    ++
                    [ b + (n % half) * halfRange | n <- [1 .. half]]
                    where
                        (Bearing (MkQuantity b)) = radial

        (Radius (MkQuantity limitRadius)) = radius zone'
        limitRadius' = toRational limitRadius
        r = Radius (MkQuantity limitRadius')

        ptCenter = center zone'
        circumR = circum ptCenter defEps

        ys :: ([ZonePoint Rational], [TrueCourse])
        ys = unzip $ getClose 10 (Radius (MkQuantity 0)) (circumR r) <$> xs

        getClose :: Int
                 -> Radius Rational
                 -> (TrueCourse -> LatLng [u| rad |])
                 -> TrueCourse
                 -> (ZonePoint Rational, TrueCourse)
        getClose trys yr@(Radius (MkQuantity offset)) f x@(TrueCourse tc)
            | trys <= 0 = (zp', dist)
            | unTolerance spTolerance <= 0 = (zp', dist)
            | limitRadius <= unTolerance spTolerance = (zp', dist)
            | otherwise =
                case d `compare` toRational limitRadius of
                     EQ ->
                         (zp', dist)

                     GT ->
                         let offset' =
                                 offset - (d - limitRadius') * 105 / 100

                             f' =
                                 circumR (Radius (MkQuantity $ limitRadius' + offset'))

                         in getClose (trys - 1) (Radius (MkQuantity offset')) f' x
                         
                     LT ->
                         if d > toRational (limitRadius - unTolerance spTolerance)
                         then (zp', dist)
                         else
                             let offset' =
                                     offset + (limitRadius' - d) * 94 / 100

                                 f' =
                                     circumR (Radius (MkQuantity $ limitRadius' + offset'))
                             in getClose (trys - 1) (Radius (MkQuantity offset')) f' x
            where
                y = f x
                zp' = ZonePoint { sourceZone = toRationalZone zone'
                                , point = y
                                , radial = Bearing tc
                                , orbit = yr
                                } :: ZonePoint Rational
                               
                (TaskDistance (MkQuantity d)) =
                    edgesSum
                    $ distancePointToPoint
                        (distanceHaversine defEps)
                        ([Point ptCenter, Point y] :: [Zone Rational])

                dist = fromRational d
