{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Ellipsoid.PointToPoint.Double (distanceVincenty) where

import Prelude hiding (sum, span)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.Distance (TaskDistance(..), SpanLatLng)
import Flight.Ellipsoid
    ( Ellipsoid(..), AbnormalLatLng(..), VincentyInverse(..), VincentyAccuracy(..)
    , defaultVincentyAccuracy, flattening
    )

-- SEE: https://en.wikipedia.org/wiki/Vincenty%27s_formulae#Inverse_problem
-- Notation[edit]
-- Define the following notation:
--
-- a
-- length of semi-major axis of the ellipsoid (radius at equator) (6378137.0
-- metres in WGS-84)
--
-- ƒ
-- flattening of the ellipsoid
-- (1/298.257223563 in WGS-84)
--
-- b = (1 − ƒ) a
-- length of semi-minor axis of the ellipsoid (radius at the poles)
-- (6356752.314245 meters in WGS-84)
--
-- Φ1, Φ2
-- latitude of the points
--
-- U1 = arctan[(1 − ƒ) tan Φ1]
-- U2 = arctan[(1 − ƒ) tan Φ2]
-- reduced latitude (latitude on the auxiliary sphere)
--
-- L = L2 − L1
-- difference in longitude of two points
--
-- λ1, λ2
-- longitude of the points on the auxiliary sphere
--
-- α1, α2
-- forward azimuths at the points
--
-- α
-- azimuth at the equator
--
-- s
-- ellipsoidal distance between the two points
--
-- σ
-- arc length between points on the auxiliary sphere
vincentyInverse
    :: (Num a, Floating a, Fractional a, RealFloat a, Show a)
    => Ellipsoid a
    -> VincentyAccuracy a
    -> a
    -> a
    -> a
    -> a
    -> VincentyInverse a
vincentyInverse
    ellipsoid@Ellipsoid{semiMajor = MkQuantity a, semiMinor = MkQuantity b}
    accuracy@(VincentyAccuracy tolerance)
    λ _L _U1 _U2 =
    if abs λ > pi
        then VincentyAntipodal
        else
            if abs (λ - λ') < tolerance
                then VincentyInverse $ b * _A * (σ - _Δσ)
                else vincentyInverse ellipsoid accuracy λ' _L _U1 _U2
    where
        f = flattening ellipsoid
        (sinU1, sinU2, sinλ) = (sin _U1, sin _U2, sin λ)
        (cosU1, cosU2, cosλ) = (cos _U1, cos _U2, cos λ)

        i = cosU2 * sinλ
        j = cosU1 * sinU2 - sinU1 * cosU2 * cosλ
        sin²σ = i * i + j * j
        sinσ = sqrt sin²σ
        cosσ = sinU1 * sinU2 + cosU1 * cosU2 * cosλ

        σ = atan2 sinσ cosσ

        sinα = cosU1 * cosU2 * sinλ / sinσ
        cos²α = 1 - sinα * sinα

        -- NOTE: Start and end points on the equator, _C = 0.
        cos2σm = if cos²α == 0 then 0 else cosσ - 2 * sinU1 * sinU2 / cos²α

        cos²2σm = cos2σm * cos2σm
        _C = f / 16 * cos²α * (4 + f * (4 - 3 * cos²α))
        λ' = _L + (1 - _C) * f * sinα * (σ + _C * sinσ * x)
        x = cos2σm + _C * cosσ * (-1 + 2 * cos²2σm)

        u² = let b² = b * b in cos²α * (a * a - b²) / b² 
        _A = 1 + u² / 16384 * (4096 + u² * (-768 + u² * (320 - 175 * u²)))
        _B = u² / 1024 * (256 + u² * (-128 + u² * (74 - 47 * u²)))

        _Δσ = _B * sinσ * (cos2σm + _B / 4 * y)
        y =
            cosσ * (-1 + 2 * cos²2σm)
            - _B / 6 * cos2σm * (-3 + 4 * sin²σ) * (-3 + 4 * cos²2σm)

tooFar :: Num a => TaskDistance a
tooFar = TaskDistance [u| 20000000 m |]

-- | Sperical distance using inverse Vincenty and floating point numbers.
distanceVincenty :: (RealFloat a, Show a) => Ellipsoid a -> SpanLatLng a
distanceVincenty e lat lng =
    case distanceVincenty' e lat lng of
        VincentyInverse d' -> d'
        VincentyAntipodal -> tooFar
        VincentyAbnormal _ -> tooFar

distanceVincenty'
    :: (RealFloat a, Show a)
    => Ellipsoid a
    -> LatLng a [u| rad |]
    -> LatLng a [u| rad |]
    -> VincentyInverse (TaskDistance a)
distanceVincenty'
    ellipsoid
    x@(LatLng xLat@(Lat (MkQuantity _Φ1), xLng@(Lng (MkQuantity _L1))))
    y@(LatLng yLat@(Lat (MkQuantity _Φ2), yLng@(Lng (MkQuantity _L2))))

    | xLat < minBound = VincentyAbnormal LatUnder
    | xLat > maxBound = VincentyAbnormal LatOver
    | xLng < minBound = VincentyAbnormal LngUnder
    | xLng > maxBound = VincentyAbnormal LngOver

    | yLat < minBound = VincentyAbnormal LatUnder
    | yLat > maxBound = VincentyAbnormal LatOver
    | yLng < minBound = VincentyAbnormal LngUnder
    | yLng > maxBound = VincentyAbnormal LngOver

    | x == y = VincentyInverse $ TaskDistance [u| 0 m |]
    | otherwise =
        case d of
            VincentyInverse d' ->
                VincentyInverse . TaskDistance . MkQuantity $ d'
                
            VincentyAntipodal -> VincentyAntipodal
            VincentyAbnormal ab -> VincentyAbnormal ab
        where
            f = flattening ellipsoid
            _U1 = atan $ (1 - f) * tan _Φ1
            _U2 = atan $ (1 - f) * tan _Φ2
            _L = _L2 - _L1
            λ = _L

            VincentyAccuracy accuracy = defaultVincentyAccuracy
            accuracy' = VincentyAccuracy $ fromRational accuracy
            
            d = vincentyInverse ellipsoid accuracy' λ _L _U1 _U2
