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
    ellipsoid@Ellipsoid{semiMajor, semiMinor}
    accuracy@(VincentyAccuracy tolerance)
    λ _L _U1 _U2 =
    if abs λ > pi
        then VincentyAntipodal
        else
            if abs (λ - λ') < tolerance
                then VincentyInverse s
                else vincentyInverse ellipsoid accuracy λ' _L _U1 _U2
    where
        f = flattening ellipsoid

        i = cos _U2 * sin λ
        j = cos _U1 * sin _U2 - sin _U1 * cos _U2 * cos λ
        i² = i * i 
        j² = j * j
        sinσ = sqrt $ i² + j²
        cosσ = sin _U1 * sin _U2 + cos _U1 * cos _U2 * cos λ
        σ = atan2 sinσ cosσ
        sinα = cos _U1 * cos _U2 * sin λ / sin σ
        sin²α = sinα * sinα 
        cos²α = 1 - sin²α 

        cos2σm =
            if cos²α == 0
                then
                    -- NOTE: Start and end points on the equator, _C = 0.
                    0
                else
                    cosσ - 2 * sin _U1 * sin _U2 / cos²α

        cos²2σm = cos2σm * cos2σm
        _C = f / 16 * cos²α * (4 + f * (4 - 3 * cos²α))
        x = σ + _C * sinσ * y
        y = cos2σm + _C * cosσ * (negate 1 + 2 * cos²2σm)
        λ' = _L + (1 - _C) * f * sinα * x

        MkQuantity a = semiMajor
        MkQuantity b = semiMinor
        a² = a * a
        b² = b * b
        u² = cos²α * (a² - b²) / b² 
        _A = 1 + u² / 16384 * (4096 + u² * (negate 768 + u² * (320 - 175 * u²)))
        _B = u² / 1024 * (256 + u² * (negate 128 + u² * (74 - 47 * u²)))
        sin²σ = sinσ * sinσ

        _Δσ =
            _B * sinσ *
                (cos2σm
                    + _B / 4
                    *
                        (cosσ * (negate 1 + 2 * cos²2σm)
                        - _B / 6
                        * cos2σm
                        * (negate 3 + 4 * sin²σ)
                        * (negate 3 + 4 * cos²2σm)
                        )
                )

        s = b * _A * (σ - _Δσ)

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
            VincentyInverse d' -> VincentyInverse . TaskDistance . MkQuantity $ d'
            VincentyAntipodal -> VincentyAntipodal
            VincentyAbnormal ab -> VincentyAbnormal ab
        where
            _U1 = atan $ (1 - f) * tan _Φ1
            _U2 = atan $ (1 - f) * tan _Φ2
            _L = _L2 - _L1
            λ = _L 
            d = vincentyInverse ellipsoid accuracy' λ _L _U1 _U2
            f = flattening ellipsoid

            VincentyAccuracy accuracy = defaultVincentyAccuracy
            accuracy' = VincentyAccuracy $ fromRational accuracy
