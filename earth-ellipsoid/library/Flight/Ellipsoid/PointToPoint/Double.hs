{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Ellipsoid.PointToPoint.Double (distanceVincenty) where

import Prelude hiding (sum, span)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.Distance (TaskDistance(..), SpanLatLng)
import Flight.Ellipsoid (Ellipsoid(..), flattening)

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
    :: (Num a, Floating a, Fractional a, RealFloat a)
    => Ellipsoid a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
vincentyInverse ellipsoid@Ellipsoid{semiMajor, semiMinor} tolerance λ l u1 u2 =
    if abs (λ - λ') < tolerance
       then s
       else vincentyInverse ellipsoid tolerance λ' l u1 u2
    where
        f = flattening ellipsoid

        i = cos u2 * sin λ
        j = cos u1 * sin u2 - sin u1 * cos u2 * cos λ
        i² = i * i 
        j² = j * j
        sinσ = sqrt $ i² + j²
        cosσ = sin u1 * sin u2 + cos u1 * cos u2 * cos λ
        σ = atan2 sinσ cosσ
        sinα = cos u1 * cos u2 * sin λ / sin σ
        cos²α = 1 - sinα * sinα 
        cos2σm = cosσ - 2 * sin u1 * sin u2 / cos²α
        cos²2σm = cos2σm * cos2σm
        c = f / 16 * cos²α * (4 - 3 * cos²α)
        x = σ + c * sinσ * y
        y = cos (2 * cos2σm + c * cosσ * (negate 1 + 2 * cos²2σm))
        λ' = l + (1 - c) * f * sinα * x

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
                (cos2σm + _B / 4 *
                    (cosσ * (negate 1 + 2 * cos²2σm)
                    - _B / 6
                    * cos2σm
                    * (negate 3 + 4 * sin²σ)
                    * (negate 3 + 4 * cos²2σm)
                    )
                )

        s = _B * _A * (σ - _Δσ)

-- | Sperical distance using inverse Vincenty and floating point numbers.
distanceVincenty :: RealFloat a => Ellipsoid a -> SpanLatLng a
distanceVincenty
    ellipsoid
    (LatLng (Lat (MkQuantity _Φ1), Lng (MkQuantity l1)))
    (LatLng (Lat (MkQuantity _Φ2), Lng (MkQuantity l2))) =
    TaskDistance . MkQuantity $ d
    where
        u1 = atan $ (1 - f) * tan _Φ1
        u2 = atan $ (1 - f) * tan _Φ2
        l = l2 - l1
        λ = l 
        d = vincentyInverse ellipsoid 0.000000000001 λ l u1 u2
        f = flattening ellipsoid
