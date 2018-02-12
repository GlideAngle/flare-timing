{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Ellipsoid.PointToPoint.Rational (distanceVincenty) where

import Prelude hiding (sum, span)
import qualified Data.Number.FixedFunctions as F
import Data.UnitsOfMeasure (u, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.LatLng.Rational (Epsilon(..))
import Flight.Zone (toRationalLatLng)
import Flight.Distance (TaskDistance(..), SpanLatLng)
import Flight.Ellipsoid
    (Ellipsoid(..), VincentyAccuracy(..)
    , defaultVincentyAccuracy, flattening, toRationalEllipsoid
    )

vincentyInverse
    :: Epsilon
    -> Ellipsoid Rational
    -> VincentyAccuracy Rational
    -> Rational
    -> Rational
    -> Rational
    -> Rational
    -> Rational
vincentyInverse
    e@(Epsilon eps)
    ellipsoid@Ellipsoid{semiMajor, semiMinor}
    accuracy@(VincentyAccuracy tolerance)
    λ _L _U1 _U2 =
    if abs (λ - λ') < tolerance
       then s
       else vincentyInverse e ellipsoid accuracy λ' _L _U1 _U2
    where
        f :: Rational
        f = toRational . flattening $ ellipsoid

        i = cos' _U2 * sin' λ
        j = cos' _U1 * sin' _U2 - sin' _U1 * cos' _U2 * cos' λ
        i² = i * i
        j² = j * j
        sinσ = F.sqrt eps $ i² + j²
        cosσ = sin' _U1 * sin' _U2 + cos' _U1 * cos' _U2 * cos' λ
        σ = atan' $ sinσ / cosσ
        sinα = cos' _U1 * cos' _U2 * sin' λ / sin' σ
        sin²α = sinα * sinα 
        cos²α = 1 - sin²α 

        cos2σm =
            if cos²α == 0
                then
                    -- NOTE: Start and end points on the equator, _C = 0.
                    0
                else
                    cosσ - 2 * sin' _U1 * sin' _U2 / cos²α

        cos²2σm = cos2σm * cos2σm
        _C = f / 16 * cos²α * (4 - 3 * cos²α)
        x = σ + _C * sinσ * y
        y = cos2σm + _C * cosσ * (negate 1 + 2 * cos²2σm)
        λ' = _L + (1 - _C) * f * sinα * x

        sin' = F.sin eps
        cos' = F.cos eps
        atan' = F.atan eps

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

-- | Sperical distance using inverse Vincenty and rational numbers.
distanceVincenty
    :: (Real a, Fractional a)
    => Epsilon
    -> Ellipsoid a
    -> SpanLatLng a
distanceVincenty e@(Epsilon eps) ellipsoid
    x@(LatLng (xLat, xLng))
    y@(LatLng (yLat, yLng))

    | xLat < minBound = error "Latitude x < -90 deg"
    | xLat > maxBound = error "Latitude x > 90 deg"
    | xLng < minBound = error "Longitude x < -180 deg"
    | xLng > maxBound = error "Longitude x > 180 deg"

    | yLat < minBound = error "Latitude y < -90 deg"
    | yLat > maxBound = error "Latitude y > 90 deg"
    | yLng < minBound = error "Longitude y < -180 deg"
    | yLng > maxBound = error "Longitude y > 180 deg"

    | x == y = TaskDistance [u| 0 m |]
    | otherwise =
        TaskDistance . fromRational' . MkQuantity $ d
        where
            (LatLng (Lat (MkQuantity _Φ1), Lng (MkQuantity _L1))) =
                    toRationalLatLng x

            (LatLng (Lat (MkQuantity _Φ2), Lng (MkQuantity _L2))) =
                    toRationalLatLng y

            _U1 = atan' $ (1 - f) * tan' _Φ1
            _U2 = atan' $ (1 - f) * tan' _Φ2
            _L = _L2 - _L1
            λ = _L

            ellipsoidR = toRationalEllipsoid ellipsoid
            f = flattening ellipsoidR
            accuracy = defaultVincentyAccuracy

            d :: Rational
            d = vincentyInverse e ellipsoidR accuracy λ _L _U1 _U2

            tan' = F.tan eps
            atan' = F.atan eps
