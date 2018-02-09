{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Ellipsoid.PointToPoint.Rational (distanceVincenty) where

import Prelude hiding (sum, span)
import Data.Ratio((%))
import qualified Data.Number.FixedFunctions as F
import Data.UnitsOfMeasure (fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.LatLng.Rational (Epsilon(..))
import Flight.Zone (toRationalLatLng)
import Flight.Distance (TaskDistance(..), SpanLatLng)
import Flight.Ellipsoid (Ellipsoid(..), flattening, toRationalEllipsoid)

vincentyInverse
    :: Epsilon
    -> Ellipsoid Rational
    -> Rational
    -> Rational
    -> Rational
    -> Rational
    -> Rational
    -> Rational
vincentyInverse
    e@(Epsilon eps) ellipsoid@Ellipsoid{semiMajor, semiMinor} tolerance λ l u1 u2 =
    if abs (λ - λ') < tolerance
       then s
       else vincentyInverse e ellipsoid tolerance λ' l u1 u2
    where
        f :: Rational
        f = toRational . flattening $ ellipsoid

        i = cos' u2 * sin' λ
        j = cos' u1 * sin' u2 - sin' u1 * cos' u2 * cos' λ
        i² = i * i
        j² = j * j
        sinσ = F.sqrt eps $ i² + j²
        cosσ = sin' u1 * sin' u2 + cos' u1 * cos' u2 * cos' λ
        σ = atan' $ sinσ / cosσ
        sinα = cos' u1 * cos' u2 * sin' λ / sin' σ
        cos²α = 1 - sinα * sinα 
        cos2σm = cosσ - 2 * sin' u1 * sin' u2 / cos²α
        cos²2σm = cos2σm * cos2σm
        c = f / 16 * cos²α * (4 - 3 * cos²α)
        x = σ + c * sinσ * y
        y = cos' (2 * cos2σm + c * cosσ * (negate 1 + 2 * cos²2σm))
        λ' = l + (1 - c) * f * sinα * x

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
                (cos2σm + _B / 4 *
                    (cosσ * (negate 1 + 2 * cos²2σm)
                    - _B / 6
                    * cos2σm
                    * (negate 3 + 4 * sin²σ)
                    * (negate 3 + 4 * cos²2σm)
                    )
                )

        s = _B * _A * (σ - _Δσ)

-- | Sperical distance using inverse Vincenty and rational numbers.
distanceVincenty
    :: (Real a, Fractional a)
    => Epsilon
    -> Ellipsoid a
    -> SpanLatLng a
distanceVincenty e@(Epsilon eps) ellipsoid x y =
    TaskDistance . fromRational' . MkQuantity $ d
    where
        (LatLng (Lat (MkQuantity _Φ1), Lng (MkQuantity l1))) = toRationalLatLng x
        (LatLng (Lat (MkQuantity _Φ2), Lng (MkQuantity l2))) = toRationalLatLng y

        u1 :: Rational
        u1 = atan' $ (1 - f) * tan' _Φ1

        u2 :: Rational
        u2 = atan' $ (1 - f) * tan' _Φ2

        l :: Rational
        l = l2 - l1

        λ :: Rational
        λ = l

        ellipsoidR = toRationalEllipsoid ellipsoid

        d :: Rational
        d = vincentyInverse e ellipsoidR (1 % 1000000000000) λ l u1 u2

        f :: Rational
        f = flattening ellipsoidR

        tan' = F.tan eps
        atan' = F.atan eps
