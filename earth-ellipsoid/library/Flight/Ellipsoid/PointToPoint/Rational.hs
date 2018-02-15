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
    ( Ellipsoid(..), AbnormalLatLng(..), VincentyInverse(..), VincentyAccuracy(..)
    , defaultVincentyAccuracy, flattening, toRationalEllipsoid
    )

-- | The numbers package doesn't have atan₂.
-- SEE: https://hackage.haskell.org/package/base
-- SEE: https://stackoverflow.com/questions/₂83406/what-is-the-difference-between-atan-and-atan₂-in-c
atan2' :: Epsilon -> Rational -> Rational -> Rational
atan2' e@(Epsilon eps) y x
    | x > 0 = atan' $ y / x
    | x == 0 && y > 0 = pi' / 2
    | x <  0 && y > 0 = pi' + atan' (y / x)
    | (x <= 0 && y < 0) = negate $ atan2' e (-y) x
    | y == 0 = pi'
    | x == 0 && y == 0 = y
    | otherwise = atan' $ y / x
    where
        atan' = F.atan eps
        pi' = F.pi eps

vincentyInverse
    :: Epsilon
    -> Ellipsoid Rational
    -> VincentyAccuracy Rational
    -> LatLng Rational [u| rad |]
    -> LatLng Rational [u| rad |]
    -> VincentyInverse Rational
vincentyInverse
    e@(Epsilon eps)
    ellipsoid@Ellipsoid{semiMajor = MkQuantity a, semiMinor = MkQuantity b}
    (VincentyAccuracy tolerance)
    (LatLng (Lat (MkQuantity _Φ₁), Lng (MkQuantity _L₁)))
    (LatLng (Lat (MkQuantity _Φ₂), Lng (MkQuantity _L₂))) =
    loop _L
    where
        sin' = F.sin eps
        cos' = F.cos eps
        tan' = F.tan eps
        atan' = F.atan eps

        f = flattening ellipsoid
        auxLat = atan' . ((1 - f) *) . tan'
        _U₁ = auxLat _Φ₁; _U₂ = auxLat _Φ₂
        _L = _L₂ - _L₁

        sinU₁ = sin' _U₁; sinU₂ = sin' _U₂
        cosU₁ = cos' _U₁; cosU₂ = cos' _U₂
        sinU₁sinU₂ = sinU₁ * sinU₂
        cosU₁cosU₂ = cosU₁ * cosU₂

        loop λ =
            if abs λ > F.pi eps
                then VincentyAntipodal
                else
                    if abs (λ - λ') < tolerance
                        then VincentyInverse $ b * _A * (σ - _Δσ)
                        else loop λ'
            where
                sinλ = sin' λ
                cosλ = cos' λ

                i = cosU₂ * sinλ
                j = cosU₁ * sinU₂ - sinU₁ * cosU₂ * cosλ
                sin²σ = i * i + j * j
                sinσ = F.sqrt eps sin²σ
                cosσ = sinU₁sinU₂ + cosU₁cosU₂ * cosλ

                σ = atan2' e sinσ cosσ

                sinα = cosU₁cosU₂ * sinλ / sinσ
                cos²α = 1 - sinα * sinα
                _C = f / 16 * cos²α * (4 + f * (4 - 3 * cos²α))
                u² = let b² = b * b in cos²α * (a * a - b²) / b² 

                -- NOTE: Start and end points on the equator, _C = 0.
                cos2σm = if cos²α == 0 then 0 else cosσ - 2 * sinU₁sinU₂ / cos²α
                cos²2σm = cos2σm * cos2σm

                _A = 1 + u² / 16384 * (4096 + u² * (-768 + u² * (320 - 175 * u²)))
                _B = u² / 1024 * (256 + u² * (-128 + u² * (74 - 47 * u²)))

                _Δσ = _B * sinσ * (cos2σm + _B / 4 * y)
                y =
                    cosσ * (-1 + 2 * cos²2σm)
                    - _B / 6 * cos2σm * (-3 + 4 * sin²σ) * (-3 + 4 * cos²2σm)

                λ' = _L + (1 - _C) * f * sinα * (σ + _C * sinσ * x)
                x = cos2σm + _C * cosσ * (-1 + 2 * cos²2σm)

tooFar :: Num a => TaskDistance a
tooFar = TaskDistance [u| 20000000 m |]

-- | Sperical distance using inverse Vincenty and rational numbers.
distanceVincenty
    :: (Real a, Fractional a, Show a)
    => Epsilon
    -> Ellipsoid a
    -> SpanLatLng a
distanceVincenty epsilon ellipsoid lat lng =
    case distanceVincenty' epsilon ellipsoid lat lng of
        VincentyInverse d' -> d'
        VincentyAntipodal -> tooFar
        VincentyAbnormal _ -> tooFar

distanceVincenty'
    :: (Real a, Fractional a)
    => Epsilon
    -> Ellipsoid a
    -> LatLng a [u| rad |]
    -> LatLng a [u| rad |]
    -> VincentyInverse (TaskDistance a)
distanceVincenty' e ellipsoid
    x@(LatLng (xLat, xLng))
    y@(LatLng (yLat, yLng))

    | x == y = VincentyInverse $ TaskDistance [u| 0 m |]

    | xLat < minBound = VincentyAbnormal LatUnder
    | xLat > maxBound = VincentyAbnormal LatOver
    | xLng < minBound = VincentyAbnormal LngUnder
    | xLng > maxBound = VincentyAbnormal LngOver

    | yLat < minBound = VincentyAbnormal LatUnder
    | yLat > maxBound = VincentyAbnormal LatOver
    | yLng < minBound = VincentyAbnormal LngUnder
    | yLng > maxBound = VincentyAbnormal LngOver

    | otherwise =
        case vincentyInverse e ellipsoidR accuracy (llR x) (llR y) of
            VincentyInverse d' ->
                VincentyInverse . TaskDistance . fromRational' . MkQuantity $ d'
                
            VincentyAntipodal -> VincentyAntipodal
            VincentyAbnormal ab -> VincentyAbnormal ab
        where
            ellipsoidR = toRationalEllipsoid ellipsoid
            llR = toRationalLatLng
            accuracy = defaultVincentyAccuracy
