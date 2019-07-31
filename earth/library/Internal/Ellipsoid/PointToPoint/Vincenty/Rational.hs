{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Internal.Ellipsoid.PointToPoint.Vincenty.Rational
    ( distance
    , inverse
    , azimuthFwd
    , azimuthRev
    ) where

import qualified Data.Number.FixedFunctions as F
import Data.UnitsOfMeasure (u, convert, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (Lat(..), Lng(..), LatLng(..), AzimuthFwd, AzimuthRev)
import Flight.LatLng.Rational (Epsilon(..))
import Flight.Zone (Radius(..), toRationalLatLng)
import Flight.Distance (QTaskDistance, TaskDistance(..), SpanLatLng)
import Flight.Earth.Ellipsoid
    ( Ellipsoid(..), AbnormalLatLng(..), GeodeticInverse(..), GeodeticAccuracy(..)
    , defaultGeodeticAccuracy, flattening, polarRadius, toRationalEllipsoid, tooFar
    )
import Flight.Geodesy (InverseProblem(..), InverseSolution(..))
import Flight.Earth.Math (normalizeLngR, atan2')

inverse
    :: Ellipsoid Rational
    -> Epsilon
    -> GeodeticAccuracy Rational
    -> InverseProblem (LatLng Rational [u| rad |])
    -> GeodeticInverse
        (InverseSolution
            (QTaskDistance Rational [u| m |])
            (Quantity Rational [u| rad |])
        )
inverse
    ellipsoid@Ellipsoid{equatorialR = Radius (MkQuantity a)}
    e@(Epsilon eps)
    (GeodeticAccuracy tolerance)
    InverseProblem
        { x = LatLng (Lat (MkQuantity _Φ₁), Lng (MkQuantity _L₁))
        , y = LatLng (Lat (MkQuantity _Φ₂), Lng (MkQuantity _L₂))
        } =
    loop _L
    where
        MkQuantity b = polarRadius ellipsoid

        sin' = F.sin eps
        cos' = F.cos eps
        tan' = F.tan eps
        atan' = F.atan eps
        normalizeLng' = normalizeLngR e

        f = flattening ellipsoid
        auxLat = atan' . ((1 - f) *) . tan'
        _U₁ = auxLat _Φ₁; _U₂ = auxLat _Φ₂
        _L =
            case _L₂ - _L₁ of
                _L' | abs _L' <= F.pi eps -> _L'
                _ -> normalizeLng' _L₂ - normalizeLng' _L₁

        sinU₁ = sin' _U₁; sinU₂ = sin' _U₂
        cosU₁ = cos' _U₁; cosU₂ = cos' _U₂
        sinU₁sinU₂ = sinU₁ * sinU₂
        cosU₁cosU₂ = cosU₁ * cosU₂

        loop λ
            | abs λ > F.pi eps = GeodeticInverseAntipodal
            | abs (λ - λ') >= tolerance = loop λ'
            | otherwise =
                GeodeticInverse $
                InverseSolution
                    { s = TaskDistance . MkQuantity $ b * _A * (σ - _Δσ)
                    , α₁ = MkQuantity $ atan2' e i j
                    , α₂ = Just . MkQuantity $ atan2' e i' j'
                    }
            where
                sinλ = sin' λ
                cosλ = cos' λ

                i' = cosU₁ * sinλ
                j' = -sinU₁ * cosU₂ + cosU₁ * sinU₂ * cosλ

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

-- | Spherical distance using inverse Vincenty and rational numbers.
distance
    :: (Real a, Fractional a)
    => Ellipsoid a
    -> Epsilon
    -> SpanLatLng a
distance e eps x y =
    case distance' e eps (InverseProblem x y) of
        GeodeticInverseAntipodal -> tooFar
        GeodeticInverseAbnormal _ -> tooFar
        GeodeticInverse InverseSolution{s} -> s

azimuthFwd
    :: (Real a, Fractional a)
    => Ellipsoid a
    -> Epsilon
    -> AzimuthFwd a
azimuthFwd e eps x y =
    case distance' e eps (InverseProblem x y) of
        GeodeticInverseAntipodal -> Nothing
        GeodeticInverseAbnormal _ -> Nothing
        GeodeticInverse InverseSolution{α₁} -> Just α₁

azimuthRev
    :: (Real a, Fractional a)
    => Ellipsoid a
    -> Epsilon
    -> AzimuthRev a
azimuthRev e eps x y =
    case distance' e eps (InverseProblem x y) of
        GeodeticInverseAntipodal -> Nothing
        GeodeticInverseAbnormal _ -> Nothing
        GeodeticInverse InverseSolution{α₂} -> α₂

distance'
    :: (Real a, Fractional a)
    => Ellipsoid a
    -> Epsilon
    -> InverseProblem (LatLng a [u| rad |])
    -> GeodeticInverse
        (InverseSolution (QTaskDistance a [u| m |]) (Quantity a [u| rad |]))
distance' e eps
    InverseProblem
        { x = x@(LatLng (xLat, xLng))
        , y = y@(LatLng (yLat, yLng))
        }

    | x == y =
        GeodeticInverse $
            InverseSolution
                { s = TaskDistance [u| 0 m |]
                , α₁ = convert [u| 0 deg |]
                , α₂ = Just $ convert [u| 180 deg |]
                }

    | xLat < minBound = GeodeticInverseAbnormal LatUnder
    | xLat > maxBound = GeodeticInverseAbnormal LatOver
    | xLng < minBound = GeodeticInverseAbnormal LngUnder
    | xLng > maxBound = GeodeticInverseAbnormal LngOver

    | yLat < minBound = GeodeticInverseAbnormal LatUnder
    | yLat > maxBound = GeodeticInverseAbnormal LatOver
    | yLng < minBound = GeodeticInverseAbnormal LngUnder
    | yLng > maxBound = GeodeticInverseAbnormal LngOver

    | otherwise =
        case inverse eR eps accuracy probR of
            GeodeticInverseAntipodal -> GeodeticInverseAntipodal
            GeodeticInverseAbnormal ab -> GeodeticInverseAbnormal ab
            GeodeticInverse
                InverseSolution
                    {s = TaskDistance s', α₁, α₂} ->
                GeodeticInverse $
                    InverseSolution
                        { s = TaskDistance . fromRational' $ s'
                        , α₁ = fromRational' α₁
                        , α₂ = fromRational' <$> α₂
                        }
        where
            eR = toRationalEllipsoid e
            llR = toRationalLatLng
            accuracy = defaultGeodeticAccuracy

            probR :: InverseProblem (LatLng Rational [u| rad |])
            probR =
                InverseProblem
                    { x = llR x
                    , y = llR y
                    }
