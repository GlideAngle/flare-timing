{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Internal.Ellipsoid.PointToPoint.Andoyer.Rational
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
    ( Ellipsoid(..), AbnormalLatLng(..)
    , GeodeticInverse(..), GeodeticAccuracy(..)
    , Andoyer(..)
    , defaultGeodeticAccuracy, flattening, toRationalEllipsoid
    )
import Flight.Geodesy (InverseProblem(..), InverseSolution(..))
import Flight.Earth.Math (normalizeLngR, atan2')
import Internal.Ellipsoid.PointToPoint.Andoyer.Double (InverseWorking(..))

inverse
    :: Andoyer
    -> Ellipsoid Rational
    -> Epsilon
    -> GeodeticAccuracy Rational
    -> InverseProblem (LatLng Rational [u| rad |])
    -> GeodeticInverse
        (InverseSolution
            (QTaskDistance Rational [u| m |])
            (Quantity Rational [u| rad |])
        )
inverse a@FsAndoyer = inverseFs a
inverse a = inverseStuifbergen a

inverseWorking :: Ellipsoid Rational -> Epsilon -> InverseProblem (LatLng Rational u) -> InverseWorking Rational
inverseWorking
    ellipsoid
    e@(Epsilon eps)
    InverseProblem
        { x = LatLng (Lat (MkQuantity _Φ₁), Lng (MkQuantity _L₁))
        , y = LatLng (Lat (MkQuantity _Φ₂), Lng (MkQuantity _L₂))
        } =
        InverseWorking f sin' tan' d sind cosd sinΦ₁ sinΦ₂ sinU₁ sinU₂ i j i' j'
    where
        f = flattening ellipsoid

        sin' = F.sin eps
        cos' = F.cos eps
        acos' = F.acos eps
        tan' = F.tan eps
        atan' = F.atan eps
        normalizeLng' = normalizeLngR e

        auxLat = atan' . ((1 - f) *) . tan'
        _U₁ = auxLat _Φ₁; _U₂ = auxLat _Φ₂

        λ =
            case _L₂ - _L₁ of
                _L' | abs _L' <= F.pi eps -> _L'
                _ -> normalizeLng' _L₂ - normalizeLng' _L₁

        sinΦ₁ = sin' _Φ₁; sinΦ₂ = sin' _Φ₂

        sinU₁ = sin' _U₁; sinU₂ = sin' _U₂
        cosU₁ = cos' _U₁; cosU₂ = cos' _U₂

        sinU₁sinU₂ = sinU₁ * sinU₂
        cosU₁cosU₂ = cosU₁ * cosU₂

        sinλ = sin' λ
        cosλ = cos' λ

        i' = cosU₁ * sinλ
        j' = -sinU₁ * cosU₂ + cosU₁ * sinU₂ * cosλ

        i = cosU₂ * sinλ
        j = cosU₁ * sinU₂ - sinU₁ * cosU₂ * cosλ

        cosd = sinU₁sinU₂ + cosU₁cosU₂ * cosλ
        d = acos' cosd
        sind = sin' d

-- | The inverse solution of Andoyer-Lambert using the same formulae as FS.
inverseFs
    :: Andoyer
    -> Ellipsoid Rational
    -> Epsilon
    -> GeodeticAccuracy Rational
    -> InverseProblem (LatLng Rational [u| rad |])
    -> GeodeticInverse
        (InverseSolution
            (QTaskDistance Rational [u| m |])
            (Quantity Rational [u| rad |])
        )
inverseFs
    andoyer
    ellipsoid@Ellipsoid{equatorialR = Radius (MkQuantity a)}
    e
    _
    prob@InverseProblem
        { x = LatLng (Lat (MkQuantity _Φ₁), Lng (MkQuantity _L₁))
        , y = LatLng (Lat (MkQuantity _Φ₂), Lng (MkQuantity _L₂))
        } =
    GeodeticInverse $
        InverseSolution
            { s =
                TaskDistance . MkQuantity $
                    case andoyer of
                         FsAndoyer -> a * (d + f * d₁)
                         _ -> error "FsAndoyer expected."

            , α₁ = MkQuantity $ atan2' e i j
            , α₂ = Just . MkQuantity $ atan2' e i' j'
            }
    where
        InverseWorking{..} = inverseWorking ellipsoid e prob

        -- NOTE: This is the same Andoyer correction as used in FS.
        _K = let x = sinU₁ - sinU₂ in x * x
        _L = let x = sinU₁ + sinU₂ in x * x
        _3sind = 3 * sind

        _1minuscosd = 1 - cosd
        _1pluscosd = 1 + cosd

        _H = if _1minuscosd == 0 then 0 else (d + _3sind) / _1minuscosd
        _G = if _1pluscosd == 0 then 0 else (d - _3sind) / _1pluscosd
        d₁ = -(f / 4) * (_H * _K + _G * _L)

inverseStuifbergen
    :: Andoyer
    -> Ellipsoid Rational
    -> Epsilon
    -> GeodeticAccuracy Rational
    -> InverseProblem (LatLng Rational [u| rad |])
    -> GeodeticInverse
        (InverseSolution
            (QTaskDistance Rational [u| m |])
            (Quantity Rational [u| rad |])
        )
inverseStuifbergen
    andoyer
    ellipsoid@Ellipsoid{equatorialR = Radius (MkQuantity a)}
    e
    _
    prob@InverseProblem
        { x = LatLng (Lat (MkQuantity _Φ₁), Lng (MkQuantity _L₁))
        , y = LatLng (Lat (MkQuantity _Φ₂), Lng (MkQuantity _L₂))
        } =
    GeodeticInverse $
        InverseSolution
            { s =
                TaskDistance . MkQuantity $
                    case andoyer of
                         AndoyerLambert -> a * (d + f * d₁)
                         ForsytheAndoyerLambert -> a * (d + f * (d₁ + d₂))
                         FsAndoyer -> error "FsAndoyer not expected."

            , α₁ = MkQuantity $ atan2' e i j
            , α₂ = Just . MkQuantity $ atan2' e i' j'
            }
    where
        InverseWorking{..} = inverseWorking ellipsoid e prob

        sin2d = sin' $ 2 * d
        tand = tan' d

        _P =
            case 1 + cosd of
                0 -> 0
                denom -> let ss = sinU₁ + sinU₂ in ss * ss / denom

        _Q =
            case 1 - cosd of
                0 -> 0
                denom -> let ss = sinU₁ - sinU₂ in ss * ss / denom

        _X = _P + _Q
        _Y = _P - _Q

        d₁ = -(_X * d - 3 * _Y * sind) / 4

        d₂ =
            if sind == 0 || tand == 0 then 0 else
            let _A = 64 * d + 16 * d * d / tand
                _B = - 2 * _D
                _C = -(30 * d + 8 * d * d / tand + _E / 2)
                _D = 48 * sind + 8 * d * d / sind
                _E = 30 * sin2d

            in f * (_A * _X + _B * _Y + _C * _X * _X + _D * _X * _Y + _E * _Y * _Y) / 128

tooFar :: Num a => QTaskDistance a [u| m |]
tooFar = TaskDistance [u| 20000000 m |]

-- | Spherical distance using inverse Vincenty and rational numbers.
distance
    :: (Real a, Fractional a)
    => Andoyer
    -> Ellipsoid a
    -> Epsilon
    -> SpanLatLng a
distance a e eps x y =
    case distance' a e eps (InverseProblem x y) of
        GeodeticInverseAntipodal -> tooFar
        GeodeticInverseAbnormal _ -> tooFar
        GeodeticInverse InverseSolution{s} -> s

azimuthFwd
    :: (Real a, Fractional a)
    => Andoyer
    -> Ellipsoid a
    -> Epsilon
    -> AzimuthFwd a
azimuthFwd a e eps x y =
    case distance' a e eps (InverseProblem x y) of
        GeodeticInverseAntipodal -> Nothing
        GeodeticInverseAbnormal _ -> Nothing
        GeodeticInverse InverseSolution{α₁} -> Just α₁

azimuthRev
    :: (Real a, Fractional a)
    => Andoyer
    -> Ellipsoid a
    -> Epsilon
    -> AzimuthRev a
azimuthRev a e eps x y =
    case distance' a e eps (InverseProblem x y) of
        GeodeticInverseAntipodal -> Nothing
        GeodeticInverseAbnormal _ -> Nothing
        GeodeticInverse InverseSolution{α₂} -> α₂

distance'
    :: (Real a, Fractional a)
    => Andoyer
    -> Ellipsoid a
    -> Epsilon
    -> InverseProblem (LatLng a [u| rad |])
    -> GeodeticInverse
        (InverseSolution (QTaskDistance a [u| m |]) (Quantity a [u| rad |]))
distance' a e eps
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
        case inverse a eR eps accuracy probR of
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
