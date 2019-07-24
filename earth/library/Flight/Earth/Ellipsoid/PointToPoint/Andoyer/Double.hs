{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Earth.Ellipsoid.PointToPoint.Andoyer.Double
    ( distance
    , inverse
    , azimuthFwd
    , azimuthRev
    ) where

import Data.UnitsOfMeasure (KnownUnit, Unpack, u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng
    (QLat, Lat(..), QLng, Lng(..), LatLng(..), AzimuthFwd, AzimuthRev)
import Flight.Distance (QTaskDistance, TaskDistance(..), SpanLatLng)
import Flight.Zone (Radius(..))
import Flight.Earth.Ellipsoid
    ( Ellipsoid(..), AbnormalLatLng(..)
    , GeodeticInverse(..), GeodeticAccuracy(..)
    , Andoyer(..)
    , defaultGeodeticAccuracy, flattening, tooFar
    )
import Flight.Earth.Geodesy (InverseProblem(..), InverseSolution(..))
import Flight.Earth.Math (normalizeLng)

-- Intersection of Hyperbolae on the Earth
-- by N. Stuifbergen, Dec 1980, Tech. Report #77
-- UNB, Geodesy and Geomatics Engineering
-- SEE: http://www2.unb.ca/gge/Pubs/TR77.pdf
-- Section 3.1 Forsythe-Andoyer-Lamber Formulae
-- The Andoyer-Lambert method consists of calculating a spherical arc length on
-- an auxillary sphere of radius a, the ellipsoid major axis semi-diameter, and
-- applying correction terms to find the distance correpsondingg to the
-- ellipsoidal arc.

inverse
    :: (Num a, Floating a, Fractional a, RealFloat a, Show a)
    => Andoyer
    -> Ellipsoid a
    -> GeodeticAccuracy a
    -> InverseProblem (LatLng a [u| rad |])
    -> GeodeticInverse
        (InverseSolution
            (QTaskDistance a [u| m |])
            (Quantity a [u| rad |])
        )
inverse
    andoyer
    ellipsoid@Ellipsoid{equatorialR = Radius (MkQuantity a)}
    _
    InverseProblem
        { x = LatLng (Lat (MkQuantity _Φ₁), Lng (MkQuantity _L₁))
        , y = LatLng (Lat (MkQuantity _Φ₂), Lng (MkQuantity _L₂))
        } =
    GeodeticInverse $
        InverseSolution
            { s =
                TaskDistance . MkQuantity $
                    case andoyer of
                         AndoyerLambert -> a * (d + f * d₁)
                         ForsytheAndoyerLambert -> a * (d + f * _Δd)

            , α₁ = MkQuantity $ atan2 i j
            , α₂ = Just . MkQuantity $ atan2 i' j'
            }
    where
        f = flattening ellipsoid

        auxLat = atan . ((1 - f) *) . tan
        _U₁ = auxLat _Φ₁; _U₂ = auxLat _Φ₂

        λ =
            case _L₂ - _L₁ of
                _L' | abs _L' <= pi -> _L'
                _ -> normalizeLng _L₂ - normalizeLng _L₁

        sinU₁ = sin _U₁; sinU₂ = sin _U₂
        cosU₁ = cos _U₁; cosU₂ = cos _U₂

        sinU₁sinU₂ = sinU₁ * sinU₂
        cosU₁cosU₂ = cosU₁ * cosU₂

        sinλ = sin λ
        cosλ = cos λ

        i' = cosU₁ * sinλ
        j' = -sinU₁ * cosU₂ + cosU₁ * sinU₂ * cosλ

        i = cosU₂ * sinλ
        j = cosU₁ * sinU₂ - sinU₁ * cosU₂ * cosλ

        cosd = sinU₁sinU₂ + cosU₁cosU₂ * cosλ
        d = acos cosd
        sind = sin d
        sin2d = sin $ 2 * d
        tand = tan d

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

        _A = 64 * d + 16 * d * d / tand
        _B = - 2 * _D
        _C = -(30 * d + 8 * d * d / tand + _E / 2)
        _D = 48 * sind + 8 * d * d / sind
        _E = 30 * sin2d

        d₂ = f * (_A * _X + _B * _Y + _C * _X * _X + _D * _X * _Y + _E * _Y * _Y) / 128

        _Δd = d₁ + d₂

-- | Spherical distance using inverse Andoyer and floating point numbers.
distance
    :: ( RealFloat a, Show a
       , KnownUnit (Unpack u), Show (QLat a u), Show (QLng a u)
       , u ~ [u| rad |]
       )
    => Andoyer
    -> Ellipsoid a
    -> SpanLatLng a
distance a e x y =
    case distance' a e (InverseProblem x y) of
        GeodeticInverseAntipodal -> tooFar
        GeodeticInverseAbnormal _ -> tooFar
        GeodeticInverse InverseSolution{s} -> s

azimuthFwd
    :: (RealFloat a, Show a)
    => Andoyer
    -> Ellipsoid a
    -> AzimuthFwd a
azimuthFwd a e x y =
    case distance' a e (InverseProblem x y) of
        GeodeticInverseAntipodal -> Nothing
        GeodeticInverseAbnormal _ -> Nothing
        GeodeticInverse InverseSolution{α₁} -> Just α₁

azimuthRev
    :: (RealFloat a, Show a)
    => Andoyer
    -> Ellipsoid a
    -> AzimuthRev a
azimuthRev a e x y =
    case distance' a e (InverseProblem x y) of
        GeodeticInverseAntipodal -> Nothing
        GeodeticInverseAbnormal _ -> Nothing
        GeodeticInverse InverseSolution{α₂} -> α₂

distance'
    :: (RealFloat a, Show a)
    => Andoyer
    -> Ellipsoid a
    -> InverseProblem (LatLng a [u| rad |])
    -> GeodeticInverse
        (InverseSolution (QTaskDistance a [u| m |]) (Quantity a [u| rad |]))
distance'
    a
    ellipsoid
    prob@InverseProblem
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
        inverse a ellipsoid accuracy' prob
        where
            GeodeticAccuracy accuracy = defaultGeodeticAccuracy
            accuracy' = GeodeticAccuracy $ fromRational accuracy
