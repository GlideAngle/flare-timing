{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Earth.Ellipsoid.PointToPoint.Double
    ( distanceVincenty
    , azimuthFwd
    , azimuthRev
    , vincentyInverse
    ) where

import Data.Fixed (mod')
import Data.UnitsOfMeasure (KnownUnit, Unpack, u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng
    (QLat, Lat(..), QLng, Lng(..), LatLng(..), AzimuthFwd, AzimuthRev)
import Flight.Distance (QTaskDistance, TaskDistance(..), SpanLatLng)
import Flight.Earth.Ellipsoid
    ( Ellipsoid(..), AbnormalLatLng(..), VincentyInverse(..), VincentyAccuracy(..)
    , defaultVincentyAccuracy, flattening, polarRadius
    )
import Flight.Earth.Geodesy (InverseProblem(..), InverseSolution(..))

-- | In Vincenty's paper he says,
--
-- "The inverse formula may give no solution over a line between
-- two nearly antipodal points. This will occur when λ, as computed
-- by eqn. (11), is greater than π in absolute value."
--
-- (45°,-179°59'58.17367'') to (45°,180°)
-- Comparing the above two points, longitudes are less than a minute apart.  To
-- be able to get the difference using simple subtraction normalize the
-- longitudes to a range 0 <= lng <= 2π.
normalizeLng :: (Floating a, Real a) => a -> a
normalizeLng lng =
    lng `mod'` (2 * pi)

vincentyInverse
    :: (Num a, Floating a, Fractional a, RealFloat a, Show a)
    => Ellipsoid a
    -> VincentyAccuracy a
    -> InverseProblem (LatLng a [u| rad |])
    -> VincentyInverse
        (InverseSolution
            (QTaskDistance a [u| m |])
            (Quantity a [u| rad |])
        )
vincentyInverse
    ellipsoid@Ellipsoid{equatorialR = MkQuantity a}
    (VincentyAccuracy tolerance)
    InverseProblem
        { x = LatLng (Lat (MkQuantity _Φ₁), Lng (MkQuantity _L₁))
        , y = LatLng (Lat (MkQuantity _Φ₂), Lng (MkQuantity _L₂))
        } =
    loop _L
    where
        MkQuantity b = polarRadius ellipsoid
        f = flattening ellipsoid

        auxLat = atan . ((1 - f) *) . tan
        _U₁ = auxLat _Φ₁; _U₂ = auxLat _Φ₂
        _L =
            case _L₂ - _L₁ of
                _L' | abs _L' <= pi -> _L'
                _ -> normalizeLng _L₂ - normalizeLng _L₁

        sinU₁ = sin _U₁; sinU₂ = sin _U₂
        cosU₁ = cos _U₁; cosU₂ = cos _U₂
        sinU₁sinU₂ = sinU₁ * sinU₂
        cosU₁cosU₂ = cosU₁ * cosU₂

        loop λ
            | abs λ > pi = VincentyInverseAntipodal
            | abs (λ - λ') >= tolerance = loop λ'
            | otherwise =
                VincentyInverse $
                InverseSolution
                    { s = TaskDistance . MkQuantity $ b * _A * (σ - _Δσ)
                    , α₁ = MkQuantity $ atan2 i j
                    , α₂ = Just . MkQuantity $ atan2 i' j'
                    } 
            where
                sinλ = sin λ
                cosλ = cos λ

                i' = cosU₁ * sinλ
                j' = -sinU₁ * cosU₂ + cosU₁ * sinU₂ * cosλ

                i = cosU₂ * sinλ
                j = cosU₁ * sinU₂ - sinU₁ * cosU₂ * cosλ

                sin²σ = i * i + j * j
                sinσ = sqrt sin²σ
                cosσ = sinU₁sinU₂ + cosU₁cosU₂ * cosλ

                σ = atan2 sinσ cosσ

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


tooFar :: Num a => QTaskDistance a [u| m |]
tooFar = TaskDistance [u| 20000000 m |]

-- | Sperical distance using inverse Vincenty and floating point numbers.
distanceVincenty
    :: ( RealFloat a, Show a
       , KnownUnit (Unpack u), Show (QLat a u), Show (QLng a u)
       , u ~ [u| rad |]
       )
    => Ellipsoid a
    -> SpanLatLng a
distanceVincenty e x y =
    case distanceVincenty' e (InverseProblem x y) of
        VincentyInverseAntipodal -> tooFar
        VincentyInverseAbnormal _ -> tooFar
        VincentyInverse InverseSolution{s} -> s

azimuthFwd :: (RealFloat a, Show a) => Ellipsoid a -> AzimuthFwd a
azimuthFwd e x y =
    case distanceVincenty' e (InverseProblem x y) of
        VincentyInverseAntipodal -> Nothing
        VincentyInverseAbnormal _ -> Nothing
        VincentyInverse InverseSolution{α₁} -> Just α₁

azimuthRev :: (RealFloat a, Show a) => Ellipsoid a -> AzimuthRev a
azimuthRev e x y =
    case distanceVincenty' e (InverseProblem x y) of
        VincentyInverseAntipodal -> Nothing
        VincentyInverseAbnormal _ -> Nothing
        VincentyInverse InverseSolution{α₂} -> α₂

distanceVincenty'
    :: (RealFloat a, Show a)
    => Ellipsoid a
    -> InverseProblem (LatLng a [u| rad |])
    -> VincentyInverse
        (InverseSolution (QTaskDistance a [u| m |]) (Quantity a [u| rad |]))
distanceVincenty'
    ellipsoid
    prob@InverseProblem
        { x = x@(LatLng (xLat, xLng))
        , y = y@(LatLng (yLat, yLng))
        }

    | x == y =
        VincentyInverse $
            InverseSolution
                { s = TaskDistance [u| 0 m |]
                , α₁ = convert [u| 0 deg |]
                , α₂ = Just $ convert [u| 180 deg |]
                }

    | xLat < minBound = VincentyInverseAbnormal LatUnder
    | xLat > maxBound = VincentyInverseAbnormal LatOver
    | xLng < minBound = VincentyInverseAbnormal LngUnder
    | xLng > maxBound = VincentyInverseAbnormal LngOver

    | yLat < minBound = VincentyInverseAbnormal LatUnder
    | yLat > maxBound = VincentyInverseAbnormal LatOver
    | yLng < minBound = VincentyInverseAbnormal LngUnder
    | yLng > maxBound = VincentyInverseAbnormal LngOver

    | otherwise =
        vincentyInverse ellipsoid accuracy' prob
        where
            VincentyAccuracy accuracy = defaultVincentyAccuracy
            accuracy' = VincentyAccuracy $ fromRational accuracy
