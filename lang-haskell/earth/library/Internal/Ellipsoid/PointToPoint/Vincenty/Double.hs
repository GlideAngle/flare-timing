{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Internal.Ellipsoid.PointToPoint.Vincenty.Double
    ( distance
    , inverse
    , azimuthFwd
    , azimuthRev
    ) where

import Text.Printf (printf)
import Data.Maybe (fromMaybe)
import Data.UnitsOfMeasure (u, convert, fromRational', toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units.DegMinSec (DMS(..))
import Flight.Units.Angle (Angle(..))
import Flight.LatLng (Lat(..), Lng(..), LatLng(..), AzimuthFwd, AzimuthRev)
import Flight.Distance (QTaskDistance, TaskDistance(..), SpanLatLng)
import Flight.Zone (Radius(..), fromRationalLatLng, toRationalLatLng)
import Flight.Earth.Ellipsoid
    ( Ellipsoid(..), GeodeticInverse(..), GeodeticAccuracy(..)
    , defaultGeodeticAccuracy, flattening, polarRadius
    )
import Flight.Geodesy (InverseProblem(..), InverseSolution(..))
import Flight.Earth.Math (normalizeLng)

inverse
    :: (Num a, Fractional a, RealFloat a)
    => Ellipsoid a
    -> GeodeticAccuracy a
    -> InverseProblem (LatLng a [u| rad |])
    -> GeodeticInverse
        (InverseSolution
            (QTaskDistance a [u| m |])
            (Quantity a [u| rad |])
        )
inverse
    ellipsoid@Ellipsoid{equatorialR = Radius (MkQuantity a)}
    (GeodeticAccuracy tolerance)
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
            | abs λ > pi = GeodeticInverseAntipodal
            | abs (λ - λ') >= tolerance = loop λ'
            | otherwise =
                GeodeticInverse $
                InverseSolution
                    { s = TaskDistance . MkQuantity $ b * _A * (σ - _Δσ)
                    , α₁ = MkQuantity $ atan2 i j
                    , α₂ = Just . MkQuantity $ atan2 i' j'
                    }
            where
                sinλ = sin λ
                cosλ = cos λ

                -- WARNING: The sign of numerator and denominator are important
                -- in atan2. The sign of each term below follows Vincenty's
                -- 1975 paper "Direct and Inverse Solutions of Geodesics on the
                -- Ellipsoid with Application of Nested Equations"
                --
                -- i' = cosU₁ * sinλ
                -- j' = -sinU₁ * cosU₂ + cosU₁ * sinU₂ * cosλ
                --
                -- By contrast Delorme's 1978 paper "Evaluation Direct and
                -- Inverse Geodetic Algorithms" has this formulation with the
                -- signs reversed.
                --
                -- i' = -cosU₁ * sinλ
                -- j' = sinU₁ * cosU₂ - cosU₁ * sinU₂ * cosλ
                --
                -- As the method is Vincenty's I'm going their signage.
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

{-# SPECIALIZE
   inverse
    :: Ellipsoid Double
    -> GeodeticAccuracy Double
    -> InverseProblem (LatLng Double [u| rad |])
    -> GeodeticInverse
        (InverseSolution
            (QTaskDistance Double [u| m |])
            (Quantity Double [u| rad |])
        ) #-}

tooFar :: Num a => QTaskDistance a [u| m |]
tooFar = TaskDistance [u| 20000000 m |]

-- | Spherical distance using inverse Vincenty and floating point numbers.
--
-- >>> :{
--     distance
--         wgs84
--         (LatLng (Lat $ convert [u| -45.0 deg |], Lng $ convert [u| 180.0 deg |]))
--         (LatLng (Lat $ convert [u| -44.99999999886946 deg |], Lng $ convert [u| 180.00000000000762 deg |]))
-- :}
-- [u| 1.2564e-7 km |]
--
-- [u| 0.0 km |]
--
-- >>> :{
--     distance
--         wgs84
--         (fromDMS (DMS (-45, 0, 0), DMS (180, 0, 0)))
--         (fromDMS (DMS (-44, 59, 59), DMS (180, 0, 0)))
-- :}
-- [u| 3.0869937417e-2 km |]
--
-- [u| 3.087e-2 km |]
--
-- >>> :{
--     distance
--         wgs84
--         (fromDMS (DMS (-45, 0, 0), DMS (180, 0, 0)))
--         (fromDMS (DMS (-44, 59, 59), DMS (180, 0, 1)))
-- :}
-- [u| 3.7850343635e-2 km |]
--
-- [u| 3.785e-2 km |]
--
-- >>> :{
--     distance
--         wgs84
--         (LatLng (Lat [u| 0.0 rad |], Lng [u| 0.0 rad |]))
--         (LatLng (Lat [u| 0.0 rad |], Lng $ convert [u| 100.0 deg |]))
-- :}
-- [u| 11131.94907932264 km |]
--
-- [u| 11131.949079 km |]
--
-- >>> :{
--     distance
--         wgs84
--         (LatLng (Lat [u| 0.0 rad |], Lng [u| 0.0 rad |]))
--         (LatLng (Lat [u| 0.0 rad |], Lng $ convert [u| 10.0 deg |]))
-- :}
-- [u| 1113.194907932264 km |]
--
-- [u| 1113.194908 km |]
--
-- >>> :{
--     distance
--         wgs84
--         (LatLng (Lat [u| 0.0 rad |], Lng [u| 0.0 rad |]))
--         (LatLng (Lat [u| 0.0 rad |], Lng $ convert [u| 1.0 deg |]))
-- :}
-- [u| 111.319490793226 km |]
--
-- [u| 111.319491 km |]
--
-- >>> :{
--     distance
--         wgs84
--         (LatLng (Lat [u| 0.0 rad |], Lng [u| 0.0 rad |]))
--         (LatLng (Lat [u| 0.0 rad |], Lng $ convert [u| 0.1 deg |]))
-- :}
-- [u| 11.131949077921 km |]
--
-- [u| 11.131949 km |]
--
-- >>> :{
--     distance
--         wgs84
--         (LatLng (Lat [u| 0.0 rad |], Lng [u| 0.0 rad |]))
--         (LatLng (Lat [u| 0.0 rad |], Lng $ convert [u| 0.01 deg |]))
-- :}
-- [u| 1.113194907792 km |]
--
-- [u| 1.113195 km |]
--
-- >>> :{
--     distance
--         wgs84
--         (LatLng (Lat [u| 0.0 rad |], Lng [u| 0.0 rad |]))
--         (LatLng (Lat [u| 0.0 rad |], Lng $ convert [u| 0.001 deg |]))
-- :}
-- [u| 0.111319486598 km |]
--
-- [u| 0.111319 km |]
--
-- >>> :{
--     distance
--         wgs84
--         (LatLng (Lat [u| 0.0 rad |], Lng [u| 0.0 rad |]))
--         (LatLng (Lat [u| 0.0 rad |], Lng $ convert [u| 0.0001 deg |]))
-- :}
-- [u| 1.113194866e-2 km |]
--
-- [u| 1.1132e-2 km |]
--
-- >>> :{
--     distance
--         wgs84
--         (LatLng (Lat [u| 0.0 rad |], Lng [u| 0.0 rad |]))
--         (LatLng (Lat [u| 0.0 rad |], Lng $ convert [u| 0.00001 deg |]))
-- :}
-- [u| 1.113194866e-3 km |]
-- 
-- [u| 1.113e-3 km |]
--
-- >>> :{
--     distance
--         wgs84
--         (LatLng (Lat [u| 0.0 rad |], Lng [u| 0.0 rad |]))
--         (LatLng (Lat [u| 0.0 rad |], Lng $ convert [u| 0.000001 deg |]))
-- :}
-- [u| 1.11318239e-4 km |]
--
-- [u| 1.11e-4 km |]
--
-- >>> :{
--     distance
--         wgs84
--         (LatLng (Lat [u| 0.0 rad |], Lng [u| 0.0 rad |]))
--         (LatLng (Lat [u| 0.0 rad |], Lng $ convert [u| 0.0000001 deg |]))
-- :}
-- [u| 1.1131824e-5 km |]
--
-- [u| 1.1e-5 km |]
--
-- >>> :{
--     distance
--         wgs84
--         (LatLng (Lat [u| 0.0 rad |], Lng [u| 0.0 rad |]))
--         (LatLng (Lat [u| 0.0 rad |], Lng $ convert [u| 0.00000001 deg |]))
-- :}
-- [u| 1.109463e-6 km |]
--
-- [u| 1.0e-6 km |]
--
-- >>> :{
--     distance
--         wgs84
--         (LatLng (Lat [u| 0.0 rad |], Lng [u| 0.0 rad |]))
--         (LatLng (Lat [u| 0.0 rad |], Lng $ convert [u| 0.000000001 deg |]))
-- :}
-- [u| 1.10946e-7 km |]
--
-- [u| 0.0 km |]
distance :: RealFloat a => Ellipsoid a -> SpanLatLng a
distance
    e
    x@(LatLng (Lat qx, _))
    y@(LatLng (Lat qy, _)) =
    fromMaybe (error msg) $ do
        let LatLng (Lat xLat, Lng xLng) = toRationalLatLng x
        let LatLng (Lat yLat, Lng yLng) = toRationalLatLng y

        xLat' <- plusMinusHalfPi xLat
        yLat' <- plusMinusHalfPi yLat

        let xLng' = plusMinusPi xLng
        let yLng' = plusMinusPi yLng

        let x' = fromRationalLatLng $ LatLng (Lat xLat', Lng xLng')
        let y' = fromRationalLatLng $ LatLng (Lat yLat', Lng yLng')

        return $
            case distanceUnchecked e (InverseProblem x' y') of
                GeodeticInverseAntipodal -> tooFar
                GeodeticInverseAbnormal _ -> tooFar
                GeodeticInverse InverseSolution{s} -> s
    where
        toDMS :: Quantity _ [u| rad |] -> DMS
        toDMS = fromQuantity . fromRational' . toRational'

        msg =
            printf
                "Latitude of %s or %s is outside -90° .. 90° range"
                (show $ toDMS qx)
                (show $ toDMS qy)
{-# SPECIALIZE distance :: Ellipsoid Double -> SpanLatLng Double #-}

azimuthFwd :: RealFloat a => Ellipsoid a -> AzimuthFwd a
azimuthFwd e x y =
    case distanceUnchecked e (InverseProblem x y) of
        GeodeticInverseAntipodal -> Nothing
        GeodeticInverseAbnormal _ -> Nothing
        GeodeticInverse InverseSolution{α₁} -> Just α₁
{-# SPECIALIZE azimuthFwd :: Ellipsoid Double -> AzimuthFwd Double #-}

azimuthRev :: RealFloat a => Ellipsoid a -> AzimuthRev a
azimuthRev e x y =
    case distanceUnchecked e (InverseProblem x y) of
        GeodeticInverseAntipodal -> Nothing
        GeodeticInverseAbnormal _ -> Nothing
        GeodeticInverse InverseSolution{α₂} -> α₂
{-# SPECIALIZE azimuthRev :: Ellipsoid Double -> AzimuthRev Double #-}

distanceUnchecked
    :: RealFloat a
    => Ellipsoid a
    -> InverseProblem (LatLng a [u| rad |])
    -> GeodeticInverse
        (InverseSolution (QTaskDistance a [u| m |]) (Quantity a [u| rad |]))
distanceUnchecked
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

    | xLat < minBound = error "xlat under" -- GeodeticInverseAbnormal LatUnder
    | xLat > maxBound = error "xlat over" -- GeodeticInverseAbnormal LatOver
    | xLng < minBound = error "xlng under" -- GeodeticInverseAbnormal LngUnder
    | xLng > maxBound = error "xlng over" -- GeodeticInverseAbnormal LngOver

    | yLat < minBound = error "ylat under" -- GeodeticInverseAbnormal LatUnder
    | yLat > maxBound = error "ylat over" -- GeodeticInverseAbnormal LatOver
    | yLng < minBound = error "ylng under" -- GeodeticInverseAbnormal LngUnder
    | yLng > maxBound = error "ylng over" -- GeodeticInverseAbnormal LngOver

    | otherwise =
        inverse ellipsoid accuracy' prob
        where
            GeodeticAccuracy accuracy = defaultGeodeticAccuracy
            accuracy' = GeodeticAccuracy $ fromRational accuracy
{-# SPECIALIZE
   distanceUnchecked
    :: Ellipsoid Double
    -> InverseProblem (LatLng Double [u| rad |])
    -> GeodeticInverse
        (InverseSolution (QTaskDistance Double [u| m |]) (Quantity Double [u| rad |])) #-}

-- $setup
-- >>> import Flight.Earth.Ellipsoid
-- >>> import Flight.LatLng
-- >>> import Flight.Units.DegMinSec (DMS(..))
