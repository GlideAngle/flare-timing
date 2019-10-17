module Internal.Ellipsoid.Cylinder.Vincenty.Double
    ( circumSample
    , direct
    , directUnchecked
    , cos2
    ) where

import Text.Printf (printf)
import Data.Maybe (fromMaybe)
import Data.UnitsOfMeasure (u, fromRational', toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units.DegMinSec (DMS(..))
import Flight.Units.Angle (Angle(..))
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.Zone
    ( Zone(..)
    , QRadius
    , Radius(..)
    , Bearing(..)
    , ArcSweep(..)
    , center
    , radius
    , realToFracZone
    , realToFracLatLng
    , fromRationalLatLng
    , toRationalLatLng
    )
import Flight.Zone.Path (distancePointToPoint)
import Internal.Ellipsoid.PointToPoint.Double (distance)
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.Zone.Cylinder
    ( TrueCourse(..)
    , ZonePoint(..)
    , Tolerance(..)
    , SampleParams(..)
    , CircumSample
    , orbit
    , radial
    , point
    , sourceZone
    , sampleAngles
    )
import Flight.Earth.Ellipsoid
    ( Ellipsoid(..), GeodeticAccuracy(..), GeodeticDirect(..)
    , defaultGeodeticAccuracy, wgs84, flattening, polarRadius
    )
import Flight.Geodesy (EarthMath(..), DirectProblem(..), DirectSolution(..))
import Flight.Earth.ZoneShape.Double (PointOnRadial, onLine)
import Flight.Earth.Math (cos2)

iterateVincenty
    :: (Floating a, Ord a)
    => GeodeticAccuracy a -> a -> a -> a -> a -> a -> a -> a
iterateVincenty
    accuracy@(GeodeticAccuracy tolerance)
    _A
    _B
    s
    b
    σ1
    σ =
    if abs (σ - σ') < tolerance
        then σ
        else
            iterateVincenty accuracy _A _B s b σ1 σ'
    where
        (cos2σm, cos²2σm) = cos2 cos σ1 σ
        sinσ = sin σ
        cosσ = cos σ
        sin²σ = sinσ * sinσ

        _Δσ =
            _B * sinσ *
                (cos2σm + _B / 4 *
                    (cosσ * (-1 + 2 * cos²2σm)
                    - _B / 6
                    * cos2σm
                    * (-3 + 4 * sin²σ)
                    * (-3 + 4 * cos²2σm)
                    )
                )

        σ' = s / b * _A + _Δσ

-- | The solution to the direct geodesy problem with input latitude rejected
-- outside the range -90° .. 90° and longitude normalized to -180° .. 180°.
direct
    :: (Real a, Floating a, Fractional a, RealFloat a)
    => Ellipsoid a
    -> GeodeticAccuracy a
    -> DirectProblem
        (LatLng a [u| rad |])
        (TrueCourse a)
        (QRadius a [u| m |])
    -> GeodeticDirect
        (DirectSolution
            (LatLng a [u| rad |])
            (TrueCourse a)
        )
direct
    e
    a
    p@DirectProblem
        { x = x@(LatLng (Lat qLat, _))
        , α₁ = TrueCourse qTC
        } =
    fromMaybe (error msg) $ do
        let LatLng (Lat xLat, Lng xLng) = toRationalLatLng x

        nLat <- plusMinusHalfPi xLat
        let nLng = plusMinusPi xLng
        let xNorm = fromRationalLatLng $ LatLng (Lat nLat, Lng nLng)

        let nTC = normalize $ toRational' qTC
        let tcNorm = TrueCourse . fromRational' $ nTC

        return $ directUnchecked e a p{x = xNorm, α₁ = tcNorm}
    where
        dmsLat :: DMS
        dmsLat = fromQuantity . fromRational' . toRational' $ qLat

        msg = printf "Latitude of %s is outside -90° .. 90° range" $ show dmsLat

-- | The solution to the direct geodesy problem with input latitude unchecked
-- and longitude not normalized.
directUnchecked
    :: (Real a, Floating a, Fractional a, RealFloat a)
    => Ellipsoid a
    -> GeodeticAccuracy a
    -> DirectProblem
        (LatLng a [u| rad |])
        (TrueCourse a)
        (QRadius a [u| m |])
    -> GeodeticDirect
        (DirectSolution
            (LatLng a [u| rad |])
            (TrueCourse a)
        )
directUnchecked
    ellipsoid@Ellipsoid{equatorialR = Radius (MkQuantity a)}
    accuracy
    DirectProblem
        { x = LatLng (Lat (MkQuantity _Φ1), Lng (MkQuantity _L1))
        , α₁ = TrueCourse (MkQuantity α1)
        , s = (Radius (MkQuantity s))
        } =
    GeodeticDirect $
        DirectSolution
            { y = LatLng (Lat . MkQuantity $ _Φ2, Lng . MkQuantity $ _L2)
            , α₂ = Just . TrueCourse . MkQuantity $ sinα / (-j)
            }
    where
        MkQuantity b = polarRadius ellipsoid
        f = flattening ellipsoid

        -- Initial setup
        _U1 = atan $ (1 - f) * tan _Φ1
        σ1 = atan2 (tan _U1) (cos α1)
        sinα = cos _U1 * sin α1
        sin²α = sinα * sinα
        cos²α = 1 - sin²α 
        _A = 1 + u² / 16384 * (4096 + u² * (-768 + u² * (320 - 175 * u²)))
        _B = u² / 1024 * (256 + u² * (-128 + u² * (74 - 47 * u²)))

        -- Solution
        σ = iterateVincenty accuracy _A _B s b σ1 (s / (b * _A))
        sinσ = sin σ
        cosσ = cos σ
        cosα1 = cos α1
        sinU1 = sin _U1
        cosU1 = cos _U1
        v = sinU1 * cosσ + cosU1 * sinσ * cosα1
        j = sinU1 * sinσ - cosU1 * cosσ * cosα1
        w = (1 - f) * sqrt (sin²α + j * j)
        _Φ2 = atan2 v w
        λ = atan2 (sinσ * sin α1) (cosU1 * cosσ - sinU1 * sinσ * cosα1)
        _C = f / 16 * cos²α * (4 - 3 * cos²α)

        (cos2σm, cos²2σm) = cos2 cos σ1 σ
        x = σ + _C * sinσ * y
        y = cos (2 * cos2σm + _C * cosσ * (-1 + 2 * cos²2σm))
        _L = λ * (1 - _C) * f * sinα * x

        _L2 = _L + _L1

        a² = a * a
        b² = b * b
        u² = cos²α * (a² - b²) / b²

-- |
-- TODO: Why is a point 286 m out is 0.254 km away.
-- >>> circumDeg (LatLng (Lat [u| -32.46363 deg |], Lng [u| 148.989 deg |])) (Radius [u| 286.27334927563106 m |]) [u| 332.30076790172313 deg |]
-- (-32.46133783488965°, 148.9890000000845°)
--
-- >>> :{
--     V.distance
--         wgs84
--         (LatLng (Lat $ convert [u| -32.46363 deg |], Lng $ convert [u| 148.989 deg |]))
--         (LatLng (Lat $ convert [u| -32.46133783488965 deg |], Lng $ convert [u| 148.9890000000845 deg |]))
-- :}
-- [u| 0.254189500595 km |]
--
-- [u| 0.25419 km |]
--
-- TODO: Why is a point 177 m out is 0.157 km away.
-- >>> circumDeg (LatLng (Lat [u| -32.46363 deg |], Lng [u| 148.989 deg |])) (Radius [u| 177.23328234645362 m |]) [u| 152.30076790172313 deg |]
-- (-32.465049082605454°, 148.9890000000324°)
--
-- >>> :{
--     V.distance
--         wgs84
--         (LatLng (Lat $ convert [u| -32.46363 deg |], Lng $ convert [u| 148.989 deg |]))
--         (LatLng (Lat $ convert [u| -32.465049082605454 deg |], Lng $ convert [u| 148.9890000000324 deg |]))
-- :}
-- [u| 0.157369119533 km |]
-- 
-- [u| 0.157369 km |]
--
-- TODO: Why is a point 40 m out is 0 km away when bearing 90° from (-45°, 0°).
-- >>> circumDeg (LatLng (Lat [u| -45.0 deg |], Lng [u| 0.0 deg |])) (Radius [u| 40.0 m |]) [u| 90 deg |]
-- (-44.99999999886946°, 7.603632320668826e-12°)
--
-- >>> :{
--     V.distance
--         wgs84
--         (LatLng (Lat $ convert [u| -45.0 deg |], Lng $ convert [u| 0.0 deg |]))
--         (LatLng (Lat $ convert [u| -44.99999999886946 deg |], Lng $ convert [u| 7.603632320668826e-12 deg |]))
-- :}
-- [u| 1.2564e-7 km |]
-- 
-- [u| 0.0 km |]
--
-- TODO: Why is a point 40 m out is 0 km away when bearing 90° from (+45°, 0°).
-- >>> circumDeg (LatLng (Lat [u| 45.0 deg |], Lng [u| 0.0 deg |])) (Radius [u| 40.0 m |]) [u| 90 deg |]
-- (44.99999999886946°, 7.603632320668826e-12°)
--
-- >>> :{
--     V.distance
--         wgs84
--         (LatLng (Lat $ convert [u| 45.0 deg |], Lng $ convert [u| 0.0 deg |]))
--         (LatLng (Lat $ convert [u| 44.99999999886946 deg |], Lng $ convert [u| 7.603632320668826e-12 deg |]))
-- :}
-- [u| 1.2564e-7 km |]
--
-- [u| 0.0 km |]
--
-- >>> circumDeg (LatLng (Lat [u| 0.0 deg |], Lng [u| 0.0 deg |])) (Radius [u| 40.0 m |]) [u| 0 deg |]
-- (3.629662729528601e-4°, 0.0°)
--
-- >>> circumDeg (LatLng (Lat [u| 0.0 deg |], Lng [u| 0.0 deg |])) (Radius [u| 40.0 m |]) [u| 90 deg |]
-- (2.2150663706180143e-20°, 7.60643335827154e-12°)
--
-- >>> circumDeg (LatLng (Lat [u| 0.0 deg |], Lng [u| 0.0 deg |])) (Radius [u| 40.0 m |]) [u| -90 deg |]
-- (-6.645199111854043e-20°, 7.60643335827154e-12°)
--
-- >>> :{
--     V.distance
--         wgs84
--         (LatLng (Lat $ convert [u| 0.0 deg |], Lng $ convert [u| 0.0 deg |]))
--         (LatLng (Lat $ convert [u| -5.817348158845349e-20 deg |], Lng $ convert [u| 7.60643335827154e-12 deg |]))
-- :}
-- [u| 8.44e-10 km |]
-- 
-- [u| 1.0 km |]
circum
    :: (Real a, Fractional a, RealFloat a)
    => LatLng a [u| rad |]
    -> QRadius a [u| m |]
    -> TrueCourse a
    -> LatLng Double [u| rad |]
circum x r tc =
    case direct wgs84 accuracy' prob of
        GeodeticDirectAbnormal _ -> error "Geodetic direct abnormal"
        GeodeticDirectEquatorial -> error "Geodetic direct equatorial"
        GeodeticDirectAntipodal -> error "Geodetic direct antipodal"
        GeodeticDirect DirectSolution{y} -> realToFracLatLng y
    where
        GeodeticAccuracy accuracy = defaultGeodeticAccuracy
        accuracy' = GeodeticAccuracy $ fromRational accuracy

        prob =
            DirectProblem
                { x = x
                , α₁ = tc
                , s = r
                }

-- | Generates a pair of lists, the lat/lng of each generated point and its
-- distance from the center. It will generate 'samples' number of such points
-- that should lie close to the circle bounding the zone. The difference
-- between the distance to the origin and the radius should be less than the
-- 'tolerance'.
--
-- The points of the compass are divided by the number of samples requested.
circumSample :: CircumSample Double
circumSample sp@SampleParams{..} arcSweep@(ArcSweep (Bearing (MkQuantity bearing))) arc0 zoneM zoneN
    | bearing < 0 || bearing > 2 * pi = fail "Arc sweep must be in the range 0..2π radians."
    | otherwise =
        case (zoneM, zoneN) of
            (Nothing, _) -> ys
            (Just _, Point{}) -> ys
            (Just _, Vector{}) -> ys
            (Just _, Cylinder{}) -> ys
            (Just _, Conical{}) -> ys
            (Just _, Line{}) -> onLine mkLinePt θ ys
            (Just _, Circle{}) -> ys
            (Just _, SemiCircle{}) -> ys
    where
        zone' :: Zone Double
        zone' = maybe zoneN sourceZone arc0

        (θ, xs) = sampleAngles pi sp arcSweep arc0 zoneM zoneN

        r :: QRadius Double [u| m |]
        r@(Radius (MkQuantity limitRadius)) = radius zone'

        ptCenter = center zone'
        circumR = circum ptCenter

        getClose' = getClose zone' ptCenter limitRadius spTolerance

        mkLinePt :: PointOnRadial
        mkLinePt _ (Bearing b) rLine = circumR rLine $ TrueCourse b

        ys :: ([ZonePoint Double], [TrueCourse Double])
        ys = unzip $ getClose' 9 (MkQuantity 0) 0.05 (circumR r) <$> xs

getClose
    :: Zone Double
    -- The center point.
    -> LatLng Double [u| rad |]
    -- The limit radius, the radius we're trying for.
    -> Double
    -> Tolerance Double
    -- How many tries.
    -> Int
    -- An offset to the radius.
    -> Quantity Double [u| m |]
    -- A scaling.
    -> Double
    -- A function returning a point from the origin on this radial
    -> (TrueCourse Double -> LatLng Double [u| rad |])
    -- The true course for this radial.
    -> TrueCourse Double
    -> (ZonePoint Double, TrueCourse Double)
getClose zone' ptCenter limitRadius spTolerance trys (MkQuantity offset) scaling f x@(TrueCourse tc)
    | trys <= 0 = (zp', x)
    | unTolerance spTolerance <= 0 = (zp', x)
    | limitRadius <= unTolerance spTolerance = (zp', x)
    | otherwise =
        case d `compare` limitRadius of
             EQ ->
                 (zp', x)

             GT ->
                 if d < (limitRadius + unTolerance spTolerance)
                 then (zp', x)
                 else
                     -- NOTE: We aimed for (limitRadius + offset) but ended up
                     -- with an actual radius d larger than the limit radius.
                     let scaling' = scaling / 10
                         aimRadius = (limitRadius + offset) * (1 - scaling)
                         aimRadius' = Radius $ MkQuantity aimRadius
                         offset' = aimRadius - limitRadius

                         f' =
                             circumR aimRadius'

                     in
                         getClose
                             zone'
                             ptCenter
                             limitRadius
                             spTolerance
                             (trys - 1)
                             (MkQuantity offset')
                             scaling'
                             f'
                             x

             LT ->
                 if d > (limitRadius - unTolerance spTolerance)
                 then (zp', x)
                 else
                     -- NOTE: We aimed for (limitRadius + offset) but ended up
                     -- with an actual radius d smaller than the limit radius.
                     let scaling' = scaling / 10
                         aimRadius = (limitRadius + offset) * (1 + scaling)
                         aimRadius' = Radius $ MkQuantity aimRadius
                         offset' = aimRadius - limitRadius

                         f' =
                             circumR aimRadius'

                     in
                         getClose
                             zone'
                             ptCenter
                             limitRadius
                             spTolerance
                             (trys - 1)
                             (MkQuantity offset')
                             scaling'
                             f'
                             x
    where
        circumR = circum ptCenter

        y = f x

        zp' :: ZonePoint Double
        zp' = ZonePoint
                { sourceZone = realToFracZone zone'
                , point = y
                , radial = Bearing $ normalize tc
                , orbit = Radius foundRadius
                } :: ZonePoint Double

        pts = [Point ptCenter, Point y]

        (TaskDistance signedFoundRadius) =
            edgesSum
            $ distancePointToPoint
                (distance Vincenty wgs84)
                (realToFracZone <$> pts)

        foundRadius@(MkQuantity d) = signedFoundRadius

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XQuasiQuotes
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XMultiParamTypeClasses
-- >>> :set -XScopedTypeVariables
-- >>> :set -XTypeOperators
-- >>> :set -XTypeFamilies
-- >>> :set -XUndecidableInstances
-- >>> :set -fno-warn-partial-type-signatures
--
-- >>> import Data.UnitsOfMeasure ((*:), u, convert)
-- >>> import Flight.LatLng (radToDegLL, degToRadLL)
-- >>> import Internal.Ellipsoid.PointToPoint.Vincenty.Double as V
--
-- >>> :{
-- circumDeg
--    :: RealFloat a
--    => LatLng a [u| deg |]
--    -> QRadius a [u| m |]
--    -> (Quantity a [u| deg |])
--    -> LatLng Double [u| deg |]
-- circumDeg ll r tc =
--     radToDegLL convert $ circum (degToRadLL convert ll) r (TrueCourse ((convert tc) :: Quantity _ [u| rad |]))
-- :}
