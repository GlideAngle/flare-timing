{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Flight.Earth.Ellipsoid.Cylinder.Double (circumSample) where

import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.Zone
    ( Zone(..)
    , Radius(..)
    , Bearing(..)
    , center
    , radius
    , realToFracZone
    , realToFracLatLng
    )
import Flight.Zone.Path (distancePointToPoint)
import Flight.Earth.Ellipsoid.PointToPoint.Double (distanceVincenty)
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.Zone.Cylinder
    ( TrueCourse(..)
    , ZonePoint(..)
    , Tolerance(..)
    , Samples(..)
    , SampleParams(..)
    , CircumSample
    , orbit
    , radial
    , point
    , sourceZone
    )
import Flight.Earth.Ellipsoid
    (Ellipsoid(..), VincentyAccuracy(..)
    , defaultVincentyAccuracy, wgs84, flattening
    )

iterateVincenty
    :: (Floating a, Ord a)
    => VincentyAccuracy a -> a -> a -> a -> a -> a -> a -> a
iterateVincenty
    accuracy@(VincentyAccuracy tolerance)
    _A
    _B
    s
    b
    σ1
    σ =
    if σ < tolerance
       then σ 
       else iterateVincenty accuracy _A _B s b σ1 σ'
    where
        _2σm = 2 * σ1 + σ
        cos2σm = cos _2σm
        cos²2σm = cos2σm * cos2σm
        sinσ = sin σ
        cosσ = cos σ
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

        σ' = s / b * _A + _Δσ

vincentyDirect
    :: (Real a, Floating a, Fractional a, RealFloat a)
    => Ellipsoid a
    -> VincentyAccuracy a
    -> LatLng a [u| rad |]
    -> Radius a [u| m |]
    -> TrueCourse a
    -> LatLng a [u| rad |]
vincentyDirect
    ellipsoid@Ellipsoid{semiMajor, semiMinor}
    accuracy
    (LatLng (Lat (MkQuantity _Φ1), Lng (MkQuantity _L1)))
    (Radius (MkQuantity s))
    (TrueCourse (MkQuantity α1)) =
    LatLng (Lat . MkQuantity $ _Φ2, Lng . MkQuantity $ _L2)
    where
        f = flattening ellipsoid

        -- Initial setup
        _U1 = atan $ (1 - f) * tan _Φ1
        σ1 = atan2 (tan _U1) (cos α1)
        sinα = cos _U1 * sin α1
        sin²α = sinα * sinα
        cos²α = 1 - sin²α 
        _A = 1 + u² / 16384 * (4096 + u² * (negate 768 + u² * (320 - 175 * u²)))
        _B = u² / 1024 * (256 + u² * (negate 128 + u² * (74 - 47 * u²)))

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

        _2σm = 2 * σ1 + σ
        cos2σm = cos _2σm
        cos²2σm = cos2σm * cos2σm
        x = σ + _C * sinσ * y
        y = cos (2 * cos2σm + _C * cosσ * (negate 1 + 2 * cos²2σm))
        _L = λ * (1 - _C) * f * sinα * x

        _L2 = _L + _L1

        MkQuantity a = semiMajor
        MkQuantity b = semiMinor
        a² = a * a
        b² = b * b
        u² = cos²α * (a² - b²) / b² 

circum
    :: (Real a, Fractional a, RealFloat a)
    => LatLng a [u| rad |]
    -> Radius a [u| m |]
    -> TrueCourse a
    -> LatLng Double [u| rad |]
circum x r tc =
    realToFracLatLng $ vincentyDirect wgs84 accuracy' x r tc
    where
        VincentyAccuracy accuracy = defaultVincentyAccuracy
        accuracy' = VincentyAccuracy $ fromRational accuracy

-- | Generates a pair of lists, the lat/lng of each generated point
-- and its distance from the center. It will generate 'samples' number of such
-- points that should lie close to the circle. The difference between
-- the distance to the origin and the radius should be less han the 'tolerance'.
--
-- The points of the compass are divided by the number of samples requested.
circumSample :: CircumSample Double
circumSample SampleParams{..} (Bearing (MkQuantity bearing)) zp zone =
    ys
    where
        nNum = unSamples spSamples
        half = nNum `div` 2
        halfRange = pi / bearing

        zone' :: Zone Double
        zone' =
            case zp of
              Nothing -> zone
              Just ZonePoint{..} -> sourceZone

        xs :: [TrueCourse Double]
        xs =
            TrueCourse . MkQuantity <$>
            case zp of
                Nothing ->
                    [ 2.0 * fromInteger n / fromInteger nNum * pi
                    | n <- [0 .. nNum]
                    ]

                Just ZonePoint{..} ->
                    [b]
                    ++ 
                    [ b - fromInteger n / fromInteger half * halfRange
                    | n <- [1 .. half]
                    ]
                    ++
                    [ b + fromInteger n / fromInteger half * halfRange
                    | n <- [1 .. half]
                    ]
                    where
                        (Bearing (MkQuantity b)) = radial

        r :: Radius Double [u| m |]
        r@(Radius (MkQuantity limitRadius)) = radius zone'

        ptCenter = center zone'
        circumR = circum ptCenter

        getClose' = getClose zone' ptCenter limitRadius spTolerance

        ys :: ([ZonePoint Double], [TrueCourse Double])
        ys = unzip $ getClose' 10 (Radius (MkQuantity 0)) (circumR r) <$> xs

getClose :: Zone Double
         -> LatLng Double [u| rad |] -- ^ The center point.
         -> Double -- ^ The limit radius.
         -> Tolerance Double
         -> Int -- ^ How many tries.
         -> Radius Double [u| m |] -- ^ How far from the center.
         -> (TrueCourse Double -> LatLng Double [u| rad |]) -- ^ A point from the origin on this radial
         -> TrueCourse Double -- ^ The true course for this radial.
         -> (ZonePoint Double, TrueCourse Double)
getClose zone' ptCenter limitRadius spTolerance trys yr@(Radius (MkQuantity offset)) f x@(TrueCourse tc)
    | trys <= 0 = (zp', x)
    | unTolerance spTolerance <= 0 = (zp', x)
    | limitRadius <= unTolerance spTolerance = (zp', x)
    | otherwise =
        case d `compare` limitRadius of
             EQ ->
                 (zp', x)

             GT ->
                 let offset' =
                         offset - (d - limitRadius) * 105 / 100

                     f' =
                         circumR (Radius (MkQuantity $ limitRadius + offset'))

                 in
                     getClose
                         zone'
                         ptCenter
                         limitRadius
                         spTolerance
                         (trys - 1)
                         (Radius (MkQuantity offset'))
                         f'
                         x
                 
             LT ->
                 if d > (limitRadius - unTolerance spTolerance)
                 then (zp', x)
                 else
                     let offset' =
                             offset + (limitRadius - d) * 94 / 100

                         f' =
                             circumR (Radius (MkQuantity $ limitRadius + offset'))

                     in
                         getClose
                             zone'
                             ptCenter
                             limitRadius
                             spTolerance
                             (trys - 1)
                             (Radius (MkQuantity offset'))
                             f'
                             x
    where
        circumR = circum ptCenter

        y = f x
        zp' = ZonePoint { sourceZone = realToFracZone zone'
                        , point = y
                        , radial = Bearing tc
                        , orbit = yr
                        } :: ZonePoint Double
                       
        (TaskDistance (MkQuantity d)) =
            edgesSum
            $ distancePointToPoint
                (distanceVincenty wgs84)
                (realToFracZone <$> [Point ptCenter, Point y])
