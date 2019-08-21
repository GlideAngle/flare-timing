module Internal.Flat.Cylinder.Double (circumSample) where

import Data.Functor.Identity (runIdentity)
import Control.Monad.Except (runExceptT)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import qualified UTMRef as HCEN (UTMRef(..), toLatLng)
import qualified LatLng as HCLL (LatLng(..))

import Flight.Units ()
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
    )
import Flight.Zone.Path (distancePointToPoint)
import Internal.Flat.PointToPoint.Double (distance)
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
import Internal.Flat.Projected.Internal (zoneToProjectedEastNorth)
import Flight.Earth.ZoneShape.Double (PointOnRadial, onLine)

fromHcLatLng :: HCLL.LatLng -> LatLng Double [u| rad |]
fromHcLatLng HCLL.LatLng{latitude, longitude} =
    LatLng (Lat $ convert lat, Lng $ convert lng)
    where
        lat :: Quantity Double [u| deg |]
        lat = MkQuantity latitude

        lng :: Quantity Double [u| deg |]
        lng = MkQuantity longitude

eastNorthToLatLng :: HCEN.UTMRef -> Either String HCLL.LatLng
eastNorthToLatLng = runIdentity . runExceptT . HCEN.toLatLng

-- |
--
-- >>> circumDeg (LatLng (Lat [u| -32.46363 deg |], Lng [u| 148.989 deg |])) (Radius [u| 286.27334927563106 m |]) [u| 332.30076790172313 deg |]
-- (-32.464787076769696°, 148.99172189459117°)
--
-- >>> circumDeg (LatLng (Lat [u| -32.46363 deg |], Lng [u| 148.989 deg |])) (Radius [u| 177.23328234645362 m |]) [u| 152.30076790172313 deg |]
-- (-32.462913617965945°, 148.9873148952786°)
--
-- >>> circumDeg (LatLng (Lat [u| 0.0 deg |], Lng [u| 0.0 deg |])) (Radius [u| 111383 m |]) [u| 0.0 deg |]
-- (0.0°, 0.9999989478488059°)
--
-- >>> circumDeg (LatLng (Lat [u| 0.0 deg |], Lng [u| 0.0 deg |])) (Radius [u| 111383 m |]) [u| 180.0 deg |]
-- (1.2310722351314232e-16°, -0.9990789538347999°)
--
-- >>> circumDeg (LatLng (Lat [u| 0.0 deg |], Lng [u| 0.0 deg |])) (Radius [u| 111383 m |]) [u| 90.0 deg |]
-- (1.0063254069690941°, -4.601156388384097e-4°)
--
-- >>> circumDeg (LatLng (Lat [u| 0.0 deg |], Lng [u| 0.0 deg |])) (Radius [u| 111383 m |]) [u| -90.0 deg |]
-- (-1.0063254069690941°, -4.601156388384097e-4°)
circum
    :: Real a
    => LatLng a [u| rad |]
    -> QRadius a [u| m |]
    -> TrueCourse a
    -> LatLng Double [u| rad |]
circum xLL r tc =
    case circumEN xLL r tc of
        Left s -> error s
        Right yEN ->
            case eastNorthToLatLng yEN of
                Left s -> error s
                Right yLL -> fromHcLatLng yLL

circumEN
    :: Real a
    => LatLng a [u| rad |]
    -> QRadius a [u| m |]
    -> TrueCourse a
    -> Either String HCEN.UTMRef
circumEN xLL r tc =
    translate r tc <$> zoneToProjectedEastNorth (Point xLL)

translate
    :: Real a
    => QRadius a [u| m |]
    -> TrueCourse a
    -> HCEN.UTMRef
    -> HCEN.UTMRef
translate (Radius (MkQuantity rRadius)) (TrueCourse (MkQuantity rtc)) x =
    HCEN.UTMRef
        (xE + dE)
        (xN + dN)
        (HCEN.latZone x)
        (HCEN.lngZone x)
        (HCEN.datum x)
    where
        xE :: Double
        xE = HCEN.easting x

        xN :: Double
        xN = HCEN.northing x

        rRadius' :: Double
        rRadius' = realToFrac rRadius

        rtc' :: Double
        rtc' = realToFrac rtc

        dE :: Double
        dE = rRadius' * cos rtc'

        dN :: Double
        dN = rRadius' * sin rtc'

-- | Generates a pair of lists, the lat/lng of each generated point and its
-- distance from the center. It will generate 'samples' number of such points
-- that should lie close to the circle. The difference between the distance to
-- the origin and the radius should be less than the 'tolerance'.
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
        zone' =
            case arc0 of
                Nothing -> zoneN
                Just ZonePoint{..} -> sourceZone

        (θ, xs) = sampleAngles pi sp arcSweep arc0 zoneM zoneN

        r :: QRadius Double [u| m |]
        r@(Radius (MkQuantity limitRadius)) = radius zone'

        ptCenter = center zone'
        circumR = circum ptCenter

        getClose' = getClose zone' ptCenter limitRadius spTolerance

        mkLinePt :: PointOnRadial
        mkLinePt _ (Bearing b) rLine = circumR rLine $ TrueCourse b

        ys :: ([ZonePoint Double], [TrueCourse Double])
        ys = unzip $ getClose' 10 (Radius (MkQuantity 0)) (circumR r) <$> xs

getClose
    :: Zone Double
    -> LatLng Double [u| rad |] -- ^ The center point.
    -> Double -- ^ The limit radius.
    -> Tolerance Double
    -> Int -- ^ How many tries.
    -> QRadius Double [u| m |] -- ^ How far from the center.
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

        zp' :: ZonePoint Double
        zp' =
            ZonePoint
                { sourceZone = realToFracZone zone'
                , point = y
                , radial = Bearing tc
                , orbit = yr
                }

        (TaskDistance (MkQuantity d)) =
            edgesSum
            $ distancePointToPoint
                distance
                (realToFracZone <$> [Point ptCenter, Point y])

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
--
-- >>> :{
-- circumDeg
--    :: RealFrac a
--    => LatLng a [u| deg |]
--    -> QRadius a [u| m |]
--    -> (Quantity a [u| deg |])
--    -> LatLng Double [u| deg |]
-- circumDeg ll r tc =
--     radToDegLL convert $ circum (degToRadLL convert ll) r (TrueCourse ((convert tc) :: Quantity _ [u| rad |]))
-- :}
