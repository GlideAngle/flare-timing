module Flight.Earth.Sphere.Cylinder.Double (circumSample) where

import Data.UnitsOfMeasure ((+:), (-:), u, unQuantity)
import Data.UnitsOfMeasure.Internal (Quantity(..))

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
import Flight.Earth.Sphere.PointToPoint.Double (distanceHaversine)
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
import Flight.Earth.Sphere (earthRadius)
import Flight.Earth.ZoneShape.Double (PointOnRadial, onLine, deg90)

-- | Using a method from the
-- <http://www.edwilliams.org/avform.htm#LL Aviation Formulary>
-- a point on a cylinder wall is found by going out to the distance of the
-- radius on the given radial true course 'rtc'.
-- <https://www.movable-type.co.uk/scripts/latlong.html Movable Type Scripts>
-- has working examples in Javascript backed HTML pages.
--
-- >>> circumDeg (LatLng (Lat [u| -32.46363 deg |], Lng [u| 148.989 deg |])) (Radius [u| 286.27334927563106 m |]) [u| 332.30076790172313 deg |]
-- (-32.46135051411138°, 148.98758167886174°)
--
-- >>> circumDeg (LatLng (Lat [u| -32.46363 deg |], Lng [u| 148.989 deg |])) (Radius [u| 177.23328234645362 m |]) [u| 152.30076790172313 deg |]
-- (-32.46504123330422°, 148.9898781257936°)
circum
    :: Real a
    => LatLng a [u| rad |]
    -> QRadius a [u| m |]
    -> TrueCourse a
    -> LatLng Double [u| rad |]
circum
    (LatLng (Lat (MkQuantity lat), Lng (MkQuantity lng)))
    (Radius (MkQuantity r))
    (TrueCourse (MkQuantity tc)) =
    LatLng (Lat (MkQuantity φ2), Lng (MkQuantity λ2))
    where
        φ1 = realToFrac lat
        λ1 = realToFrac lng
        θ = realToFrac tc

        δ = let Radius (MkQuantity bigR) = earthRadius in realToFrac r / bigR

        φ2 = asin $ sin φ1 * cos δ + cos φ1 * sin δ * cos θ
        λ2 = λ1 + atan2 (sin θ * sin δ * cos φ1) (cos δ - sin φ1 * sin φ2)

-- | Generates a pair of lists, the lat/lng of each generated point
-- and its distance from the center. It will generate 'samples' number of such
-- points that should lie close to the circle. The difference between
-- the distance to the origin and the radius should be less han the 'tolerance'.
--
-- The points of the compass are divided by the number of samples requested.
circumSample :: CircumSample Double
circumSample SampleParams{..} (ArcSweep (Bearing (MkQuantity bearing))) arc0 zoneM zoneN
    | bearing < 0 || bearing > 2 * pi = fail "Arc sweep must be in the range 0..2π radians."
    | otherwise =
        case (zoneM, zoneN) of
            (Nothing, _) -> ys
            (Just _, Point _) -> ys
            (Just _, Vector _ _) -> ys
            (Just _, Cylinder _ _) -> ys
            (Just _, Conical _ _ _) -> ys
            (Just _, Line _ _ _) -> onLine mkLinePt θ ys
            (Just _, Circle _ _) -> ys
            (Just _, SemiCircle _ _ _) -> ys
    where
        nNum = unSamples spSamples
        half = nNum `div` 2
        step = bearing / (fromInteger nNum)
        mid = maybe 0 (\ZonePoint{radial = Bearing (MkQuantity b)} -> b) arc0

        zone' :: Zone Double
        zone' =
            case arc0 of
              Nothing -> zoneN
              Just ZonePoint{..} -> sourceZone

        cs :: [Double]
        cs =
                let lhs = [mid - (fromInteger n) * step | n <- [1 .. half]]
                    rhs = [mid + (fromInteger n) * step | n <- [1 .. half]]
                -- NOTE: The reverse of the LHS is not needed for correct
                -- operation but it helps when tracing.
                in reverse lhs ++ (mid : rhs)

        (θ, xs) =
            (fmap . fmap) (TrueCourse . MkQuantity) $
            case (zoneM, zoneN) of
                (Nothing, _) -> (Nothing, cs)
                (Just _, Point _) -> (Nothing, cs)
                (Just _, Vector _ _) -> (Nothing, cs)
                (Just _, Cylinder _ _) -> (Nothing, cs)
                (Just _, Conical _ _ _) -> (Nothing, cs)
                (Just _, Line Nothing _ _) -> (Nothing, cs)
                (Just _, Line (Just (Bearing az)) _ _) ->
                    -- NOTE: For a line we don't want to miss a likely local
                    -- minimum where the line intersects the circle so let's
                    -- add those true courses explicitly now at 90° and 270°
                    -- from the azimuth.
                    (Just az,) $
                    if bearing < 2 * pi
                       then cs
                       else
                            unQuantity (az +: deg90)
                            : unQuantity (az -: deg90)
                            : cs

                (Just _, Circle _ _) -> (Nothing, cs)
                (Just _, SemiCircle _ _ _) -> (Nothing, cs)

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
        zp' = ZonePoint { sourceZone = realToFracZone zone'
                        , point = y
                        , radial = Bearing tc
                        , orbit = yr
                        } :: ZonePoint Double

        (TaskDistance (MkQuantity d)) =
            edgesSum
            $ distancePointToPoint
                distanceHaversine
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
