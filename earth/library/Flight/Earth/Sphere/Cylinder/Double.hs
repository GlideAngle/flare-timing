module Flight.Earth.Sphere.Cylinder.Double (circumSample) where

import Data.Fixed (mod')
import Data.Maybe (catMaybes)
import Data.UnitsOfMeasure ((-:), u, convert, unQuantity, abs')
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
import Flight.Earth.Sphere.PointToPoint.Double (distanceHaversine, azimuthFwd)
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

-- | Using a method from the
-- <http://www.edwilliams.org/avform.htm#LL Aviation Formulary>
-- a point on a cylinder wall is found by going out to the distance of the
-- radius on the given radial true course 'rtc'.
circum :: Real a
       => LatLng a [u| rad |]
       -> QRadius a [u| m |]
       -> TrueCourse a
       -> LatLng Double [u| rad |]
circum
    (LatLng (Lat (MkQuantity latRadian'), Lng (MkQuantity lngRadian')))
    (Radius (MkQuantity rRadius))
    (TrueCourse (MkQuantity rtc)) =
    LatLng (Lat lat'', Lng lng'')
    where
        lat :: Double
        lat = realToFrac latRadian'

        lng :: Double
        lng = realToFrac lngRadian'

        MkQuantity tc = MkQuantity $ realToFrac rtc

        radius' :: Double
        radius' = realToFrac rRadius

        Radius rEarth = earthRadius
        bigR = unQuantity rEarth

        lat' :: Double
        lat' = asin (sin lat * cos d + cos lat * sin d * cos tc)

        dlng = atan ((sin tc * sin d * cos lat) / (cos d - sin lat * sin lat))

        a = lng - dlng + pi
        b = 2 * pi 

        lng' :: Double
        lng' = mod' a b - pi

        d = radius' / bigR

        lat'' :: Quantity Double [u| rad |]
        lat'' = MkQuantity lat'

        lng'' :: Quantity Double [u| rad |]
        lng'' = MkQuantity lng'

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
            (Just m, Line _ x) -> let y = center m in onLine (azimuthFwd x y) ys
            (Just _, Circle _ _) -> ys
            (Just _, SemiCircle _ _) -> ys
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

        xs :: [TrueCourse Double]
        xs =
            TrueCourse . MkQuantity <$>
                let lhs = [mid - (fromInteger n) * step | n <- [1 .. half]]
                    rhs = [mid + (fromInteger n) * step | n <- [1 .. half]]
                in lhs ++ (mid : rhs)

        r :: QRadius Double [u| m |]
        r@(Radius (MkQuantity limitRadius)) = radius zone'

        ptCenter = center zone'
        circumR = circum ptCenter

        getClose' = getClose zone' ptCenter limitRadius spTolerance

        ys :: ([ZonePoint Double], [TrueCourse Double])
        ys = unzip $ getClose' 10 (Radius (MkQuantity 0)) (circumR r) <$> xs

halfPi :: Quantity Double [u| rad |]
halfPi = convert [u| 90 deg |]

onLine
    :: Maybe (Quantity Double [u| rad |])
    -> ([ZonePoint Double], [TrueCourse Double])
    -> ([ZonePoint Double], [TrueCourse Double])
onLine Nothing xs = xs
onLine (Just theta) (xs, cs) =
    unzip . catMaybes $
    [ if (abs' (b -: theta)) < halfPi then Nothing else Just (x, c)
    | x@ZonePoint{radial = Bearing b} <- xs
    | c <- cs
    ]

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
