module Flight.Earth.Sphere.Cylinder.Rational (circumSample) where

import Data.Fixed (mod')
import Data.UnitsOfMeasure (u, unQuantity, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.LatLng.Rational (Epsilon(..), defEps)
import Flight.Zone
    ( Zone(..)
    , QRadius
    , Radius(..)
    , Bearing(..)
    , ArcSweep(..)
    , center
    , radius
    , toRationalZone
    )
import Flight.Zone.Path (distancePointToPoint)
import Flight.Earth.Sphere.PointToPoint.Rational (distanceHaversine)
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
    , fromRationalZonePoint
    )
import Flight.Earth.Sphere (earthRadius)

-- | Using a method from the
-- <http://www.edwilliams.org/avform.htm#LL Aviation Formulary>
-- a point on a cylinder wall is found by going out to the distance of the
-- radius on the given radial true course 'rtc'.
circum :: Epsilon
       -> LatLng Rational [u| rad |]
       -> QRadius Rational [u| m |]
       -> TrueCourse Rational 
       -> LatLng Rational [u| rad |]
circum
    _
    (LatLng (Lat (MkQuantity latRadian'), Lng (MkQuantity lngRadian')))
    (Radius (MkQuantity rRadius))
    (TrueCourse rtc) =
    LatLng (Lat lat'', Lng lng'')
    where
        lat :: Double
        lat = fromRational latRadian'

        lng :: Double
        lng = fromRational lngRadian'

        MkQuantity tc = fromRational' rtc :: Quantity Double [u| rad |]

        radius' :: Double
        radius' = fromRational . toRational $ rRadius

        Radius rEarth = earthRadius
        bigR = fromRational $ unQuantity rEarth

        lat' :: Double
        lat' = asin (sin lat * cos d + cos lat * sin d * cos tc)

        dlng = atan ((sin tc * sin d * cos lat) / (cos d - sin lat * sin lat))

        a = lng - dlng + pi 
        b = 2 * pi 

        lng' :: Double
        lng' = mod' a b - pi

        d = radius' / bigR

        lat'' :: Quantity Rational [u| rad |]
        lat'' = MkQuantity $ toRational lat'

        lng'' :: Quantity Rational [u| rad |]
        lng'' = MkQuantity $ toRational lng'

-- | Generates a pair of lists, the lat/lng of each generated point
-- and its distance from the center. It will generate 'samples' number of such
-- points that should lie close to the circle. The difference between
-- the distance to the origin and the radius should be less han the 'tolerance'.
--
-- The points of the compass are divided by the number of samples requested.
circumSample :: CircumSample Rational
circumSample SampleParams{..} (ArcSweep (Bearing (MkQuantity bearing))) zp zone =
    (fromRationalZonePoint <$> fst ys, snd ys)
    where
        nNum = unSamples spSamples
        half = nNum `div` 2
        step = bearing / (fromInteger nNum)
        mid = maybe 0 (\ZonePoint{radial = Bearing (MkQuantity b)} -> b) zp

        zone' :: Zone Rational
        zone' =
            case zp of
              Nothing -> zone
              Just ZonePoint{..} -> sourceZone

        xs :: [TrueCourse Rational]
        xs =
            TrueCourse . MkQuantity <$>
                let lhs = [mid - (fromInteger n) * step | n <- [1 .. half]]
                    rhs = [mid + (fromInteger n) * step | n <- [1 .. half]]
                in lhs ++ (mid : rhs)

        (Radius (MkQuantity limitRadius)) = radius zone'
        limitRadius' = toRational limitRadius
        r = Radius (MkQuantity limitRadius')

        ptCenter = center zone'
        circumR = circum defEps ptCenter

        getClose' = getClose defEps zone' ptCenter limitRadius' spTolerance

        ys :: ([ZonePoint Rational], [TrueCourse Rational])
        ys = unzip $ getClose' 10 (Radius (MkQuantity 0)) (circumR r) <$> xs

getClose :: Epsilon
         -> Zone Rational
         -> LatLng Rational [u| rad |] -- ^ The center point.
         -> Rational -- ^ The limit radius.
         -> Tolerance Rational
         -> Int -- ^ How many tries.
         -> QRadius Rational [u| m |] -- ^ How far from the center.
         -> (TrueCourse Rational -> LatLng Rational [u| rad |]) -- ^ A point from the origin on this radial
         -> TrueCourse Rational -- ^ The true course for this radial.
         -> (ZonePoint Rational, TrueCourse Rational)
getClose epsilon zone' ptCenter limitRadius spTolerance trys yr@(Radius (MkQuantity offset)) f x@(TrueCourse tc)
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
                         epsilon
                         zone'
                         ptCenter
                         limitRadius
                         spTolerance
                         (trys - 1)
                         (Radius (MkQuantity offset'))
                         f'
                         x

             LT ->
                 if d > toRational (limitRadius - unTolerance spTolerance)
                 then (zp', x)
                 else
                     let offset' =
                             offset + (limitRadius - d) * 94 / 100

                         f' =
                             circumR (Radius (MkQuantity $ limitRadius + offset'))
                     in
                         getClose
                             epsilon
                             zone'
                             ptCenter
                             limitRadius
                             spTolerance
                             (trys - 1)
                             (Radius (MkQuantity offset'))
                             f'
                             x
    where
        circumR = circum epsilon ptCenter

        y = f x
        zp' = ZonePoint { sourceZone = toRationalZone zone'
                        , point = y
                        , radial = Bearing tc
                        , orbit = yr
                        } :: ZonePoint Rational

        (TaskDistance (MkQuantity d)) =
            edgesSum
            $ distancePointToPoint
                (distanceHaversine defEps)
                [Point ptCenter, Point y]
