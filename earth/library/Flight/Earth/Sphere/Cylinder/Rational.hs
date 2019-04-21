module Flight.Earth.Sphere.Cylinder.Rational (circumSample) where

import Data.Fixed (mod')
import qualified Data.Number.FixedFunctions as F
import Data.UnitsOfMeasure ((+:), (-:), u, unQuantity, fromRational')
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
import Flight.Earth.ZoneShape.Rational (PointOnRadial, onLine, deg90)

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
circumSample SampleParams{..} (ArcSweep (Bearing (MkQuantity bearing))) arc0 zoneM zoneN
    | bearing < 0 || bearing > 2 * F.pi eps = fail "Arc sweep must be in the range 0..2π radians."
    | otherwise =
        case (zoneM, zoneN) of
            (Nothing, _) -> ys
            (Just _, Point _) -> ys
            (Just _, Vector _ _) -> ys
            (Just _, Cylinder _ _) -> ys
            (Just _, Conical _ _ _) -> ys
            (Just _, Line _ _ _) -> onLine defEps mkLinePt θ ys
            (Just _, Circle _ _) -> ys
            (Just _, SemiCircle _ _ _) -> ys
    where
        (Epsilon eps) = defEps
        nNum = unSamples spSamples
        half = nNum `div` 2
        step = bearing / (fromInteger nNum)
        mid = maybe 0 (\ZonePoint{radial = Bearing (MkQuantity b)} -> b) arc0

        zone' :: Zone Rational
        zone' =
            case arc0 of
              Nothing -> zoneN
              Just ZonePoint{..} -> sourceZone

        cs :: [Rational]
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
                    if bearing < 2 * F.pi eps
                       then cs
                       else
                            unQuantity (az +: deg90)
                            : unQuantity (az -: deg90)
                            : cs

                (Just _, Circle _ _) -> (Nothing, cs)
                (Just _, SemiCircle _ _ _) -> (Nothing, cs)

        (Radius (MkQuantity limitRadius)) = radius zone'
        limitRadius' = toRational limitRadius
        r = Radius (MkQuantity limitRadius')

        ptCenter = center zone'
        circumR = circum defEps ptCenter

        getClose' = getClose defEps zone' ptCenter limitRadius' spTolerance

        mkLinePt :: PointOnRadial
        mkLinePt _ (Bearing b) rLine = circumR rLine $ TrueCourse b

        ys' :: ([ZonePoint Rational], [TrueCourse Rational])
        ys' = unzip $ getClose' 10 (Radius (MkQuantity 0)) (circumR r) <$> xs

        ys = (fromRationalZonePoint <$> fst ys', snd ys')

getClose
    :: Epsilon
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
