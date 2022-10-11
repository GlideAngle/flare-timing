module Internal.Sphere.Cylinder.Rational (circumSample) where

import Data.Fixed (mod')
import qualified Data.Number.FixedFunctions as F
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
    )
import Flight.Zone.Cylinder
    ( TrueCourse(..)
    , ZonePoint(..)
    , SampleParams(..)
    , CircumSample
    , orbit
    , radial
    , point
    , sourceZone
    , fromRationalZonePoint
    , sampleAngles
    )
import Flight.Earth.Sphere (earthRadius)
import Flight.Earth.ZoneShape.Rational (PointOnRadial, onLine)
import qualified Internal.Sphere.PointToPoint.Rational as H (distance)
import Internal.CylinderOutline.Rational (getClose)

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
circumSample SampleParams{..} arcSweep@(ArcSweep (Bearing (MkQuantity bearing))) arc0 zoneM zoneN
    | bearing < 0 || bearing > 2 * F.pi eps = error "Arc sweep must be in the range 0..2π radians."
    | otherwise =
        case spSamples of
            [] -> error "Empty list of sample numbers."
            sp0 : _ ->
                let (θ, xs) = sampleAngles (F.pi eps) sp0 arcSweep arc0 zoneM zoneN

                    ys' :: ([ZonePoint Rational], [TrueCourse Rational])
                    ys' = unzip $ getClose' 10 (Radius (MkQuantity 0)) (circumR r) <$> xs

                    ys = (fromRationalZonePoint <$> fst ys', snd ys')

                in
                    case (zoneM, zoneN) of
                        (Nothing, _) -> ys
                        (Just _, Point{}) -> ys
                        (Just _, Vector{}) -> ys
                        (Just _, Cylinder{}) -> ys
                        (Just _, Conical{}) -> ys
                        (Just _, Line{}) -> onLine defEps mkLinePt θ ys
                        (Just _, Circle{}) -> ys
                        (Just _, SemiCircle{}) -> ys
    where
        (Epsilon eps) = defEps

        zone' :: Zone Rational
        zone' =
            case arc0 of
              Nothing -> zoneN
              Just ZonePoint{..} -> sourceZone

        (Radius (MkQuantity limitRadius)) = radius zone'
        limitRadius' = toRational limitRadius
        r = Radius (MkQuantity limitRadius')

        ptCenter = center zone'
        circumR = circum defEps ptCenter

        getClose' =
            let d = H.distance defEps
            in getClose defEps d circum zone' ptCenter limitRadius' spTolerance

        mkLinePt :: PointOnRadial
        mkLinePt _ (Bearing b) rLine = circumR rLine $ TrueCourse b
