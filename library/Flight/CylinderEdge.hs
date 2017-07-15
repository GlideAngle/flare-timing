{-# LANGUAGE RecordWildCards #-}

module Flight.CylinderEdge
    ( Samples(..)
    , Tolerance(..)
    , SampleParams(..)
    , ZonePoint(..)
    , circumSample
    , sample
    ) where

import Data.Ratio ((%))
import qualified Data.Number.FixedFunctions as F
import Data.Fixed (mod')

import Flight.Geo (LatLng(..), Epsilon(..), earthRadius, defEps, degToRad, radToDeg)
import Flight.Zone (Zone(..), Radius(..), Bearing(..), center, radius)
import Flight.PointToPoint (TaskDistance(..), distancePointToPoint)

newtype TrueCourse = TrueCourse Rational deriving (Eq, Ord, Show)
newtype Samples = Samples { unSamples :: Integer } deriving (Eq, Ord, Show)
newtype Tolerance = Tolerance { unTolerance :: Rational } deriving (Eq, Ord, Show)

data ZonePoint
    = ZonePoint
        { sourceZone :: Zone
        -- ^ This is the zone that generated the point.
        , point :: LatLng
        -- ^ A point on the edge of this zone.
        , radial :: Bearing
        -- ^ A point on the edge of this zone with this bearing from
        -- the origin.
        , orbit :: Radius
        -- ^ A point on the edge of this zone at this distance from the
        -- origin.
        }

data SampleParams
    = SampleParams
        { spSamples :: Samples
        , spTolerance :: Tolerance
        }

-- | Generate sample points for a zone. These get added to the graph to work out
-- the shortest path.
sample :: SampleParams
       -> Bearing
       -> Maybe ZonePoint
       -> Zone
       -> [ZonePoint]
sample _ _ _ px@(Point x) = [ZonePoint px x (Bearing 0) (Radius 0)]
sample _ _ _ px@(Vector _ x) = [ZonePoint px x (Bearing 0) (Radius 0)]
sample sp b zs z@(Cylinder _ _) = fst $ circumSample sp b zs z
sample sp b zs z@(Conical _ _ _) = fst $ circumSample sp b zs z
sample sp b zs z@(Line _ _) = fst $ circumSample sp b zs z
sample sp b zs z@(SemiCircle _ _) = fst $ circumSample sp b zs z
 
-- | Using a method from the
-- <http://www.edwilliams.org/avform.htm#LL Aviation Formulary>
-- a point on a cylinder wall is found by going out to the distance of the
-- radius on the given radial true course 'rtc'.
circum :: LatLng -> Epsilon -> Radius -> TrueCourse -> LatLng
circum (LatLng (latDegree, lngDegree)) _ (Radius rRadius) (TrueCourse rtc) =
    LatLng (radToDeg defEps $ toRational lat', radToDeg defEps $ toRational lng')
    where
        lat :: Double
        lat = fromRational $ degToRad defEps latDegree

        lng :: Double
        lng = fromRational $ degToRad defEps lngDegree

        tc :: Double
        tc = fromRational rtc

        radius :: Double
        radius = fromRational rRadius

        bigR = fromRational earthRadius

        lat' = asin (sin lat * cos d + cos lat * sin d * cos tc)

        dlng = atan ((sin tc * sin d * cos lat) / (cos d - sin lat * sin lat))

        a = lng - dlng + pi 
        b = 2 * pi 
        lng' = mod' a b - pi

        d = radius / bigR

-- | Generates a pair of lists, the lat/lng of each generated point
-- and its distance from the center. It will generate 'samples' number of such
-- points that should lie close to the circle. The difference between
-- the distance to the origin and the radius should be less han the 'tolerance'.
--
-- The points of the compass are divided by the number of samples requested.
circumSample :: SampleParams
             -> Bearing
             -> Maybe ZonePoint
             -> Zone
             -> ([ZonePoint], [Double])
circumSample SampleParams{..} (Bearing bearing) zp zone =
    unzip ys
    where
        (Epsilon eps) = defEps

        nNum = unSamples spSamples
        half = nNum `div` 2
        pi = F.pi eps
        halfRange = pi / bearing

        zone' =
            case zp of
              Nothing -> zone
              Just ZonePoint{..} -> sourceZone

        xs :: [TrueCourse]
        xs =
            TrueCourse <$>
            case zp of
                Nothing ->
                    [ (2 * n % nNum) * pi | n <- [0 .. nNum]]

                Just ZonePoint{..} ->
                    [b]
                    ++ 
                    [ b - (n % half) * halfRange | n <- [1 .. half] ]
                    ++
                    [ b + (n % half) * halfRange | n <- [1 .. half]]
                    where
                        (Bearing b) = radial

        r@(Radius limitRadius) = radius zone'
        ptCenter = center zone'
        circumR = circum ptCenter defEps

        ys = getClose 10 (Radius 0) (circumR r) <$> xs

        getClose :: Int -> Radius -> (TrueCourse -> LatLng) -> TrueCourse -> (ZonePoint, Double)
        getClose trys yr@(Radius offset) f x@(TrueCourse tc)
            | trys <= 0 = (zp', dist)
            | (unTolerance spTolerance) <= 0 = (zp', dist)
            | limitRadius <= (unTolerance spTolerance) = (zp', dist)
            | otherwise =
                case d `compare` limitRadius of
                     EQ ->
                         (zp', dist)

                     GT ->
                         let offset' = offset - (d - limitRadius) * 105 / 100
                             f' = circumR (Radius $ limitRadius + offset')
                         in getClose (trys - 1) (Radius offset') f' x
                         
                     LT ->
                         if d > limitRadius - (unTolerance spTolerance) then (zp', dist) else
                             let offset' = offset + (limitRadius - d) * 94 / 100
                                 f' = circumR (Radius $ limitRadius + offset')
                             in getClose (trys - 1) (Radius offset') f' x
            where
                y = f x
                zp' = ZonePoint { sourceZone = zone'
                                , point = y
                                , radial = Bearing tc
                                , orbit = yr
                                }
                               
                (TaskDistance d) = distancePointToPoint [Point ptCenter, Point y]
                dist = fromRational d
