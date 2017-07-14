{-# LANGUAGE RecordWildCards #-}

module Flight.CylinderEdge
    ( Samples(..)
    , Tolerance(..)
    , SampleParams(..)
    , circumSample
    , sample
    ) where

import Data.Ratio ((%))
import qualified Data.Number.FixedFunctions as F
import Data.Fixed (mod')
import Data.Maybe (catMaybes)
import Control.Arrow (first)

import Flight.Geo (LatLng(..), Epsilon(..), earthRadius, defEps, degToRad, radToDeg)
import Flight.Zone (Zone(..), Radius(..), center)
import Flight.PointToPoint (TaskDistance(..), distancePointToPoint)
import Flight.Separated (separatedZones)

newtype TrueCourse = TrueCourse Rational deriving (Eq, Ord, Show)
newtype Samples = Samples { unSamples :: Integer } deriving (Eq, Ord, Show)
newtype Tolerance = Tolerance { unTolerance :: Rational } deriving (Eq, Ord, Show)

data SampleParams
    = SampleParams
        { samples :: Samples
        , tolerance :: Tolerance
        }

sample :: SampleParams -> Zone -> [LatLng]
sample _ (Point x) = [x]
sample _ (Vector _ x) = [x]
sample sp (Cylinder r x) = fst $ circumSample sp r x
sample sp (Conical _ r x) = fst $ circumSample sp r x
sample sp (Line r x) = fst $ circumSample sp r x
sample sp (SemiCircle r x) = fst $ circumSample sp r x
 
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

-- | SEE: http://www.edwilliams.org/avform.htm#LL
circumSample :: SampleParams -> Radius -> LatLng -> ([LatLng], [Double])
circumSample SampleParams{..} r@(Radius limitRadius) ptCenter =
    unzip ys
    where
        (Epsilon eps) = defEps

        nNum = unSamples samples

        xs :: [TrueCourse]
        xs =
            [ TrueCourse $ ((2 * n) % nNum) * F.pi eps
            | n <- [0 .. nNum]
            ]

        circumR = circum ptCenter defEps

        ys = getClose 10 (Radius 0) (circumR r) <$> xs

        getClose :: Int -> Radius -> (TrueCourse -> LatLng) -> TrueCourse -> (LatLng, Double)
        getClose trys (Radius offset) f x
            | trys <= 0 = (y, dist)
            | (unTolerance tolerance) <= 0 = (y, dist)
            | limitRadius <= (unTolerance tolerance) = (y, dist)
            | otherwise =
                case d `compare` limitRadius of
                     EQ -> (y, dist)
                     GT ->
                         let offset' = offset - (d - limitRadius) * 105 / 100
                             f' = circumR (Radius $ limitRadius + offset')
                         in getClose (trys - 1) (Radius offset') f' x
                     LT ->
                         if d > limitRadius - (unTolerance tolerance) then (y, dist) else
                             let offset' = offset + (limitRadius - d) * 94 / 100
                                 f' = circumR (Radius $ limitRadius + offset')
                             in getClose (trys - 1) (Radius offset') f' x
            where
                y = f x
                (TaskDistance d) = distancePointToPoint [Point ptCenter, Point y]
                dist = fromRational d
