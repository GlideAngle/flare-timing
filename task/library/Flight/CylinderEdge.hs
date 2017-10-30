{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Flight.CylinderEdge
    ( Samples(..)
    , Tolerance(..)
    , SampleParams(..)
    , ZonePoint(..)
    , circumSample
    , sample
    ) where

import Prelude hiding (span)
import Data.Ratio ((%), numerator, denominator)
import qualified Data.Number.FixedFunctions as F
import Data.Fixed (mod')
import Data.UnitsOfMeasure (u, zero, unQuantity, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng
    ( Lat(..)
    , Lng(..)
    , LatLng(..)
    , Epsilon(..)
    , earthRadius
    , defEps
    )
import Flight.Zone (Zone(..), Radius(..), Bearing(..), center, radius)
import Flight.PointToPoint (distancePointToPoint, distanceHaversine)
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.Units (showRadian)

newtype TrueCourse =
    TrueCourse (Quantity Rational [u| rad |])
    deriving (Eq, Ord)

instance Show TrueCourse where
    show (TrueCourse tc) = "tc = " ++ showRadian tc

instance Num TrueCourse where
    (+) (TrueCourse (MkQuantity a)) (TrueCourse (MkQuantity b)) =
        TrueCourse (MkQuantity $ a + b)

    (*) (TrueCourse (MkQuantity a)) (TrueCourse (MkQuantity b)) =
        TrueCourse (MkQuantity $ a * b)

    negate (TrueCourse (MkQuantity tc)) =
        TrueCourse (MkQuantity $ negate tc)

    abs (TrueCourse (MkQuantity tc)) =
        TrueCourse (MkQuantity $ abs tc)

    signum (TrueCourse (MkQuantity tc)) =
        TrueCourse (MkQuantity $ signum tc)

    fromInteger x =
        TrueCourse (MkQuantity $ fromInteger x)

instance Fractional TrueCourse where
    fromRational tc = TrueCourse (MkQuantity tc)

    recip (TrueCourse (MkQuantity x)) =
        TrueCourse (MkQuantity (denominator x % numerator x))

newtype Samples = Samples { unSamples :: Integer } deriving (Eq, Ord, Show)
newtype Tolerance = Tolerance { unTolerance :: Rational } deriving (Eq, Ord, Show)

data ZonePoint
    = ZonePoint
        { sourceZone :: Zone
        -- ^ This is the zone that generated the point.
        , point :: LatLng [u| rad |]
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
sample _ _ _ px@(Point x) = [ZonePoint px x (Bearing zero) (Radius (MkQuantity 0))]
sample _ _ _ px@(Vector _ x) = [ZonePoint px x (Bearing zero) (Radius (MkQuantity 0))]
sample sp b zs z@Cylinder{} = fst $ circumSample sp b zs z
sample sp b zs z@Conical{} = fst $ circumSample sp b zs z
sample sp b zs z@Line{} = fst $ circumSample sp b zs z
sample sp b zs z@SemiCircle{} = fst $ circumSample sp b zs z
 
-- | Using a method from the
-- <http://www.edwilliams.org/avform.htm#LL Aviation Formulary>
-- a point on a cylinder wall is found by going out to the distance of the
-- radius on the given radial true course 'rtc'.
circum :: LatLng [u| rad |]
       -> Epsilon
       -> Radius
       -> TrueCourse
       -> LatLng [u| rad |]
circum
    (LatLng (Lat (MkQuantity latRadian'), Lng (MkQuantity lngRadian')))
    _
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
        radius' = fromRational rRadius

        bigR = fromRational $ unQuantity earthRadius

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
circumSample :: SampleParams
             -> Bearing
             -> Maybe ZonePoint
             -> Zone
             -> ([ZonePoint], [TrueCourse])
circumSample SampleParams{..} (Bearing (MkQuantity bearing)) zp zone =
    unzip ys
    where
        (Epsilon eps) = defEps

        nNum = unSamples spSamples
        half = nNum `div` 2
        pi' = F.pi eps
        halfRange = pi' / bearing

        zone' =
            case zp of
              Nothing -> zone
              Just ZonePoint{..} -> sourceZone

        xs :: [TrueCourse]
        xs =
            TrueCourse . MkQuantity <$>
            case zp of
                Nothing ->
                    [ (2 * n % nNum) * pi' | n <- [0 .. nNum]]

                Just ZonePoint{..} ->
                    [b]
                    ++ 
                    [ b - (n % half) * halfRange | n <- [1 .. half] ]
                    ++
                    [ b + (n % half) * halfRange | n <- [1 .. half]]
                    where
                        (Bearing (MkQuantity b)) = radial

        r@(Radius (MkQuantity limitRadius)) = radius zone'
        ptCenter = center zone'
        circumR = circum ptCenter defEps

        ys = getClose 10 (Radius (MkQuantity 0)) (circumR r) <$> xs

        getClose :: Int
                 -> Radius
                 -> (TrueCourse -> LatLng [u| rad |])
                 -> TrueCourse
                 -> (ZonePoint, TrueCourse)
        getClose trys yr@(Radius (MkQuantity offset)) f x@(TrueCourse tc)
            | trys <= 0 = (zp', dist)
            | unTolerance spTolerance <= 0 = (zp', dist)
            | limitRadius <= unTolerance spTolerance = (zp', dist)
            | otherwise =
                case d `compare` limitRadius of
                     EQ ->
                         (zp', dist)

                     GT ->
                         let offset' = offset - (d - limitRadius) * 105 / 100
                             f' = circumR (Radius (MkQuantity $ limitRadius + offset'))
                         in getClose (trys - 1) (Radius (MkQuantity offset')) f' x
                         
                     LT ->
                         if d > limitRadius - unTolerance spTolerance then (zp', dist) else
                             let offset' = offset + (limitRadius - d) * 94 / 100
                                 f' = circumR (Radius (MkQuantity $ limitRadius + offset'))
                             in getClose (trys - 1) (Radius (MkQuantity offset')) f' x
            where
                y = f x
                zp' = ZonePoint { sourceZone = zone'
                                , point = y
                                , radial = Bearing tc
                                , orbit = yr
                                }
                               
                (TaskDistance (MkQuantity d)) =
                    edgesSum
                    $ distancePointToPoint
                        (distanceHaversine defEps)
                        [Point ptCenter, Point y]

                dist = fromRational d
