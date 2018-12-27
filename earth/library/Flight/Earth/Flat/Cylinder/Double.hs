module Flight.Earth.Flat.Cylinder.Double (circumSample) where

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
import Flight.Earth.Flat.PointToPoint.Double (distanceEuclidean)
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
import Flight.Earth.Flat.Projected.Internal (zoneToProjectedEastNorth)

fromHcLatLng :: HCLL.LatLng -> LatLng Double [u| rad |]
fromHcLatLng HCLL.LatLng{latitude, longitude} =
    LatLng (Lat . convert $ lat, Lng . convert $ lng)
    where
        lat :: Quantity Double [u| deg |]
        lat = MkQuantity latitude

        lng :: Quantity Double [u| deg |]
        lng = MkQuantity longitude

eastNorthToLatLng :: HCEN.UTMRef -> Either String HCLL.LatLng
eastNorthToLatLng = runIdentity . runExceptT . HCEN.toLatLng

errorHc :: a
errorHc = error "Cannot convert between lat/lng and easting/northing."

circum
    :: Real a
    => LatLng a [u| rad |]
    -> QRadius a [u| m |]
    -> TrueCourse a
    -> LatLng Double [u| rad |]
circum xLL r tc =
    case circumEN xLL r tc of
        Left _ -> errorHc
        Right yEN ->
            case eastNorthToLatLng yEN of
                Left _ -> errorHc
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

-- | Generates a pair of lists, the lat/lng of each generated point
-- and its distance from the center. It will generate 'samples' number of such
-- points that should lie close to the circle. The difference between
-- the distance to the origin and the radius should be less han the 'tolerance'.
--
-- The points of the compass are divided by the number of samples requested.
circumSample :: CircumSample Double
circumSample SampleParams{..} (ArcSweep (Bearing (MkQuantity bearing))) zp zone =
    ys
    where
        nNum = unSamples spSamples
        half = nNum `div` 2
        step = bearing / (fromInteger nNum)
        mid = maybe 0 (\ZonePoint{radial = Bearing (MkQuantity b)} -> b) zp

        zone' :: Zone Double
        zone' =
            case zp of
              Nothing -> zone
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

getClose :: Zone Double
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
                distanceEuclidean
                (realToFracZone <$> [Point ptCenter, Point y])
