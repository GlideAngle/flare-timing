{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Internal.Flat.Projected.Internal
    ( DistanceAzimuth(..)
    , pythagorean
    , zoneToProjectedEastNorth 
    , tooFar
    ) where

import Prelude hiding (flip)
import Data.Functor.Identity (runIdentity)
import Control.Monad.Except (runExceptT)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import qualified UTMRef as HC (UTMRef(..), toUTMRef)
import qualified LatLng as HC (LatLng(..), mkLatLng)
import qualified Datum as HC (wgs84Datum)

import Flight.Units ()
import Flight.Units.Angle (Angle(..))
import Flight.Units.DegMinSec (DMS(..))
import Flight.LatLng (LatLng(..), Lat(..), Lng(..), radToDegLL)
import Flight.LatLng.Double (radToDeg)
import Flight.Zone (Zone(..), center, realToFracZone)
import Flight.Distance (QTaskDistance, TaskDistance(..))

data DistanceAzimuth a =
    DistanceAzimuth
        { dist :: Quantity a [u| m |]
        , azFwd :: Quantity a [u| rad |]
        , azRev :: Quantity a [u| rad |]
        }

tooFar :: Num a => QTaskDistance a [u| m |]
tooFar = TaskDistance [u| 20000000 m |]

zoneToProjectedEastNorth :: Real a => Zone a -> Either String HC.UTMRef
zoneToProjectedEastNorth z = do
    xLL <- runIdentity . runExceptT $ HC.mkLatLng xDegLat xDegLng 0 HC.wgs84Datum
    runIdentity . runExceptT $ HC.toUTMRef xLL
    where
        xRad = center . realToFracZone $ z

        (LatLng (Lat (MkQuantity xDegLat), Lng (MkQuantity xDegLng))) =
            radToDegLL radToDeg xRad

-- |
-- >>> HC.ellipsoid HC.wgs84Datum
-- [semi-major axis = 6378137.0, semi-minor axis = 6356752.3142]
--
-- >>> runIdentity . runExceptT $ checkLatLng 0 0
-- Right ((0°,0°),((166021.44317932962,0.0),('N',31)))
--
-- >>> runIdentity . runExceptT $ checkLatLng 45 0
-- Right ((45°,0°),((263553.9738663146,4987329.504902129),('T',31)))
--
-- >>> runIdentity . runExceptT $ checkLatLng (negate 45) 0
-- Right ((-45°,0°),((263553.9738663146,5012670.495097871),('G',31)))
--
-- >>> runIdentity . runExceptT $ checkLatLng 90 0
-- Left "Latitude (90.0) falls outside the UTM grid."
--
-- >>> runIdentity . runExceptT $ checkLatLng 85 0
-- Left "Latitude (85.0) falls outside the UTM grid."
--
-- >>> runIdentity . runExceptT $ checkLatLng 84 0
-- Right ((84°,0°),((465005.34493874514,9329005.182975996),('X',31)))
--
-- >>> runIdentity . runExceptT $ checkLatLng (negate 90) 0
-- Left "Latitude (-90.0) falls outside the UTM grid."
--
-- >>> runIdentity . runExceptT $ checkLatLng (negate 81) 0
-- Left "Latitude (-81.0) falls outside the UTM grid."
--
-- >>> runIdentity . runExceptT $ checkLatLng (negate 80) 0
-- Right ((-80°,0°),((441867.7848676922,1116915.0433355588),('C',31)))
--
-- Check the latitude bands, C .. X. There are on bands I and O.
-- >>> partitionEithers $ runIdentity . runExceptT . checkLatZ <$> [-80,-72..80]
-- ([],[(-80°,'C'),(-72°,'D'),(-64°,'E'),(-56°,'F'),(-48°,'G'),(-40°,'H'),(-32°,'J'),(-24°,'K'),(-16°,'L'),(-8°,'M'),(0°,'N'),(8°,'P'),(16°,'Q'),(24°,'R'),(32°,'S'),(40°,'T'),(48°,'U'),(56°,'V'),(64°,'W'),(72°,'X'),(80°,'X')])
pythagorean :: HC.UTMRef -> HC.UTMRef -> DistanceAzimuth Double
pythagorean x y =
    DistanceAzimuth
        { dist = MkQuantity $ realToFrac d
        , azFwd = theta
        , azRev = rotate flip theta
        }
    where
        dN = yN - xN
        dE = yE - xE
        d = sqrt $ dN * dN + dE * dE

        xN = HC.northing x
        yN = HC.northing y

        xE = HC.easting x
        yE = HC.easting y

        theta :: Quantity _ [u| rad |]
        theta = normalize . MkQuantity $ atan2 dN dE

        flip :: Quantity _ [u| rad |]
        flip = convert [u| 180 deg |]

_EN :: HC.UTMRef -> ((Double, Double), (Char, Int))
_EN HC.UTMRef{easting = e, northing = n, latZone = latZ, lngZone = lngZ} =
    ((e, n), (latZ, lngZ))

_LL :: HC.LatLng -> (Double, Double)
_LL HC.LatLng{latitude = lat, longitude = lng} =
    (lat, lng)

_DMS :: (Double, Double) -> (DMS, DMS)
_DMS (lat, lng) =
    ( fromQuantity (MkQuantity lat :: Quantity Double [u| deg |])
    , fromQuantity (MkQuantity lng :: Quantity Double [u| deg |])
    )

_LLtoDMS :: HC.LatLng -> (DMS, DMS)
_LLtoDMS = _DMS . _LL

-- $setup
-- >>> import qualified Datum as HC
-- >>> import qualified LatLng as HC
-- >>> import Data.Either
--
-- >>> checkLatLng lat lng = do ll <- HC.mkLatLng lat lng 0 HC.wgs84Datum; u <- HC.toUTMRef ll; return $ (_LLtoDMS ll, _EN u)
-- >>> checkLatZ lat = do ll <- HC.mkLatLng lat 0 0 HC.wgs84Datum; u <- HC.toUTMRef ll; return $ (fst $ _LLtoDMS ll, HC.latZone u)
