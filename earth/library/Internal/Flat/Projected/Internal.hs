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
-- >>> runIdentity . runExceptT $ do ll <- HC.mkLatLng 0 0 0 HC.wgs84Datum; u <- HC.toUTMRef ll; return $ (_LLtoDMS ll, _EN u)
-- Right ((0°,0°),((166021.44317932962,0.0),('N',31)))
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
