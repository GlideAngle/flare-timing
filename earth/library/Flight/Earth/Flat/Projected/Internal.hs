{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Earth.Flat.Projected.Internal
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
import qualified LatLng as HC (mkLatLng)
import qualified Datum as HC (wgs84Datum)

import Flight.Units ()
import Flight.Units.Angle (Angle(..))
import Flight.LatLng (LatLng(..), Lat(..), Lng(..), radToDegLL)
import Flight.LatLng.Double (radToDeg)
import Flight.Zone (Zone(..), center, realToFracZone)
import Flight.Distance (TaskDistance(..))

data DistanceAzimuth a =
    DistanceAzimuth
        { dist :: Quantity a [u| m |]
        , azFwd :: Quantity a [u| rad |]
        , azRev :: Quantity a [u| rad |]
        }

tooFar :: Num a => TaskDistance a
tooFar = TaskDistance [u| 20000000 m |]

zoneToProjectedEastNorth :: Real a => Zone a -> Either String HC.UTMRef
zoneToProjectedEastNorth z = do
    xLL <- runIdentity . runExceptT $ HC.mkLatLng xDegLat xDegLng 0 HC.wgs84Datum
    runIdentity . runExceptT $ HC.toUTMRef xLL
    where
        xRad = center . realToFracZone $ z

        (LatLng (Lat (MkQuantity xDegLat), Lng (MkQuantity xDegLng))) =
            radToDegLL radToDeg xRad

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
