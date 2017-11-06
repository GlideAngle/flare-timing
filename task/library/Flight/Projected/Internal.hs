{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE PartialTypeSignatures #-}

module Flight.Projected.Internal
    ( pythagorean
    , zoneToProjectedEastNorth 
    , tooFar
    ) where

import Data.Functor.Identity (runIdentity)
import Control.Monad.Except (runExceptT)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import qualified UTMRef as HC (UTMRef(..), toUTMRef)
import qualified LatLng as HC (mkLatLng)
import qualified Datum as HC (wgs84Datum)

import Flight.Zone (Zone(..), center, realToFracZone)
import Flight.Distance (TaskDistance(..))
import Flight.Units ()
import Flight.LatLng (LatLng(..), Lat(..), Lng(..), radToDegLL)
import Flight.LatLng.Double (radToDeg)

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

pythagorean :: HC.UTMRef -> HC.UTMRef -> Double
pythagorean x y =
    sqrt $ dN * dN + dE * dE
    where
        dN = yN - xN
        dE = yE - xE

        xN = HC.northing x
        yN = HC.northing y

        xE = HC.easting x
        yE = HC.easting y
