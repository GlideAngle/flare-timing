{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Sphere.Compare.SphereVersusEllipsoid (dblChecks, ratChecks) where

import Prelude hiding (span, min)
import Data.Ratio ((%))
import Test.Tasty (TestTree)
import Data.UnitsOfMeasure (u, convert)

import Flight.Units ()
import Flight.Units.DegMinSec (DMS(..))
import Flight.LatLng.Rational (Epsilon(..))
import Flight.Distance (TaskDistance(..))
import qualified Flight.Earth.Sphere.PointToPoint.Double as Dbl (distanceHaversine)
import qualified Flight.Earth.Sphere.PointToPoint.Rational as Rat (distanceHaversine)
import qualified Tolerance as T (GetTolerance, dblChecks, ratChecks)

getTolerance :: (Ord a, Fractional a) => T.GetTolerance a
getTolerance d'
    | d < [u| 100 km |] = convert [u| 425.4 m |]
    | d < [u| 500 km |] = convert [u| 2.495 km |]
    | d < [u| 1000 km |] = [u| 4.06 km |]
    | otherwise = [u| 19.5 km |]
    where
        d = convert d'

dblChecks
    :: [TaskDistance Double]
    -> [((DMS, DMS), (DMS, DMS))]
    -> [TestTree]
dblChecks =
    T.dblChecks (Dbl.distanceHaversine) getTolerance

ratChecks
    :: [TaskDistance Double]
    -> [((DMS, DMS), (DMS, DMS))]
    -> [TestTree]
ratChecks =
    T.ratChecks span getTolerance
    where
        span = Rat.distanceHaversine e
        e = Epsilon $ 1 % 1000000000000000000
