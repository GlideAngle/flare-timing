{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Sphere.Distance (toDistanceEqual, toDistanceClose) where

import Prelude hiding (span)
import Test.Tasty (TestTree)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng.Rational (defEps)
import Flight.Distance (SpanLatLng)
import Flight.Zone (Zone(..))
import qualified Flight.Earth.Sphere.PointToPoint.Rational as Rat
    (distanceHaversine)
import qualified Distance as D

toDistanceEqual
    :: (Real a, Fractional a)
    => SpanLatLng a
    -> Quantity a [u| m |]
    -> String
    -> (Zone a, Zone a)
    -> TestTree
toDistanceEqual span = D.toDistanceEqual span

toDistanceClose
    :: (Real a, Fractional a)
    => SpanLatLng a
    -> Quantity a [u| mm |]
    -> Quantity a [u| m |]
    -> String
    -> (Zone a, Zone a)
    -> TestTree
toDistanceClose span = D.toDistanceClose span
