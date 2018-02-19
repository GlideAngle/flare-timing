{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Sphere.Distance where

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

span :: SpanLatLng Rational
span = Rat.distanceHaversine defEps

toDistanceEqual
    :: Quantity Rational [u| m |]
    -> String
    -> (Zone Rational, Zone Rational)
    -> TestTree
toDistanceEqual = D.toDistanceEqual span

toDistanceClose
    :: Quantity Rational [u| mm |]
    -> Quantity Rational [u| m |]
    -> String
    -> (Zone Rational, Zone Rational)
    -> TestTree
toDistanceClose = D.toDistanceClose span
