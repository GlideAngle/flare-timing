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

module Sphere.Greda (gredaUnits) where

import Prelude hiding (span)
import Test.Tasty (TestTree, TestName, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (LatLng(..), Lat(..), Lng(..))
import Flight.LatLng.Rational (defEps)
import Flight.Distance (TaskDistance(..), SpanLatLng)
import Flight.Zone (Zone(..), Radius(..), toRationalLatLng)
import Flight.Earth.Sphere.Separated (separatedZones)
import Flight.Zone.Path (distancePointToPoint)
import qualified Flight.Earth.Sphere.PointToPoint.Rational as Rat (distanceHaversine)

import Sphere.Cylinder.Span (spanR)
import Flight.Units.Angle (Angle(..))

gs1 :: LatLng Rational [u| rad |]
gs1 = toRationalLatLng . LatLng $
    ( Lat $ convert [u| 43.82972999 deg |]
    , Lng $ convert [u| 16.64243 deg |]
    )

g35 :: LatLng Rational [u| rad |]
g35 = toRationalLatLng . LatLng $
    ( Lat $ convert [u| 43.84411 deg |]
    , Lng $ convert [u| 16.6599 deg |]
    )

g44 :: LatLng Rational [u| rad |]
g44 = toRationalLatLng . LatLng $
    ( Lat $ convert [u| 43.82292999 deg |]
    , Lng $ convert [u| 16.61628999 deg |]
    )

g10 :: LatLng Rational [u| rad |]
g10 = toRationalLatLng . LatLng $
    ( Lat $ convert [u| 43.78045 deg |]
    , Lng $ convert [u| 16.65744 deg |]
    )

g36 :: LatLng Rational [u| rad |]
g36 = toRationalLatLng . LatLng $
    ( Lat $ convert [u| 43.82341999 deg |]
    , Lng $ convert [u| 16.58681999 deg |]
    )

g17 :: LatLng Rational [u| rad |]
g17 = toRationalLatLng . LatLng $
    ( Lat $ convert [u| 43.72863 deg |]
    , Lng $ convert [u| 16.69474999 deg |]
    )

g39 :: LatLng Rational [u| rad |]
g39 = toRationalLatLng . LatLng $
    ( Lat $ convert [u| 43.75677 deg |]
    , Lng $ convert [u| 16.62018 deg |]
    )
    
task1 :: [Zone Rational]
task1 =
    [ Cylinder (Radius [u|  400 m |]) gs1
    , Cylinder (Radius [u| 3000 m |]) g35
    , Cylinder (Radius [u|  400 m |]) g44
    , Cylinder (Radius [u|  400 m |]) g10
    , Cylinder (Radius [u| 1000 m |]) g36
    , Cylinder (Radius [u| 1000 m |]) g17
    , Cylinder (Radius [u| 1000 m |]) g39
    , Cylinder (Radius [u|  400 m |]) g39
    ]

gredaUnits :: TestTree
gredaUnits =
    testGroup "greda 2011/2012 distances"
    [ HU.testCase "Task 1 zones are separated" $
        separatedZones spanR task1 @?= True
    ]
