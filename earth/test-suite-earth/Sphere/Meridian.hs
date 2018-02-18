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
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Sphere.Meridian (meridianUnits) where

import Prelude hiding (span)
import Data.List (intersperse)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng.Rational (defEps)
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng)
import Flight.Zone (Zone(..), Radius(..), showZoneDMS, fromRationalZone)
import Flight.Zone.Path (distancePointToPoint)
import qualified Flight.Earth.Sphere.PointToPoint.Rational as Rat (distanceHaversine)
import Flight.Earth.Sphere (earthRadius)

import Zone (QLL, point, vector, cylinder, line, conical, semicircle)

span :: SpanLatLng Rational
span = Rat.distanceHaversine defEps

meridianUnits :: TestTree
meridianUnits =
    testGroup "Point-to-point distance"
    [ emptyDistance
    , pointDistance
    , vectorDistance
    , cylinderDistance
    , conicalDistance
    , lineDistance
    , semicircleDistance
    ]

emptyDistance :: TestTree
emptyDistance =
    testGroup "Point-to-point distance"
    [ HU.testCase "No zones = zero point-to-point distance" $
        edgesSum (distancePointToPoint span []) @?= (TaskDistance $ MkQuantity 0)
    ]

toDistance :: String -> [[Zone Rational]] -> TestTree
toDistance title xs =
    testGroup title (f <$> xs)
    where
        f x =
            HU.testCase
                (mconcat
                    ([ "distance [" ]
                    ++ intersperse ", " (showZoneDMS . fromRationalZone <$> x)
                    ++ [ "] = earth radius" ])
                )
                $
                edgesSum (distancePointToPoint span x)
                    @?= TaskDistance earthRadius

pts :: [(QLL Rational, QLL Rational)]
pts =
    [ ((m, z), (m, z))
    , ((z, m), (z, m))
    , ((m, z), (m, z))
    , ((m, m), (m, m))
    ]
    where
        z = [u| 0 rad |]
        m = convert [u| 45 deg |]

pointDistance :: TestTree
pointDistance =
    toDistance
    "Distance over point zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = point $ Radius earthRadius

vectorDistance :: TestTree
vectorDistance =
    toDistance
    "Distance over vector zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = vector $ Radius earthRadius

cylinderDistance :: TestTree
cylinderDistance =
    toDistance
    "Distance over cylinder zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = cylinder $ Radius earthRadius

conicalDistance :: TestTree
conicalDistance =
    toDistance
    "Distance over conical zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = conical $ Radius earthRadius

lineDistance :: TestTree
lineDistance =
    toDistance
    "Distance over line zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = line $ Radius earthRadius

semicircleDistance :: TestTree
semicircleDistance =
    toDistance
    "Distance over semicircle zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = semicircle $ Radius earthRadius

