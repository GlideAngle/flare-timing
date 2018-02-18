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

module Sphere.Coincident (coincidentUnits) where

import Prelude hiding (span)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.UnitsOfMeasure (u, convert)

import Flight.LatLng.Rational (defEps)
import Flight.Distance (SpanLatLng)
import Flight.Zone (Zone(..), Radius(..))
import qualified Flight.Earth.Sphere.PointToPoint.Rational as Rat (distanceHaversine)
import Flight.Earth.Sphere.Separated (separatedZones)
import Flight.Earth.Sphere (earthRadius)

import Zone (QLL, point, vector, cylinder, line, conical, semicircle)

span :: SpanLatLng Rational
span = Rat.distanceHaversine defEps

coincidentUnits :: TestTree
coincidentUnits =
    testGroup "Coincident zone separation"
    [ pointCoincident
    , vectorCoincident
    , cylinderCoincident
    , conicalCoincident
    , lineCoincident
    , semicircleCoincident
    ]

coincident :: String -> [[Zone Rational]] -> TestTree
coincident title xs =
    testGroup title (f <$> xs)
    where
        f x =
            HU.testCase (mconcat [ "concident pair of "
                                 , show $ head x
                                 , " = not separate"
                                 ]) $
                separatedZones span x
                    @?= False

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

pointCoincident :: TestTree
pointCoincident =
    coincident
    "Point zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = point $ Radius earthRadius

vectorCoincident :: TestTree
vectorCoincident =
    coincident
    "Vector zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = vector $ Radius earthRadius

cylinderCoincident :: TestTree
cylinderCoincident =
    coincident
    "Cylinder zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = cylinder $ Radius earthRadius

conicalCoincident :: TestTree
conicalCoincident =
    coincident
    "Conical zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = conical $ Radius earthRadius

lineCoincident :: TestTree
lineCoincident =
    coincident
    "Line zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = line $ Radius earthRadius

semicircleCoincident :: TestTree
semicircleCoincident =
    coincident
    "Semicircle zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = semicircle $ Radius earthRadius
