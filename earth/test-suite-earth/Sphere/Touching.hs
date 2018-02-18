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

module Sphere.Touching (touchingUnits) where

import Prelude hiding (span)
import Data.Ratio ((%))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.UnitsOfMeasure ((*:), u, convert, negate')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng.Rational (defEps)
import Flight.Distance (SpanLatLng)
import Flight.Zone (Zone(..), Radius(..), showZoneDMS, fromRationalZone)
import qualified Flight.Earth.Sphere.PointToPoint.Rational as Rat (distanceHaversine)
import Flight.Earth.Sphere.Separated (separatedZones)
import Flight.Earth.Sphere (earthRadius)

import Zone (QLL, cylinder, line, conical, semicircle)

span :: SpanLatLng Rational
span = Rat.distanceHaversine defEps

touchingUnits :: TestTree
touchingUnits =
    testGroup "Touching zone separation"
    [ cylinderTouching
    , conicalTouching
    , lineTouching
    , semicircleTouching
    ]

touching :: String -> [[Zone Rational]] -> TestTree
touching title xs =
    testGroup title (f <$> xs)
    where
        f x =
            HU.testCase
                (mconcat
                    [ "touching pair of "
                    , showZoneDMS . fromRationalZone . head $ x
                    , " = not separate"
                    ]
                )
                $ separatedZones span x
                    @?= False

eps :: Quantity Rational [u| 1 |]
eps = MkQuantity $ 2 % 1 - 1 % 100000000

pts :: [(QLL Rational, QLL Rational)]
pts =
    [ ((z, pos), (z, z))
    , ((z, neg), (z, z))
    ]
    where
        z = [u| 0 rad |]
        pos = convert $ eps *: [u| 1 deg |]
        neg = convert $ negate' eps *: [u| -1 deg |]

cylinderTouching :: TestTree
cylinderTouching =
    touching
    "Cylinder zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = cylinder $ Radius earthRadius

conicalTouching :: TestTree
conicalTouching =
    touching
    "Conical zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = conical $ Radius earthRadius

lineTouching :: TestTree
lineTouching =
    touching
    "Line zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = line $ Radius earthRadius

semicircleTouching :: TestTree
semicircleTouching =
    touching
    "Semicircle zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = semicircle $ Radius earthRadius
