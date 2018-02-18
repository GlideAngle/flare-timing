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

module Sphere.Disjoint (disjointUnits) where

import Prelude hiding (span)
import Data.Ratio ((%))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.UnitsOfMeasure ((*:), u, convert, negate')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng.Rational (defEps)
import Flight.Distance (SpanLatLng)
import Flight.Zone (Zone(..), Radius(..))
import qualified Flight.Earth.Sphere.PointToPoint.Rational as Rat (distanceHaversine)
import Flight.Earth.Sphere.Separated (separatedZones)
import Flight.Earth.Sphere (earthRadius)

import Zone (QLL, point, vector, cylinder, line, conical, semicircle)

span :: SpanLatLng Rational
span = Rat.distanceHaversine defEps

disjointUnits :: TestTree
disjointUnits =
    testGroup "Disjoint zone separation"
    [ pointDisjoint
    , vectorDisjoint
    , cylinderDisjoint
    , conicalDisjoint
    , lineDisjoint
    , semicircleDisjoint
    ]

disjoint :: String -> [[Zone Rational]] -> TestTree
disjoint title xs =
    testGroup title (f <$> xs)
    where
        f x =
            HU.testCase (mconcat [ "disjoint pair of "
                                 , show x 
                                 , " = separate"
                                 ]) $
                separatedZones span x
                    @?= True

eps :: Quantity Rational [u| 1 |]
eps = MkQuantity $ 2 % 1 + 1 % 100000000

pts :: [(QLL Rational, QLL Rational)]
pts =
    [ ((z, pos), (z, z))
    , ((z, neg), (m, z))
    ]
    where
        z = [u| 0 rad |]
        m = convert [u| 45 deg |]
        pos = convert $ eps *: [u| 1 deg |]
        neg = convert $ negate' eps *: [u| 1 deg |]

pointDisjoint :: TestTree
pointDisjoint =
    disjoint
    "Point zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = point $ Radius earthRadius

vectorDisjoint :: TestTree
vectorDisjoint =
    disjoint
    "Vector zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = vector $ Radius earthRadius

cylinderDisjoint :: TestTree
cylinderDisjoint =
    disjoint
    "Cylinder zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = cylinder $ Radius earthRadius

conicalDisjoint :: TestTree
conicalDisjoint =
    disjoint
    "Conical zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = conical $ Radius earthRadius

lineDisjoint :: TestTree
lineDisjoint =
    disjoint
    "Line zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = line $ Radius earthRadius

semicircleDisjoint :: TestTree
semicircleDisjoint =
    disjoint
    "Semicircle zones"
    ((\(x, y) -> [f x, f y]) <$> pts)
    where
        f = semicircle $ Radius earthRadius
