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
import Data.UnitsOfMeasure ((+:), (*:), u, negate')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone (Zone(..), Radius(..))
import Flight.Earth.Sphere (earthRadius)

import Zone (QLL, point, vector, cylinder, line, conical, semicircle)
import Sphere.Touching (Overlay(..), separatedZones)

disjointUnits :: TestTree
disjointUnits =
    testGroup "Disjoint zones are separated"
    [ pointDisjoint
    , vectorDisjoint
    , cylinderDisjoint
    , conicalDisjoint
    , lineDisjoint
    , semicircleDisjoint
    ]

disjoint :: String -> [[Zone Rational]] -> TestTree
disjoint = separatedZones Disjoint delta radius

delta :: Quantity Rational [u| 1 |]
delta = MkQuantity $ 1 % 100000000

radius :: Quantity Rational [u| 1 |]
radius = MkQuantity $ 2 % 1

pts :: [(QLL Rational, QLL Rational)]
pts =
    [ ((z, z), (z, pos))
    , ((z, z), (z, neg))
    ]
    where
        z :: Quantity Rational [u| rad |]
        z = [u| 0 rad |]

        pos = (radius +: delta) *: [u| 1 rad |]
        neg = negate' (radius +: delta) *: [u| 1 rad |]

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
