module Sphere.Sphere (units) where

import Test.Tasty (TestTree, testGroup)

import Sphere.Touching (touchingUnits)
import Sphere.Coincident (coincidentUnits)
import Sphere.Disjoint (disjointUnits)

units :: TestTree
units =
    testGroup "On the FAI sphere using haversines"
        [ disjointUnits
        , touchingUnits
        , coincidentUnits
        ]
