module Sphere.Sphere (tests) where

import Test.Tasty (TestTree, testGroup)

import Sphere.Cylinder.Outer (outerCylinderUnits)
import Sphere.Cylinder.Inner (innerCylinderUnits)

tests :: TestTree
tests =
    testGroup "On the FAI sphere using haversines"
    [ outerCylinderUnits
    , innerCylinderUnits
    ]
