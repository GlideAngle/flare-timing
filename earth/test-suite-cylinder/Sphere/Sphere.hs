module Sphere.Sphere (tests, testsR) where

import Test.Tasty (TestTree, testGroup)

import Sphere.Cylinder.Outer (outerUnits, outerUnitsR)
import Sphere.Cylinder.Inner (innerUnits, innerUnitsR)

tests :: TestTree
tests =
    testGroup "On the FAI sphere using haversines"
    [ outerUnits
    , innerUnits
    ]

testsR :: TestTree
testsR =
    testGroup "On the FAI sphere using haversines"
    [ outerUnitsR
    , innerUnitsR
    ]
