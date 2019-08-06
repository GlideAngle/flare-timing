module Ellipsoid.Ellipsoid (tests, testsR) where

import Test.Tasty (TestTree, testGroup)

import Ellipsoid.Cylinder.Outer (outerUnits, outerUnitsR)
import Ellipsoid.Cylinder.Inner (innerUnits, innerUnitsR)

tests :: TestTree
tests =
    testGroup
    "On the WGS84 ellipsoid using Vincenty's solution to the inverse geodetic problem"
    [ outerUnits
    , innerUnits
    ]

testsR :: TestTree
testsR =
    testGroup
    "On the WGS84 ellipsoid using Vincenty's solution to the inverse geodetic problem"
    [ outerUnitsR
    , innerUnitsR
    ]
