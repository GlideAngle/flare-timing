module Ellipsoid.Ellipsoid (tests) where

import Test.Tasty (TestTree, testGroup)

import Ellipsoid.Cylinder.Outer (outerCylinderUnits)
import Ellipsoid.Cylinder.Inner (innerCylinderUnits)

tests :: TestTree
tests =
    testGroup
    "On the WGS84 ellipsoid using Vincenty's solution to the inverse geodetic problem"
    [ outerCylinderUnits
    , innerCylinderUnits
    ]
