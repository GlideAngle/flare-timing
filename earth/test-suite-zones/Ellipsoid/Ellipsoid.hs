module Ellipsoid.Ellipsoid (units) where

import Test.Tasty (TestTree, testGroup)

import Ellipsoid.Coincident (coincidentUnits)

units :: TestTree
units =
    testGroup
    "On the WGS84 ellipsoid using Vincenty's solution to the inverse geodetic problem"
    [ coincidentUnits
    ]
