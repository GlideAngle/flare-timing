module Sphere.Compare.Bedford (bedfordUnits) where

import Test.Tasty (TestTree, testGroup)

import Bedford (points, solutions)
import Sphere.Compare.SphereVersusEllipsoid (dblChecks, ratChecks)

bedfordUnits :: TestTree
bedfordUnits =
    testGroup "Bedford Institute of Oceanography distances"
    [ testGroup "with doubles" $ dblChecks solutions points
    , testGroup "with rationals" $ ratChecks solutions points
    ]
