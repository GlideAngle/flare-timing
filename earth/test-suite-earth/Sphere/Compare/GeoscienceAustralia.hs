module Sphere.Compare.GeoscienceAustralia (geoSciAuUnits) where

import Test.Tasty (TestTree, testGroup)

import GeoscienceAustralia (points, solutions)
import Sphere.Compare.SphereVersusEllipsoid (dblChecks, ratChecks)

geoSciAuUnits :: TestTree
geoSciAuUnits =
    testGroup "Geoscience Australia distances between Flinders Peak and Buninyong"
    [ testGroup "with doubles" $ dblChecks solutions points
    , testGroup "with rationals" $ ratChecks solutions points
    ]
