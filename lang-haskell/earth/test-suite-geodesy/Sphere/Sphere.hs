module Sphere.Sphere (properties) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QC

import Props.Haversine (distancePoint, distanceHaversineF, distanceHaversine)

properties :: TestTree
properties =
    testGroup "Property tests on the FAI sphere using haversines"
    [ qcProps
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty
        "HaversineF distances, are not negative"
        distanceHaversineF

    , QC.testProperty
        "Haversine distances, are not negative"
        distanceHaversine

    , QC.testProperty
        "Zone distances, point-to-point, are not negative"
        distancePoint
    ]
