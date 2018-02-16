module Sphere.Sphere (properties, units, tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

import Sphere.General
import Sphere.Specific
import Sphere.Bedford
import Sphere.Cylinder

tests :: TestTree
tests =
    testGroup "On the FAI sphere using haversines"
    [ units
    , properties
    ]

properties :: TestTree
properties =
    testGroup "Properties"
    [ scProps
    , qcProps
    ]

units :: TestTree
units =
    testGroup "Unit Tests"
    [ zoneUnits
    , forbesUnits
    , bedfordUnits
    , circumSampleUnits
    ]

scProps :: TestTree
scProps =
    testGroup "(checked by SmallCheck)"
    [ SC.testProperty "HaversineF distances, are not negative" distanceHaversineF
    , SC.testProperty "Haversine distances, are not negative" distanceHaversine
    , SC.testProperty "Zone distances, point-to-point, are not negative" distancePoint
    , SC.testProperty "Zone distances, point-to-point, are not negative" distancePoint
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "HaversineF distances, are not negative" distanceHaversineF
    , QC.testProperty "Haversine distances, are not negative" distanceHaversine
    , QC.testProperty "Zone distances, point-to-point, are not negative" distancePoint
    ]
