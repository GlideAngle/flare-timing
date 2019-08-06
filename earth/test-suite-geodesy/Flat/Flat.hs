module Flat.Flat (properties, units, tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QC

import Flat.General
    (zoneUnits, distancePoint, distanceEuclidean, distanceEuclideanF)

tests :: TestTree
tests =
    testGroup
    "On a flat Earth (UTM Projection) using Pythagoras' theorem a² + b² = c²"
    [ units
    , properties
    ]

properties :: TestTree
properties =
    testGroup "Property tests on the flat Earth using Pythagoras' theorem a² + b² = c²"
    [ qcProps
    ]

units :: TestTree
units =
    testGroup "Unit tests on the flat Earth using Pythagoras' theorem a² + b² = c²"
    [ zoneUnits
    ]

qcProps :: TestTree
qcProps =
    testGroup "(checked by QuickCheck)"
    [ QC.testProperty "EuclideanF distances, are not negative" distanceEuclideanF
    , QC.testProperty "Euclidean distances, are not negative" distanceEuclidean
    , QC.testProperty "Zone distances, point-to-point, are not negative" distancePoint
    ]
