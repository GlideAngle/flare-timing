module Flat.Flat (properties, units, tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

import Flat.General
import Flat.Bedford
import Flat.Forbes

tests :: TestTree
tests =
    testGroup
    "On a flat Earth (UTM Projection) using Pythagoras' theorem a² + b² = c²"
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
    testGroup "Units"
    [ forbesUnits
    , bedfordUnits
    , zoneUnits
    ]

scProps :: TestTree
scProps =
    testGroup "(checked by SmallCheck)"
    [ SC.testProperty "EuclideanF distances, are not negative" distanceEuclideanF
    , SC.testProperty "Euclidean distances, are not negative" distanceEuclidean
    , SC.testProperty "Zone distances, point-to-point, are not negative" distancePoint
    , SC.testProperty "Zone distances, point-to-point, are not negative" distancePoint
    ]

qcProps :: TestTree
qcProps =
    testGroup "(checked by QuickCheck)"
    [ QC.testProperty "EuclideanF distances, are not negative" distanceEuclideanF
    , QC.testProperty "Euclidean distances, are not negative" distanceEuclidean
    , QC.testProperty "Zone distances, point-to-point, are not negative" distancePoint
    ]
