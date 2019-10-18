module Flat.Flat (properties, units) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QC

import Flat.Distance (units, distancePoint, distanceEuclidean, distanceEuclideanF)

properties :: TestTree
properties =
    testGroup
    "On a flat Earth (UTM Projection) using Pythagoras' theorem a² + b² = c²"
    [ qcProps
    ]

qcProps :: TestTree
qcProps =
    testGroup "(checked by QuickCheck)"
    [ QC.testProperty "EuclideanF distances, are not negative" distanceEuclideanF
    , QC.testProperty "Euclidean distances, are not negative" distanceEuclidean
    , QC.testProperty "Zone distances, point-to-point, are not negative" distancePoint
    ]
