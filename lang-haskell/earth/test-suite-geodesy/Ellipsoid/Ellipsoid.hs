module Ellipsoid.Ellipsoid (properties) where

import Test.Tasty (testGroup)
import Test.Tasty.QuickCheck as QC
import Test.Tasty.Providers as QC

import Props.Vincenty (distancePoint, distanceVincentyF, distanceVincenty)

properties :: TestTree
properties =
    testGroup
    "On the WGS84 ellipsoid using Vincenty's solution to the inverse geodetic problem"
    [ qcProps
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "VincentyF distances, are not negative" distanceVincentyF
    , QC.singleTest
        "Vincenty distances, are not negative"
        $ QC.QC
        $ QC.withMaxSuccess 6
        $ QC.property distanceVincenty
    , QC.singleTest
        "Zone distances, point-to-point, are not negative"
        $ QC.QC
        $ QC.withMaxSuccess 2
        $ QC.property distancePoint
    ]
