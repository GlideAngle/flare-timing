module Ellipsoid (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.QuickCheck as QC
import Test.Tasty.Providers as QC

import General
import Specific
import Bedford

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
        [ units
        , properties
        ]

properties :: TestTree
properties = testGroup "Properties"
        [ qcProps
        ]

units :: TestTree
units = testGroup "Units"
        [ bedfordUnits
        , zoneUnits
        , specificUnits
        ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "VincentyF distances, are not negative" distanceVincentyF
    , QC.singleTest
        "Vincenty distances, are not negative"
        $ QC.QC
        $ QC.withMaxSuccess 10
        $ QC.property distanceVincenty
    , QC.singleTest
        "Zone distances, point-to-point, are not negative"
        $ QC.QC
        $ QC.withMaxSuccess 10
        $ QC.property distancePoint
    ]
