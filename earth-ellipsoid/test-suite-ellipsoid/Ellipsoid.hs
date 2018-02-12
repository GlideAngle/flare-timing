module Ellipsoid (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.QuickCheck as QC

import General
import Specific

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
        [ zoneUnits
        , specificUnits
        ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "VincentyF distances, are not negative" distanceVincentyF
    , QC.testProperty "Vincenty distances, are not negative" distanceVincenty
    , QC.testProperty "Zone distances, point-to-point, are not negative" distancePoint
    ]
