module Sphere (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.SmallCheck as SC
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
        [ scProps
        , qcProps
        ]

units :: TestTree
units = testGroup "Units"
        [ zoneUnits
        , specificUnits
        ]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
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
