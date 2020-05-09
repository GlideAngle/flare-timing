module Main (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

import LaunchValidity
import DistanceValidity
import TimeValidity
import TaskValidity
import StopValidity

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ units
        , properties
        ]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

units :: TestTree
units = testGroup "Units" [validityUnits]

validityUnits :: TestTree
validityUnits =
    testGroup
        "Validities"
        [ launchValidityUnits
        , distanceValidityUnits
        , timeValidityUnits
        , taskValidityUnits
        , stopValidityUnits
        ]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
    [ SC.testProperty "Launch validity is in the range of [0, 1]" scLaunchValidity
    -- WARNING: Takes a long time.
    --, SC.testProperty "Distance validity is in the range of [0, 1]" scDistanceValidity
    --, SC.testProperty "Time validity is in the range of [0, 1]" scTimeValidity
    , SC.testProperty "Task validity is in the range of [0, 1]" taskValidity
    , SC.testProperty "Stopped validity is in the range of [0, 1]" stopValidity
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "Launch validity is in the range of [0, 1]" qcLaunchValidity
    , QC.testProperty "Distance validity is in the range of [0, 1]" qcDistanceValidity
    , QC.testProperty "Time validity is in the range of [0, 1]" qcTimeValidity
    , QC.testProperty "Task validity is in the range of [0, 1]" taskValidity
    , QC.testProperty "Stopped validity is in the range of [0, 1]" stopValidity
    ]
