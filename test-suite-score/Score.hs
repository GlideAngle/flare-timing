module Main (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

import LaunchValidity
import DistanceValidity
import TimeValidity
import Weighting

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ properties
        , units
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
        , weightingUnits
        ]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
    [ SC.testProperty "Launch validity is in the range of [0, 1]" scLaunchValidity

    , SC.testProperty "Distance validity is in the range of [0, 1]" scDistanceValidity

    , SC.testProperty "Time validity is in the range of [0, 1]" scTimeValidity
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "Launch validity is in the range of [0, 1]" qcLaunchValidity

    , QC.testProperty "Distance validity is in the range of [0, 1]" qcDistanceValidity

    , QC.testProperty "Time validity is in the range of [0, 1]" qcTimeValidity
    ]
