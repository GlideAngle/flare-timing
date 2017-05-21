module Main (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

import LaunchValidity
import DistanceValidity
import TimeValidity
import TaskValidity
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
        , taskValidityUnits
        , weightingUnits
        ]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
    [ SC.testProperty "Launch validity is in the range of [0, 1]" scLaunchValidity
    , SC.testProperty "Distance validity is in the range of [0, 1]" scDistanceValidity
    , SC.testProperty "Time validity is in the range of [0, 1]" scTimeValidity
    , SC.testProperty "Task validity is in the range of [0, 1]" taskValidity
    , SC.testProperty "Distance weight is in the range of [0, 1]" distanceWeight
    , SC.testProperty "Arrival weight is in the range of [0, 1], PG" arrivalWeightPgZ
    , SC.testProperty "Arrival weight is in the range of [0, 1]" arrivalWeight
    , SC.testProperty "Leading weight is in the range of [0, 1]" leadingWeight

    -- WARNING: Failing test.
    --  there exists TwTest (DistanceWeight (1 % 1),LeadingWeight (0 % 1),ArrivalWeight (1 % 1)) such that
    --    condition is false
    , SC.testProperty "Time weight is in the range of [0, 1]" timeWeight
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "Launch validity is in the range of [0, 1]" qcLaunchValidity
    , QC.testProperty "Distance validity is in the range of [0, 1]" qcDistanceValidity
    , QC.testProperty "Time validity is in the range of [0, 1]" qcTimeValidity
    , QC.testProperty "Task validity is in the range of [0, 1]" taskValidity
    , QC.testProperty "Distance weight is in the range of [0, 1]" distanceWeight
    , QC.testProperty "Arrival weight is in the range of [0, 1], PG" arrivalWeightPgZ
    , QC.testProperty "Arrival weight is in the range of [0, 1]" arrivalWeight
    , QC.testProperty "Leading weight is in the range of [0, 1]" leadingWeight

    -- WARNING: Failing test.
    --  *** Failed! Falsifiable (after 1 test):
    --  TwTest (DistanceWeight (1 % 1),LeadingWeight (1 % 1),ArrivalWeight (1 % 1))
    --  Use --quickcheck-replay '0 TFGenR 00000069EA3E9D9B0000000000989680000000000000E22500000914AB200BC0 0 128 8 0' to reproduce.
    , QC.testProperty "Time weight is in the range of [0, 1]" timeWeight
    ]
