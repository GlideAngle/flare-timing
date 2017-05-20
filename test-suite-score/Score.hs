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

    -- WARNING: Failing test.
    --  there exists GrTest (2 % 1) such that
    --    condition is false
    , SC.testProperty "Distance weight is in the range of [0, 1]" distanceWeight

    , SC.testProperty "Arrival weight is in the range of [0, 1], PG" arrivalWeightPgZ

    -- WARNING: Failing test.
    --  there exists AwTest (AwHg (2 % 1)) such that
    --    condition is false
    , SC.testProperty "Arrival weight is in the range of [0, 1]" arrivalWeight

    -- WARNING: Failing test.
    --  there exists LwTest (LwHg (2 % 1)) such that
    --    condition is false
    , SC.testProperty "Leading weight is in the range of [0, 1]" leadingWeight

    -- WARNING: Failing test.
    --  there exist 0 % 1 0 % 1 (-1) % 1 such that
    --    condition is false
    , SC.testProperty "Time weight is in the range of [0, 1]" timeWeight
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "Launch validity is in the range of [0, 1]" qcLaunchValidity
    , QC.testProperty "Distance validity is in the range of [0, 1]" qcDistanceValidity
    , QC.testProperty "Time validity is in the range of [0, 1]" qcTimeValidity

    -- WARNING: Failing test.
    --  *** Failed! Falsifiable (after 6 tests):
    --  GrTest (5 % 2)
    --  Use --quickcheck-replay '5 TFGenR 000000320C612AAD00000000002625A0000000000000E224000013E52B9ABE00 0 504 9 0' to reproduce.
    , QC.testProperty "Distance weight is in the range of [0, 1]" distanceWeight

    , QC.testProperty "Arrival weight is in the range of [0, 1], PG" arrivalWeightPgZ

    -- WARNING: Failing test.
    --  *** Failed! Falsifiable (after 3 tests):
    --  AwTest (AwHg (2 % 1))
    --  Use --quickcheck-replay '2 TFGenR 000000045CB5FCCD000000000004C4B4000000000000E224000013F3B8E50F00 0 224 8 0' to reproduce.
    , QC.testProperty "Arrival weight is in the range of [0, 1]" arrivalWeight

    -- WARNING: Failing test.
    --  *** Failed! Falsifiable (after 4 tests):
    --  LwTest (LwHg (3 % 2))
    --  Use --quickcheck-replay '3 TFGenR 000000320C612AAD00000000002625A0000000000000E224000013E52B9ABE00 0 960 10 0' to reproduce.
    , QC.testProperty "Leading weight is in the range of [0, 1]" leadingWeight

    -- WARNING: Failing test.
    --  *** Failed! Falsifiable (after 2 tests and 4 shrinks):
    --  0 % 1
    --  0 % 1
    --  5933387054035 % 3335967451731
    --  Use --quickcheck-replay '1 TFGenR 000000320C612AAD00000000002625A0000000000000E224000013E52B9ABE00 0 384 9 0' to reproduce.
    , QC.testProperty "Time weight is in the range of [0, 1]" timeWeight
    ]
