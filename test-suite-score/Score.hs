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

    -- WARNING: Failing test.
    --  *** Failed! Falsifiable (after 2 tests):
    --  (-930403447992) % 4158160448795
    --  Use --quickcheck-replay '1 TFGenR 0000001BDE14F45100000000001E8480000000000000E22400001069DD92DCC0 0 24 5 0' to reproduce.
    , QC.testProperty "Distance weight is in the range of [0, 1]" qcDistanceWeight

    , QC.testProperty "Arrival weight is in the range of [0, 1], PG" qcArrivalWeightPgZ

    -- WARNING: Failing test.
    --  *** Failed! Falsifiable (after 3 tests):
    --  AwTest (AwHg (2 % 1))
    --  Use --quickcheck-replay '2 TFGenR 000000045CB5FCCD000000000004C4B4000000000000E224000013F3B8E50F00 0 224 8 0' to reproduce.
    , QC.testProperty "Arrival weight is in the range of [0, 1]" qcArrivalWeight

    -- WARNING: Failing test.
    --  *** Failed! Falsifiable (after 8 tests):
    --  LwTest (LwPg (5 % 2))
    --  Use --quickcheck-replay '7 TFGenR 000000045CB5FCCD000000000004C4B4000000000000E224000013F3B8E50F00 0 16320 14 0' to reproduce.
    , QC.testProperty "Leading weight is in the range of [0, 1]" qcLeadingWeight

    -- WARNING: Failing test.
    --  *** Failed! Falsifiable (after 2 tests and 2 shrinks):
    --  4687285062235 % 8827846269843
    --  6659634358439 % 6666915772746
    --  0 % 1
    --  Use --quickcheck-replay '1 TFGenR 000000045CB5FCCD000000000004C4B4000000000000E224000013F3B8E50F00 0 384 9 0' to reproduce.
    , QC.testProperty "Time weight is in the range of [0, 1]" qcTimeWeight
    ]
