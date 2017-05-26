module Main (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

import LaunchValidity
import DistanceValidity
import TimeValidity
import TaskValidity
import Weighting
import ArrivalFraction
import SpeedFraction
import LinearFraction
import DifficultyFraction
import LeadingCoefficient
import Points

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
units = testGroup
            "Units"
            [ validityUnits
            , fractionUnits
            , pointUnits
            ]

validityUnits :: TestTree
validityUnits =
    testGroup
        "Validities"
        [ launchValidityUnits
        , distanceValidityUnits
        , timeValidityUnits
        , taskValidityUnits
        ]

fractionUnits :: TestTree
fractionUnits =
    testGroup
        "Fractions"
        [ weightingUnits
        , arrivalFractionUnits
        , speedFractionUnits
        , linearFractionUnits
        , difficultyFractionUnits
        , leadingCoefficientUnits
        ]

pointUnits :: TestTree
pointUnits =
    testGroup
        "Points"
        [ tallyUnits
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
    , SC.testProperty "Time weight is in the range of [0, 1]" timeWeight
    , SC.testProperty "Arrival fraction is in the range of [0.2, 1]" arrivalFraction
    , SC.testProperty "Speed fraction pilot time is not less than best time" speedFractionInputs
    , SC.testProperty "Speed fraction is in the range of [0, 1]" speedFraction
    , SC.testProperty "Linear distance fraction is in the range of [0, 1]" linearFraction
    , SC.testProperty "Difficulty lookahead is in the range of [30, 30 * best flown] chunks" lookaheadChunks
    , SC.testProperty "Difficulty fraction is in the range of [0, 1]" difficultyFraction
    , SC.testProperty "A cleaned track is smaller if there is some flying away from goal" cleanTrack 
    , SC.testProperty "Leading fraction is in the range of [0, 1]" leadingFractions
    , SC.testProperty "Task points add up" taskPoints
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
    , QC.testProperty "Time weight is in the range of [0, 1]" timeWeight
    , QC.testProperty "Arrival fraction is in the range of [0.2, 1]" arrivalFraction
    , QC.testProperty "Speed fraction pilot time is not less than best time" speedFractionInputs
    , QC.testProperty "Speed fraction is in the range of [0, 1]" speedFraction
    , QC.testProperty "Linear distance fraction is in the range of [0, 1]" linearFraction
    , QC.testProperty "Difficulty lookahead is in the range of [30, 30 * best flown] chunks" lookaheadChunks
    , QC.testProperty "Difficulty fraction is in the range of [0, 1]" difficultyFraction
    , QC.testProperty "A cleaned track is smaller if there is some flying away from goal" cleanTrack 
    , QC.testProperty "Leading fraction is in the range of [0, 1]" leadingFractions

    -- WARNING: Failing test.
    --        *** Failed! Falsifiable (after 2 tests):
    --        PtTest (TaskPenalties {earlyStart = Just (EarlyStartPg (LaunchToSssPoints (9098551556919 % 219231563575))), noGoal = Just NoGoalHg},TaskPointParts {distance = 818621382539 % 568492668514, leading = 7851487824439 % 7639617044771, time = 1377098268643 % 1256374703148, arrival = 6816961050973 % 1206749894820})
    --        Use --quickcheck-replay '1 TFGenR 00000080366A0C890000000000989680000000000000E22B000087B30F43D740 0 196608 18 0' to reproduce.
    , QC.testProperty "Task points add up" taskPoints
    ]
