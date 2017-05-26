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
    --        PtTest (TaskPenalties {earlyStart = Just (EarlyStartHg (SecondsPerPoint (1087298782847 % 459545533308)) (JumpedTheGun (7473202757887 % 727009849175))), noGoal = Nothing},TaskPointParts {distance = 4921461470319 % 3451021783192, leading = 7533350198503 % 636686678356, time = 1613148298820 % 6452842806801, arrival = 4830760331746 % 5669024148481})
    --        Use --quickcheck-replay '1 TFGenR 0000007EAD8980DF0000000000989680000000000000E22B000059D3A9FE8040 0 196608 18 0' to reproduce.
    , QC.testProperty "Task points add up" taskPoints
    ]
