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
import Stopped

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
            , stoppedUnits
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
        , difficultyUnits
        , leadingCoefficientUnits
        ]

pointUnits :: TestTree
pointUnits =
    testGroup
        "Points"
        [ tallyUnits
        ]

stoppedUnits :: TestTree
stoppedUnits =
    testGroup
        "Stopped Task"
        [ stoppedTimeUnits
        , stoppedScoreUnits
        , stoppedValidityUnits
        , scoreTimeWindowUnits
        , applyGlideUnits 
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
    , SC.testProperty "Difficulty lookahead is in the range of [30, 30 * best flown] chunks" lookahead
    , SC.testProperty "Difficulty fraction is in the range of [0, 1]" difficulty
    , SC.testProperty "A cleaned track is smaller if there is some flying away from goal" cleanTrack 
    , SC.testProperty "Leading fraction is in the range of [0, 1]" leadingFractions
    , SC.testProperty "Task points add up with Hg penalties" taskPointsHg
    , SC.testProperty "Task points add up with Pg penalties" taskPointsPg
    , SC.testProperty "Stop task time from announced time, Hg" stopTaskTimeHg
    , SC.testProperty "Stop task time from announced time, Pg" stopTaskTimePg
    , SC.testProperty "Can score a stopped task, Hg" canScoreStoppedHg

    -- WARNING: Can score a stopped task, Pg fail.
    -- *** Failed! Falsifiable (after 89 tests):
    -- StopCanScoreTest (FromLastStart [954s,2943s,6346s,2637s,7257s,992s,8886s,3291s,3190s,3403s,1233s,5817s,2146s,3348s,7421s,7570s,552s,7939s,2780s,637s,7111s,3133s,2050s,9711s,7258s,7602s,4471s,159s,1571s,5648s,4294s,8953s,3913s,354s,1606s,9543s,7268s,1551s,5724s,9632s,9365s,8868s,5766s,1s,2082s,2874s,5297s,2137s,9252s,9823s,1795s,3996s,2084s,1261s,2809s,1707s,1184s,4387s,2575s,8657s,3559s,6273s,5586s,6422s,1735s,9599s,8674s,7363s,8094s,8125s,6898s,80s,4699s,3595s,2444s,2711s,102s,9515s,772s,1801s,4429s,3097s] (TaskStopTime (292652705910549 % 65505319040)))
    -- Use --quickcheck-replay=129015 to reproduce.
    , SC.testProperty "Can score a stopped task, Pg" canScoreStoppedPg
    , SC.testProperty "Stopped validity is in the range of [0, 1]" stoppedValidity
    , SC.testProperty "Score time window is in the range [0, stop time]" scoreTimeWindow
    , SC.testProperty "Stopped track has glide distance bonus" applyGlide
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
    , QC.testProperty "Difficulty lookahead is in the range of [30, 30 * best flown] chunks" lookahead
    , QC.testProperty "Difficulty fraction is in the range of [0, 1]" difficulty
    , QC.testProperty "A cleaned track is smaller if there is some flying away from goal" cleanTrack 
    , QC.testProperty "Leading fraction is in the range of [0, 1]" leadingFractions
    , QC.testProperty "Task points add up with Hg penalties" taskPointsHg
    , QC.testProperty "Task points add up with Pg penalties" taskPointsPg
    , QC.testProperty "Stop task time from announced time, Hg" stopTaskTimeHg
    , QC.testProperty "Stop task time from announced time, Pg" stopTaskTimePg
    , QC.testProperty "Can score a stopped task, Hg" canScoreStoppedHg
    , QC.testProperty "Can score a stopped task, Pg" canScoreStoppedPg
    , QC.testProperty "Stopped validity is in the range of [0, 1]" stoppedValidity
    , QC.testProperty "Score time window is in the range [0, stop time]" scoreTimeWindow
    , QC.testProperty "Stopped track has glide distance bonus" applyGlide
    ]
