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
        , stopValidityUnits
        , scoreTimeWindowUnits
        , applyGlideUnits
        ]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
    [ SC.testProperty "Launch validity is in the range of [0, 1]" scLaunchValidity
    -- WARNING: Takes a long time.
    --, SC.testProperty "Distance validity is in the range of [0, 1]" scDistanceValidity
    --, SC.testProperty "Time validity is in the range of [0, 1]" scTimeValidity
    , SC.testProperty "Task validity is in the range of [0, 1]" taskValidity
    , SC.testProperty "Distance weight is in the range of [0, 1]" distanceWeight
    , SC.testProperty "Arrival weight is in the range of [0, 1]" arrivalWeight
    , SC.testProperty "Leading weight is in the range of [0, 1]" leadingWeight
    , SC.testProperty "Time weight is in the range of [0, 1]" timeWeight
    , SC.testProperty "Arrival fraction is in the range of [0.2, 1]" arrivalFraction
    , SC.testProperty "Speed fraction pilot time is not less than best time" speedFractionInputs
    , SC.testProperty "Speed fraction is in the range of [0, 1]" speedFraction
    , SC.testProperty "Linear distance fraction is in the range of [0, 1]" linearFraction
    , SC.testProperty "Difficulty lookahead is in the range of [30, 30 * best flown] chunks" lookahead
    -- WARNING: Takes a long time.
    --, SC.testProperty "Difficulty fraction is in the range of [0, 1]" difficulty
    , SC.testProperty "A cleaned track is smaller if there is some flying away from goal" cleanTrack 
    --, SC.testProperty "Leading fraction is in the range of [0, 1]" leadingFractions
    , SC.testProperty "Task points add up with Hg penalties" taskPointsHg
    , SC.testProperty "Task points add up with Pg penalties" taskPointsPg
    , SC.testProperty "Stop task time from announced time, Hg" stopTaskTimeHg
    , SC.testProperty "Stop task time from announced time, Pg" stopTaskTimePg
    , SC.testProperty "Can score a stopped task, Hg" canScoreStoppedHg
    , SC.testProperty "Can score a stopped task, Pg" canScoreStoppedPg
    , SC.testProperty "Stopped validity is in the range of [0, 1]" stopValidity
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
    , QC.testProperty "Arrival weight is in the range of [0, 1]" arrivalWeight
    , QC.testProperty "Leading weight is in the range of [0, 1]" leadingWeight
    , QC.testProperty "Time weight is in the range of [0, 1]" timeWeight
    , QC.testProperty "Arrival fraction is in the range of [0.2, 1]" arrivalFraction
    , QC.testProperty "Speed fraction pilot time is not less than best time" speedFractionInputs
    , QC.testProperty "Speed fraction is in the range of [0, 1]" speedFraction
    , QC.testProperty "Linear distance fraction is in the range of [0, 1]" linearFraction
    , QC.testProperty "Difficulty lookahead is in the range of [30, 30 * best flown] chunks" lookahead
    -- WARNING: Takes a long time.
    --, QC.testProperty "Difficulty fraction is in the range of [0, 1]" difficulty
    , QC.testProperty "A cleaned track is smaller if there is some flying away from goal" cleanTrack 
    --, QC.testProperty "Leading fraction is in the range of [0, 1]" leadingFractions
    , QC.testProperty "Task points add up with Hg penalties" taskPointsHg
    , QC.testProperty "Task points add up with Pg penalties" taskPointsPg
    , QC.testProperty "Stop task time from announced time, Hg" stopTaskTimeHg
    , QC.testProperty "Stop task time from announced time, Pg" stopTaskTimePg
    , QC.testProperty "Can score a stopped task, Hg" canScoreStoppedHg
    , QC.testProperty "Can score a stopped task, Pg" canScoreStoppedPg
    , QC.testProperty "Stopped validity is in the range of [0, 1]" stopValidity
    , QC.testProperty "Score time window is in the range [0, stop time]" scoreTimeWindow
    , QC.testProperty "Stopped track has glide distance bonus" applyGlide
    ]
