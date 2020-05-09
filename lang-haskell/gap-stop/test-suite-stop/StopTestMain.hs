module Main (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

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
units =
    testGroup
        "Stopped Task Units"
        [ stoppedTimeUnits
        , stoppedScoreUnits
        , scoreTimeWindowUnits
        , applyGlideUnits
        ]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
    [ SC.testProperty "Stop task time from announced time, Hg" stopTaskTimeHg
    , SC.testProperty "Stop task time from announced time, Pg" stopTaskTimePg
    , SC.testProperty "Can score a stopped task, Hg" canScoreStoppedHg
    , SC.testProperty "Can score a stopped task, Pg" canScoreStoppedPg
    , SC.testProperty "Score time window is in the range [0, stop time]" scoreTimeWindow
    , SC.testProperty "Stopped track has glide distance bonus" applyGlide
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "Stop task time from announced time, Hg" stopTaskTimeHg
    , QC.testProperty "Stop task time from announced time, Pg" stopTaskTimePg
    , QC.testProperty "Can score a stopped task, Hg" canScoreStoppedHg
    , QC.testProperty "Can score a stopped task, Pg" canScoreStoppedPg
    , QC.testProperty "Score time window is in the range [0, stop time]" scoreTimeWindow
    , QC.testProperty "Stopped track has glide distance bonus" applyGlide
    ]
