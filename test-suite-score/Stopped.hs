{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
module Stopped where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)

import qualified Flight.Score as FS
import Flight.Score
    ( StopTime(..)
    , ScoreBackTime(..)
    , AnnouncedTime(..)
    , StartGateInterval(..)
    , TaskStopTime(..)
    )

stoppedTimeUnits :: TestTree
stoppedTimeUnits = testGroup "Effective task stop time"
    [ HU.testCase "Announced stop time minus score back time, Pg = task stop time" $
        FS.stopTaskTime (ScoreBackStop (ScoreBackTime 1) (AnnouncedTime 3)) @?= TaskStopTime 2

    , HU.testCase "Announced stop time minus time between start gates, Hg = task stop time" $
        FS.stopTaskTime (InterGateStop (StartGateInterval 1) (AnnouncedTime 3)) @?= TaskStopTime 2

    , HU.testCase "Announced stop time with a single start gate, Hg = task stop time is 15 min earlier" $
        FS.stopTaskTime (SingleGateStop (AnnouncedTime (17 * 60))) @?= TaskStopTime (2 * 60)
    ]
