{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
module Stopped (stoppedTimeUnits, stoppedScoreUnits) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)

import qualified Flight.Score as FS
import Flight.Score
    ( StopTime(..)
    , ScoreBackTime(..)
    , AnnouncedTime(..)
    , StartGateInterval(..)
    , TaskTime(..)
    , TaskStopTime(..)
    , CanScoreStopped(..)
    , NumberInGoalAtStop(..)
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

stoppedScoreUnits :: TestTree
stoppedScoreUnits = testGroup "Can score a stopped task?"
    [ HU.testCase "Not when noone made goal and the task ran less than an hour, Hg womens" $
        FS.canScoreStopped(Womens (NumberInGoalAtStop 0) (TaskStopTime $ 59 * 60)) @?= False

    , HU.testCase "When someone made goal, Hg womens" $
        FS.canScoreStopped(Womens (NumberInGoalAtStop 1) (TaskStopTime 0)) @?= True

    , HU.testCase "When the task ran for 1 hr, Hg womans" $
        FS.canScoreStopped(Womens (NumberInGoalAtStop 0) (TaskStopTime $ 60 * 60)) @?= True

    , HU.testCase "Not when noone made goal and the task ran less than 90 mins, Hg" $
        FS.canScoreStopped(GoalOrDuration (NumberInGoalAtStop 0) (TaskStopTime $ 89 * 60)) @?= False
    , HU.testCase "When someone made goal, Hg" $
        FS.canScoreStopped(GoalOrDuration (NumberInGoalAtStop 1) (TaskStopTime 0)) @?= True

    , HU.testCase "When the task ran for 90 mins, Hg" $
        FS.canScoreStopped(GoalOrDuration (NumberInGoalAtStop 0) (TaskStopTime $ 90 * 60)) @?= True

    , HU.testCase "When the task ran for 1 hr, Pg" $
        FS.canScoreStopped(FromGetGo (TaskStopTime $ 60 * 60)) @?= True

    , HU.testCase "Not when there are no starters, Pg" $
        FS.canScoreStopped(FromLastStart [] (TaskStopTime $ 120 * 60)) @?= False

    , HU.testCase "Not when the last start was less than an hour before stop, Pg" $
        FS.canScoreStopped(FromLastStart [TaskTime 0] (TaskStopTime $ 59 * 60)) @?= False

    , HU.testCase "When the last start was an hour before stop, Pg" $
        FS.canScoreStopped(FromLastStart [TaskTime 0] (TaskStopTime $ 60 * 60)) @?= True
    ]
