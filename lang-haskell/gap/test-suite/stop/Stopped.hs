module Stopped
    ( stoppedTimeUnits
    , stoppedScoreUnits
    , scoreTimeWindowUnits
    , applyGlideUnits
    , stopTaskTimeHg
    , stopTaskTimePg
    , canScoreStoppedHg
    , canScoreStoppedPg
    , scoreTimeWindow
    , applyGlide
    ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.UnitsOfMeasure (u, convert, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import qualified Flight.Score as FS
import Flight.Score
    ( StopTime(..)
    , ScoreBackTime(..)
    , AnnouncedTime(..)
    , StartGateInterval(..)
    , TaskTime(..)
    , TaskStopTime(..)
    , CanScoreStopped(..)
    , PilotsAtEss(..)
    , PilotsLanded(..)
    , PilotsFlying(..)
    , LaunchToEss(..)
    , StopValidity(..)
    , TaskType(..)
    , StartGates(..)
    , ScoreTimeWindow(..)
    , AltitudeAboveGoal(..)
    , DistanceToGoal(..)
    , GlideRatio(..)
    , StoppedTrack(..)
    , Hg
    , Pg
    , isNormal
    )

import TestNewtypes

stoppedTimeUnits :: TestTree
stoppedTimeUnits = testGroup "Effective task stop time"
    [ HU.testCase "Announced stop time minus score back time, Pg = task stop time" $
        FS.stopTaskTime (ScoreBackStop (ScoreBackTime [u| 1s |]) (AnnouncedTime 3)) @?= TaskStopTime 2

    , HU.testCase "Announced stop time minus time between start gates, Hg = task stop time" $
        FS.stopTaskTime (InterGateStop (StartGateInterval 1) (AnnouncedTime 3)) @?= TaskStopTime 2

    , HU.testCase "Announced stop time with a single start gate, Hg = task stop time is 15 min earlier" $
        FS.stopTaskTime (SingleGateStop (AnnouncedTime (17 * 60))) @?= TaskStopTime (2 * 60)
    ]

stoppedScoreUnits :: TestTree
stoppedScoreUnits = testGroup "Can score a stopped task?"
    [ HU.testCase "Not when noone made goal and the task ran less than an hour, Hg womens" $
        FS.canScoreStopped(Womens (PilotsAtEss 0) (TaskStopTime $ 59 * 60)) @?= False

    , HU.testCase "When someone made goal, Hg womens" $
        FS.canScoreStopped(Womens (PilotsAtEss 1) (TaskStopTime 0)) @?= True

    , HU.testCase "When the task ran for 1 hr, Hg womans" $
        FS.canScoreStopped(Womens (PilotsAtEss 0) (TaskStopTime $ 60 * 60)) @?= True

    , HU.testCase "Not when noone made goal and the task ran less than 90 mins, Hg" $
        FS.canScoreStopped(GoalOrDuration (PilotsAtEss 0) (TaskStopTime $ 89 * 60)) @?= False
    , HU.testCase "When someone made goal, Hg" $
        FS.canScoreStopped(GoalOrDuration (PilotsAtEss 1) (TaskStopTime 0)) @?= True

    , HU.testCase "When the task ran for 90 mins, Hg" $
        FS.canScoreStopped(GoalOrDuration (PilotsAtEss 0) (TaskStopTime $ 90 * 60)) @?= True

    , HU.testCase "When the task ran for 1 hr, Pg" $
        FS.canScoreStopped(FromGetGo (TaskStopTime $ 60 * 60)) @?= True

    , HU.testCase "Not when there are no starters, Pg" $
        FS.canScoreStopped(FromLastStart [] (TaskStopTime $ 120 * 60)) @?= False

    , HU.testCase "Not when the last start was less than an hour before stop, Pg" $
        FS.canScoreStopped(FromLastStart [TaskTime [u| 0 s |]] (TaskStopTime $ 59 * 60)) @?= False

    , HU.testCase "When the last start was an hour before stop, Pg" $
        FS.canScoreStopped(FromLastStart [TaskTime [u| 0 s |]] (TaskStopTime $ 60 * 60)) @?= True
    ]

scoreTimeWindowUnits :: TestTree
scoreTimeWindowUnits = testGroup "Score time window"
    [ testGroup "Race to goal"
        [ HU.testCase "1 start gate, noone launches = start to stop" $
            FS.scoreTimeWindow
                RaceToGoal
                (StartGates 1)
                (TaskStopTime 1)
                []
                @?= ScoreTimeWindow 1

        , HU.testCase "1 start gate, 1 launches at start = start to stop" $
            FS.scoreTimeWindow
                RaceToGoal
                (StartGates 1)
                (TaskStopTime 1)
                [TaskTime [u| 0 s |]]
                @?= ScoreTimeWindow 1

        , HU.testCase "1 start gate, 1 launches at stop = start to stop" $
            FS.scoreTimeWindow
                RaceToGoal
                (StartGates 1)
                (TaskStopTime 1)
                [TaskTime [u| 1 s |]]
                @?= ScoreTimeWindow 1

        , HU.testCase "2 start gates, noone launches = 0" $
            FS.scoreTimeWindow
                RaceToGoal
                (StartGates 2)
                (TaskStopTime 1)
                []
                @?= ScoreTimeWindow 0

        , HU.testCase "2 start gates, 1 launches at start = start to stop" $
            FS.scoreTimeWindow
                RaceToGoal
                (StartGates 2)
                (TaskStopTime 1)
                [TaskTime [u| 0 s |]]
                @?= ScoreTimeWindow 1

        , HU.testCase "2 start gates, 1 launches at stop = 0" $
            FS.scoreTimeWindow
                RaceToGoal
                (StartGates 2)
                (TaskStopTime 1)
                [TaskTime [u| 1 s |]]
                @?= ScoreTimeWindow 0
        ]
    , testGroup "Elapsed time"
        [ HU.testCase "1 start gate, noone launches = 0" $
            FS.scoreTimeWindow
                ElapsedTime
                (StartGates 1)
                (TaskStopTime 1)
                []
                @?= ScoreTimeWindow 0

        , HU.testCase "1 start gate, 1 launches at start = start to stop" $
            FS.scoreTimeWindow
                ElapsedTime
                (StartGates 1)
                (TaskStopTime 1)
                [TaskTime [u| 0 s |]]
                @?= ScoreTimeWindow 1

        , HU.testCase "1 start gate, 1 launches at stop = 0" $
            FS.scoreTimeWindow
                ElapsedTime
                (StartGates 1)
                (TaskStopTime 1)
                [TaskTime [u| 1 s |]]
                @?= ScoreTimeWindow 0

        , HU.testCase "2 start gates, noone launches = 0" $
            FS.scoreTimeWindow
                ElapsedTime
                (StartGates 2)
                (TaskStopTime 1)
                []
                @?= ScoreTimeWindow 0

        , HU.testCase "2 start gates, 1 launches at start = start to stop" $
            FS.scoreTimeWindow
                ElapsedTime
                (StartGates 2)
                (TaskStopTime 1)
                [TaskTime [u| 0 s |]]
                @?= ScoreTimeWindow 1

        , HU.testCase "2 start gates, 1 launches at stop = 0" $
            FS.scoreTimeWindow
                ElapsedTime
                (StartGates 2)
                (TaskStopTime 1)
                [TaskTime [u| 1 s |]]
                @?= ScoreTimeWindow 0
        ]
    ]

applyGlideUnits :: TestTree
applyGlideUnits = testGroup "Distance points with altitude bonus"
    [ HU.testCase "Noone flies = no changed tracks" $
        FS.applyGlides
            (GlideRatio 1)
            []
            []
            @?= []

    , HU.testCase "Non-positive glide ratio = no changed tracks" $
        FS.applyGlides
            (GlideRatio $ negate 1)
            [AltitudeAboveGoal 1]
            [StoppedTrack [(TaskTime [u| 1 s |], DistanceToGoal 1)]]
            @?= [StoppedTrack [(TaskTime [u| 1 s |], DistanceToGoal 1)]]

    , HU.testCase "Out at 1:1 below goal, 1:1 glide ratio = no changed tracks" $
        FS.applyGlides
            (GlideRatio 1)
            [AltitudeAboveGoal $ negate 1]
            [StoppedTrack [(TaskTime [u| 1 s |], DistanceToGoal 1)]]
            @?= [StoppedTrack [(TaskTime [u| 1 s |], DistanceToGoal 1)]]

    , HU.testCase "Out at 1:1 from goal, 1:1 glide ratio = at goal" $
        FS.applyGlides
            (GlideRatio 1)
            [AltitudeAboveGoal 1]
            [StoppedTrack [(TaskTime [u| 1 s |], DistanceToGoal 1)]]
            @?= [StoppedTrack [(TaskTime [u| 1 s |], DistanceToGoal 0)]]

    , HU.testCase "Out at 1:1 from goal, 2:1 glide ratio = at goal with no overshoot" $
        FS.applyGlides
            (GlideRatio 2)
            [AltitudeAboveGoal 1]
            [StoppedTrack [(TaskTime [u| 1 s |], DistanceToGoal 1)]]
            @?= [StoppedTrack [(TaskTime [u| 1 s |], DistanceToGoal 0)]]

    , HU.testCase "Out at 1:2 from goal, 1:1 glide ratio = halve distance from goal" $
        FS.applyGlides
            (GlideRatio 1)
            [AltitudeAboveGoal 1]
            [StoppedTrack [(TaskTime [u| 1 s |], DistanceToGoal 2)]]
            @?= [StoppedTrack [(TaskTime [u| 1 s |], DistanceToGoal 1)]]
    ]

correctTime :: forall a. StopTime a -> TaskStopTime -> Bool

correctTime
    (ScoreBackStop (ScoreBackTime sb) (AnnouncedTime at))
    (TaskStopTime st) =
    st + sb' == at
    where
        (MkQuantity sb') :: Quantity Rational [u| s |] =
            convert . toRational' $ sb

correctTime (InterGateStop (StartGateInterval i) (AnnouncedTime at)) (TaskStopTime st) =
    st + i == at

correctTime (SingleGateStop (AnnouncedTime at)) (TaskStopTime st) =
    st + (15 * 60) == at

stopTaskTimePg :: StopTimeTest Pg -> Bool
stopTaskTimePg (StopTimeTest x@(ScoreBackStop _ _)) =
    correctTime x $ FS.stopTaskTime x

stopTaskTimeHg :: StopTimeTest Hg -> Bool
stopTaskTimeHg (StopTimeTest x@(InterGateStop _ _)) =
    correctTime x $ FS.stopTaskTime x

stopTaskTimeHg (StopTimeTest x@(SingleGateStop _)) =
    correctTime x $ FS.stopTaskTime x

correctCan :: forall a. CanScoreStopped a -> Bool -> Bool
correctCan (Womens (PilotsAtEss n) (TaskStopTime t)) canScore
    | n >= 1 = canScore
    | t >= 60 * 60 = canScore
    | otherwise = not canScore
correctCan (GoalOrDuration (PilotsAtEss n) (TaskStopTime t)) canScore
    | n >= 1 = canScore
    | t >= 90 * 60 = canScore
    | otherwise = not canScore
correctCan (FromGetGo (TaskStopTime t)) canScore
    | t >= 60 * 60 = canScore
    | otherwise = not canScore
correctCan (FromLastStart [] _) canScore =
    not canScore
correctCan (FromLastStart xs (TaskStopTime st)) canScore =
    all (\(TaskTime (MkQuantity t)) -> st >= t + 60 * 60) xs == canScore

canScoreStoppedHg :: StopCanScoreTest Hg -> Bool
canScoreStoppedHg (StopCanScoreTest x@(Womens _ _)) =
    correctCan x $ FS.canScoreStopped x
canScoreStoppedHg (StopCanScoreTest x@(GoalOrDuration _ _)) =
    correctCan x $ FS.canScoreStopped x

canScoreStoppedPg :: StopCanScoreTest Pg -> Bool
canScoreStoppedPg (StopCanScoreTest x@(FromGetGo _)) =
    correctCan x $ FS.canScoreStopped x
canScoreStoppedPg (StopCanScoreTest x@(FromLastStart _ _)) =
    correctCan x $ FS.canScoreStopped x

scoreTimeWindow :: StopWindowTest -> Bool
scoreTimeWindow (StopWindowTest (taskType, gates, stop@(TaskStopTime st), xs)) =
    (\(ScoreTimeWindow t) -> 0 <= t && t <= st) $ FS.scoreTimeWindow taskType gates stop xs

correctGlide :: StoppedTrack -> StoppedTrack -> Bool
correctGlide (StoppedTrack xs) (StoppedTrack ys) =
    and $ zipWith (\(_, x) (_, y) -> x >= y) xs ys

applyGlide :: StopGlideTest -> Bool
applyGlide (StopGlideTest ( gr@(GlideRatio glide)
                          , alt@(AltitudeAboveGoal altitude)
                          , track
                          ))
    | glide <= 0 =
        (== track) ys
    | altitude <= 0 =
        (== track) ys
    | otherwise =
        correctGlide track ys

    where ys = FS.applyGlide gr alt track
