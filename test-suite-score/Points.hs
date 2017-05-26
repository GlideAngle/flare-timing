{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Points (tallyUnits, taskPointsHg, taskPointsPg) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Ratio ((%))

import qualified Flight.Score as FS
import Flight.Score
    ( LaunchToSssPoints(..)
    , MinimumDistancePoints(..)
    , SecondsPerPoint(..)
    , JumpedTheGun(..)
    , Hg
    , Pg
    , Penalty(..)
    , TaskPointParts(..)
    , TaskPoints(..)
    , zeroPoints
    )

import TestNewtypes

tallyUnits :: TestTree
tallyUnits = testGroup "Tally task points, with and without penalties"
    [ HU.testCase "No penalties, no points = zero task points" $
        FS.taskPoints (Nothing :: Maybe (Penalty Hg)) zeroPoints @?= TaskPoints 0

    , HU.testCase "No penalties = sum of distance, leading, time & arrival points" $
        FS.taskPoints
            Nothing
            TaskPointParts { distance = 1, leading = 1, time = 1, arrival = 1 }
            @?= TaskPoints 4

    , HU.testCase "Early start PG = distance to start points only" $
        FS.taskPoints
            (Just (Early $ LaunchToSssPoints 1))
            TaskPointParts { distance = 10, leading = 10, time = 10, arrival = 10 }
            @?= TaskPoints 1

    , HU.testCase "Way too early start HG = minimum distance points only" $
        FS.taskPoints
            (Just (JumpedTooEarly $ MinimumDistancePoints 1))
            TaskPointParts { distance = 10, leading = 10, time = 10, arrival = 10 }
            @?= TaskPoints 1

    , HU.testCase "Somewhat early start HG = full points minus jump the gun penalty" $
        FS.taskPoints
            (Just (Jumped (SecondsPerPoint 1) (JumpedTheGun 1)))
            TaskPointParts { distance = 10, leading = 10, time = 10, arrival = 10 }
            @?= TaskPoints 39
    ]

correct :: forall a. Maybe (Penalty a) -> TaskPointParts -> TaskPoints -> Bool

correct Nothing (TaskPointParts{..}) (TaskPoints pts) =
    pts == distance + leading + time + arrival

correct
    (Just (JumpedTooEarly (MinimumDistancePoints md)))
    (TaskPointParts{..})
    (TaskPoints pts) =
    pts == md

correct
    (Just (Jumped (SecondsPerPoint spp) (JumpedTheGun jtg)))
    (TaskPointParts{..})
    (TaskPoints pts) =
    pts == max 0 x
    where
        x = (distance + leading + time + arrival) - jtg / spp

correct
    (Just (JumpedNoGoal (SecondsPerPoint spp) (JumpedTheGun jtg)))
    (TaskPointParts{..})
    (TaskPoints pts) =
    pts == max 0 x
    where
        x = (distance + leading + (8 % 10) * (time + arrival)) - jtg / spp

correct (Just NoGoalHg) TaskPointParts{..} (TaskPoints pts) =
    pts == distance + leading + (8 % 10) * (time + arrival)

correct (Just (Early (LaunchToSssPoints lts))) TaskPointParts{..} (TaskPoints pts) =
    pts == lts

correct (Just NoGoalPg) TaskPointParts{..} (TaskPoints pts) =
    pts == distance + leading

taskPointsHg :: PtTest Hg -> Bool
taskPointsHg (PtTest (penalty, parts)) =
    correct penalty parts $ FS.taskPoints penalty parts

taskPointsPg :: PtTest Pg -> Bool
taskPointsPg (PtTest (penalty, parts)) =
    correct penalty parts $ FS.taskPoints penalty parts
