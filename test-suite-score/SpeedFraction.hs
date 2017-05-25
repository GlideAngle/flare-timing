module SpeedFraction (speedFractionUnits, speedFractionInputs, speedFraction) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Ratio ((%))

import qualified Flight.Score as FS
import Flight.Score
    ( BestTime(..)
    , PilotTime(..)
    , SpeedFraction(..)
    , isNormal
    )

import TestNewtypes

maxS:: SpeedFraction
maxS = SpeedFraction (1 % 1)

minS :: SpeedFraction
minS = SpeedFraction (0 % 1)

halfS :: SpeedFraction
halfS = SpeedFraction (1 % 2)

point8S :: SpeedFraction
point8S = SpeedFraction (4 % 5)

hms :: Integer -> Integer -> Integer -> Rational
hms h m s =
    ((h * 60 + m) * 60 + s) % 3600

speedFractionUnits :: TestTree
speedFractionUnits = testGroup "Speed fraction unit tests"
    [ timeUnits
    , maxUnits
    , minUnits
    , point5Units
    , point8Units
    ]

timeUnits :: TestTree
timeUnits = testGroup "Time tests"
    [ HU.testCase "1 hr = 1:00:00" $ 1 `compare` (hms 1 0 0) @?= EQ 
    , HU.testCase "2 hr = 2:00:00" $ 2 `compare` (hms 2 0 0) @?= EQ 
    , HU.testCase "3 hr = 3:00:00" $ 3 `compare` (hms 3 0 0) @?= EQ 
    ]

maxUnits :: TestTree
maxUnits = testGroup "Maximum tests"
    [ HU.testCase "1 hr best time, 1:00:00 pilot time = 1 speed fraction" $
        FS.speedFraction (BestTime 1) (PilotTime $ hms 1 0 0) `compare` maxS
        @?= EQ 
     
    , HU.testCase "2 hr best time, 2:00:00 pilot time = 1 speed fraction" $
        FS.speedFraction (BestTime 2) (PilotTime $ hms 2 0 0) `compare` maxS
        @?= EQ 
     
    , HU.testCase "3 hr best time, 3:00:00 pilot time = 1 speed fraction" $
        FS.speedFraction (BestTime 3) (PilotTime $ hms 3 0 0) `compare` maxS
        @?= EQ 

    , HU.testCase "1 hr best time, 1:00:01 pilot time < 1 speed fraction" $
        FS.speedFraction (BestTime 1) (PilotTime $ hms 1 0 1) `compare` maxS
        @?= LT
     
    , HU.testCase "2 hr best time, 2:00:01 pilot time < 1 speed fraction" $
        FS.speedFraction (BestTime 2) (PilotTime $ hms 2 0 1) `compare` maxS
        @?= LT
     
    , HU.testCase "3 hr best time, 3:00:01 pilot time < 1 speed fraction" $
        FS.speedFraction (BestTime 3) (PilotTime $ hms 3 0 1) `compare` maxS
        @?= LT
    ]

minUnits :: TestTree
minUnits = testGroup "Minimum tests"
    [ HU.testCase "1 hr best time, 1:59:59 pilot time > 0 speed fraction" $
        FS.speedFraction (BestTime 1) (PilotTime $ hms 1 59 59) `compare` minS
        @?= GT
     
    , HU.testCase "1 hr best time, 2:00:00 pilot time = 0 speed fraction" $
        FS.speedFraction (BestTime 1) (PilotTime $ hms 2 0 0) `compare` minS
        @?= EQ
     
    , HU.testCase "2 hr best time, 3:24:51 pilot time > 0 speed fraction" $
        FS.speedFraction (BestTime 2) (PilotTime $ hms 3 24 51) `compare` minS
        @?= GT
     
    , HU.testCase "2 hr best time, 3:24:52 pilot time = 0 speed fraction" $
        FS.speedFraction (BestTime 2) (PilotTime $ hms 3 24 52) `compare` minS
        @?= EQ
     
    , HU.testCase "3 hr best time, 4:43:55 pilot time > 0 speed fraction" $
        FS.speedFraction (BestTime 3) (PilotTime $ hms 4 43 55) `compare` minS
        @?= GT

    , HU.testCase "3 hr best time, 4:43:56 pilot time = 0 speed fraction" $
        FS.speedFraction (BestTime 3) (PilotTime $ hms 4 43 56) `compare` minS
        @?= EQ
    ]

point5Units :: TestTree
point5Units = testGroup "50 % tests"
    [ HU.testCase "1 hr best time, 1:21:12 pilot time > 0.5 speed fraction" $
        FS.speedFraction (BestTime 1) (PilotTime $ hms 1 21 12) `compare` halfS
        @?= GT 

    , HU.testCase "1 hr best time, 1:21:13 pilot time < 0.5 speed fraction" $
        FS.speedFraction (BestTime 1) (PilotTime $ hms 1 21 13) `compare` halfS
        @?= LT 

    , HU.testCase "2 hr best time, 2:29:59 pilot time > 0.5 speed fraction" $
        FS.speedFraction (BestTime 2) (PilotTime $ hms 2 29 59) `compare` halfS
        @?= GT
     
    , HU.testCase "2 hr best time, 2:30:00 pilot time = 0.5 speed fraction" $
        FS.speedFraction (BestTime 2) (PilotTime $ hms 2 30 0) `compare` halfS
        @?= EQ 
     
    , HU.testCase "2 hr best time, 2:30:01 pilot time < 0.5 speed fraction" $
        FS.speedFraction (BestTime 2) (PilotTime $ hms 2 30 1) `compare` halfS
        @?= LT
     
    , HU.testCase "3 hr best time, 3:36:44 pilot time > 0.5 speed fraction" $
        FS.speedFraction (BestTime 3) (PilotTime $ hms 3 36 44) `compare` halfS
        @?= GT
     
    , HU.testCase "3 hr best time, 3:36:45 pilot time < 0.5 speed fraction" $
        FS.speedFraction (BestTime 3) (PilotTime $ hms 3 36 45) `compare` halfS
        @?= LT
    ]
     
point8Units :: TestTree
point8Units = testGroup "80 % tests"
    [ HU.testCase "1 hr best time, 1:05:21 pilot time > 0.8 speed fraction" $
        FS.speedFraction (BestTime 1) (PilotTime $ hms 1 5 21) `compare` point8S
        @?= GT 
     
    , HU.testCase "1 hr best time, 1:05:22 pilot time < 0.8 speed fraction" $
        FS.speedFraction (BestTime 1) (PilotTime $ hms 1 5 22) `compare` point8S
        @?= LT
     
    , HU.testCase "2 hr best time, 2:07:35 pilot time > 0.8 speed fraction" $
        FS.speedFraction (BestTime 2) (PilotTime $ hms 2 7 35) `compare` point8S
        @?= GT
     
    , HU.testCase "2 hr best time, 2:07:36 pilot time < 0.8 speed fraction" $
        FS.speedFraction (BestTime 2) (PilotTime $ hms 2 7 36) `compare` point8S
        @?= LT
     
    , HU.testCase "3 hr best time, 3:09:17 pilot time > 0.8 speed fraction" $
        FS.speedFraction (BestTime 3) (PilotTime $ hms 3 9 17) `compare` point8S
        @?= GT

    , HU.testCase "3 hr best time, 3:09:18 pilot time < 0.8 speed fraction" $
        FS.speedFraction (BestTime 3) (PilotTime $ hms 3 9 18) `compare` point8S
        @?= LT
    ]

speedFractionInputs :: SfTest -> Bool
speedFractionInputs (SfTest (BestTime best, PilotTime pilot)) = best <= pilot

speedFraction :: SfTest -> Bool
speedFraction (SfTest (best, pilot)) =
    (\(SpeedFraction x) -> isNormal x) $ FS.speedFraction best pilot
