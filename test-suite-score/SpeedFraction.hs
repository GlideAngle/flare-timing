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

hm :: Integer -> Integer -> Rational
hm h m =
    (h * 60 + m) % 60

speedFractionUnits :: TestTree
speedFractionUnits = testGroup "Speed fraction unit tests"
    [ HU.testCase "1:00 best time, 1:00 pilot time == 1 speed fraction" $
        FS.speedFraction (BestTime (1 % 1)) (PilotTime (1 % 1)) @?= maxS
     
    , HU.testCase "1:00 best time, 2:00 pilot time == 0 speed fraction" $
        FS.speedFraction (BestTime (1 % 1)) (PilotTime (2 % 1)) @?= minS
     
    , HU.testCase "2:00 best time, 3:24 pilot time == 0 speed fraction" $
        FS.speedFraction (BestTime (2 % 1)) (PilotTime $ hm 3 24) @?= minS
     
    , HU.testCase "3:00 best time, 4:42 pilot time == 0 speed fraction" $
        FS.speedFraction (BestTime (3 % 1)) (PilotTime $ hm 4 42) @?= minS
     
    -- WARNING: Failing test.
    --  expected: SpeedFraction (1 % 2)
    --   but got: SpeedFraction (4533826038964677 % 9007199254740992)
    , HU.testCase "1:00 best time, 1:21 pilot time == 0.5 speed fraction" $
        FS.speedFraction (BestTime (1 % 1)) (PilotTime $ hm 1 21) @?= halfS

    -- WARNING: Failing test.
    --  expected: SpeedFraction (1 % 2)
    --   but got: SpeedFraction (61751243628709 % 140737488355328)
    , HU.testCase "2:00 best time, 2:30 pilot time == 0.5 speed fraction" $
        FS.speedFraction (BestTime (2 % 1)) (PilotTime $ hm 2 30) @?= halfS
     
    -- WARNING: Failing test.
    --  expected: SpeedFraction (1 % 2)
    --   but got: SpeedFraction (1786702375306195 % 4503599627370496)
    , HU.testCase "3:00 best time, 3:37 pilot time == 0.5 speed fraction" $
        FS.speedFraction (BestTime (3 % 1)) (PilotTime $ hm 3 37) @?= halfS
     
    -- WARNING: Failing test.
    --  expected: SpeedFraction (4 % 5)
    --   but got: SpeedFraction (7288754376004697 % 9007199254740992)
    , HU.testCase "1:00 best time, 1:05 pilot time == 0.8 speed fraction" $
        FS.speedFraction (BestTime (1 % 1)) (PilotTime $ hm 1 5) @?= point8S
     
    -- WARNING: Failing test.
    --  expected: SpeedFraction (4 % 5)
    --   but got: SpeedFraction (13825749939663921 % 18014398509481984)
    , HU.testCase "2:00 best time, 2:08 pilot time == 0.8 speed fraction" $
        FS.speedFraction (BestTime (2 % 1)) (PilotTime $ hm 2 8) @?= point8S
     
    -- WARNING: Failing test.
    --  expected: SpeedFraction (4 % 5)
    --   but got: SpeedFraction (27559310941624217 % 36028797018963968)
    , HU.testCase "3:00 best time, 3:09 pilot time == 0.8 speed fraction" $
        FS.speedFraction (BestTime (3 % 1)) (PilotTime $ hm 3 9) @?= point8S
    ]

speedFractionInputs :: SfTest -> Bool
speedFractionInputs (SfTest (BestTime best, PilotTime pilot)) = best <= pilot

speedFraction :: SfTest -> Bool
speedFraction (SfTest (best, pilot)) =
    (\(SpeedFraction x) -> isNormal x) $ FS.speedFraction best pilot
