module DifficultyFraction
    ( difficultyFractionUnits
    ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Ratio ((%))

import qualified Flight.Score as FS
import Flight.Score
    ( BestDistance(..)
    , PilotDistance(..)
    , LookaheadChunks(..)
    , DifficultyFraction(..)
    )

dists10 :: [PilotDistance]
dists10 = [ PilotDistance $ 10 * x | x <- [ 1 .. 10 ]]

dists100 :: [PilotDistance]
dists100 = [ PilotDistance x | x <- [ 1 .. 100 ]]

expected0kms10 :: [DifficultyFraction]
expected0kms10 =
    DifficultyFraction <$> replicate 10 (1 % 20)

expected0kms100 :: [DifficultyFraction]
expected0kms100 =
    DifficultyFraction <$> replicate 100 (1 % 200)

expected10kms10 :: [DifficultyFraction]
expected10kms10 =
    DifficultyFraction <$> replicate 10 (1 % 20)

expected1kms100 :: [DifficultyFraction]
expected1kms100 =
    DifficultyFraction <$> replicate 99 (1 % 199) ++ [ 1 % 398 ]

expected30kms10 :: [DifficultyFraction]
expected30kms10 =
    DifficultyFraction <$> replicate 7 (1 % 17) ++ [ 3 % 68, 1 % 34, 1 % 68 ]

expected3kms100 :: [DifficultyFraction]
expected3kms100 = 
    DifficultyFraction <$> replicate 97 (1 % 197) ++ [ 3 % 788, 1 % 394, 1 % 788 ]

difficultyFractionUnits :: TestTree
difficultyFractionUnits = testGroup "Difficulty fraction unit tests"
    [ HU.testCase "10 pilots land out in 100 kms = 300 look ahead chunks or 30 kms" $
        FS.lookaheadChunks (BestDistance 100) dists10
        @?= LookaheadChunks 300

    , HU.testCase "100 pilots land out in 100 kms = 30 look ahead chunks or 3 kms" $
        FS.lookaheadChunks (BestDistance 100) dists100
        @?= LookaheadChunks 30

    , HU.testCase "0 kms look ahead, 10 pilots evenly land out = 1/20 difficulty fraction" $
        FS.difficultyFraction (LookaheadChunks 0) dists10
        @?= expected0kms10

    , HU.testCase "0 kms look ahead, 100 pilots evenly land out = 1/200 difficulty fraction" $
        FS.difficultyFraction (LookaheadChunks 0) dists100
        @?= expected0kms100

    , HU.testCase "10 kms look ahead, 10 pilots evenly land out = 1/20 difficulty fraction" $
        FS.difficultyFraction (LookaheadChunks 10) dists10
        @?= expected10kms10

    , HU.testCase "1 kms look ahead, 100 pilots evenly land out = 99 * 1/199, 1/398" $
        FS.difficultyFraction (LookaheadChunks 10) dists100
        @?= expected1kms100

    , HU.testCase "30 kms look ahead, 10 pilots evenly land out = 7 * 1/17, 3/68, 1/34, 1/68" $
        FS.difficultyFraction (LookaheadChunks 300) dists10
        @?= expected30kms10

    , HU.testCase "3 kms look ahead, 100 pilots evenly land out = 97 * 1/197, 3/788, 1/394, 1/788" $
        FS.difficultyFraction (LookaheadChunks 30) dists100
        @?= expected3kms100
    ]
