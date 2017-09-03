module DifficultyFraction
    ( difficultyFractionUnits
    , lookaheadChunks
    , difficultyFraction
    ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Ratio ((%))

import qualified Flight.Score as FS
import Flight.Score
    ( PilotDistance(..)
    , LookaheadChunks(..)
    , ChunkedDistance(..)
    , DifficultyFraction(..)
    , isNormal
    )

import TestNewtypes

dists10 :: [PilotDistance]
dists10 = [ PilotDistance $ 10 * x | x <- [ 1 .. 10 ]]

dists100 :: [PilotDistance]
dists100 = [ PilotDistance x | x <- [ 1 .. 100 ]]

expected10 :: [DifficultyFraction]
expected10 =
    DifficultyFraction <$>
    [ 1 % 11
    , 9 % 110
    , 4 % 55
    , 7 % 110
    , 3 % 55
    , 1 % 22
    , 2 % 55
    , 3 % 110
    , 1 % 55
    , 1 % 110
    ]

expected100 :: [DifficultyFraction]
expected100 =
    DifficultyFraction <$>
    replicate 70 (1 % 170)
    ++
    [ 3 % 527
    , 29 % 5270
    , 14 % 2635
    , 27 % 5270
    , 13 % 2635
    , 5 % 1054
    , 12 % 2635
    , 23 % 5270
    , 11 % 2635
    , 21 % 5270
    , 2 % 527
    , 19 % 5270
    , 9 % 2635
    , 1 % 310
    , 8 % 2635
    , 3 % 1054
    , 7 % 2635
    , 13 % 5270
    , 6 % 2635
    , 11 % 5270
    , 1 % 527
    , 9 % 5270
    , 4 % 2635
    , 7 % 5270
    , 3 % 2635
    , 1 % 1054
    , 2 % 2635
    , 3 % 5270
    , 1 % 2635
    , 1 % 5270
    ]

difficultyFractionUnits :: TestTree
difficultyFractionUnits = testGroup "Difficulty fraction unit tests"
    [ HU.testCase "10 pilots land out in 100 kms = 3000 look ahead chunks or 300 kms" $
        FS.lookaheadChunks dists10 @?= LookaheadChunks 3000

    , HU.testCase "100 pilots land out in 100 kms = 300 look ahead chunks or 30 kms" $
        FS.lookaheadChunks dists100 @?= LookaheadChunks 300

    , HU.testCase "10 pilots evenly land out over 100 kms" $
        FS.difficultyFraction dists10
        @?= expected10

    , HU.testCase "100 pilots evenly land out over 100 kms" $
        FS.difficultyFraction dists100
        @?= expected100
    ]

lookaheadChunks :: DfTest -> Bool
lookaheadChunks (DfTest xs) =
    (\(LookaheadChunks n) -> n >= 30 && n <= max 30 (30 * bestInChunks)) $ FS.lookaheadChunks xs
    where
        (ChunkedDistance bestInChunks) =
            if null xs then ChunkedDistance 30 else FS.toChunk $ maximum xs

difficultyFraction :: DfTest -> Bool
difficultyFraction (DfTest xs) =
    all (\(DifficultyFraction x) -> isNormal x) $ FS.difficultyFraction xs
