{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DifficultyFraction
    ( difficultyUnits
    , difficulty
    , lookahead
    ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Ratio ((%))
import Data.Maybe (fromMaybe)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import qualified Flight.Score as FS (lookahead, difficulty)
import Flight.Score
    ( PilotDistance(..)
    , Lookahead(..)
    , MinimumDistance(..)
    , BestDistance(..)
    , DifficultyFraction(..)
    , ChunkDifficultyFraction(..)
    , Difficulty(..)
    , isNormal
    , bestDistance
    )

import TestNewtypes

min0 :: MinimumDistance
min0 = (MinimumDistance . MkQuantity $ 0)

min15 :: MinimumDistance
min15 = (MinimumDistance . MkQuantity $ 15)

dists10 :: [PilotDistance (Quantity Double [u| km |])]
dists10 =
    [ PilotDistance . MkQuantity . fromRational $ 10 * x
    | x <- [ 1 .. 10 ]
    ]

best10 :: BestDistance (Quantity Double [u| km |])
best10 = fromMaybe (BestDistance . MkQuantity $ 0) $ bestDistance dists10

dists100 :: [PilotDistance (Quantity Double [u| km |])]
dists100 =
    [ PilotDistance . MkQuantity . fromRational $ x
    | x <- [ 1 .. 100 ]
    ]

best100 :: BestDistance (Quantity Double [u| km |])
best100 = fromMaybe (BestDistance . MkQuantity $ 0) $ bestDistance dists100

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

difficultyUnits :: TestTree
difficultyUnits = testGroup "Difficulty fraction unit tests"
    [ HU.testCase
        "10 pilots land out in 100 kms = 3000 look ahead chunks or 300 kms"
        $ FS.lookahead best10 dists10 @?= Lookahead 3000

    , HU.testCase
        "100 pilots land out in 100 kms = 300 look ahead chunks or 30 kms"
        $ FS.lookahead best100 dists100 @?= Lookahead 300

    , HU.testCase
        "10 pilots evenly land out over 100 kms, minimum distance = 0"
        $ diffFracs (FS.difficulty min0 best10 dists10)
        @?= expected10

    , HU.testCase
        "100 pilots evenly land out over 100 kms, minimum distance = 0"
        $ diffFracs (FS.difficulty min0 best100 dists100)
        @?= expected100

    , HU.testCase
        "10 pilots evenly land out over 100 kms, minimum distance = 15"
        $ diffFracs (FS.difficulty min15 best10 dists10)
        @?= expected10

    , HU.testCase
        "100 pilots evenly land out over 100 kms, minimum distance = 1"
        $ diffFracs (FS.difficulty min15 best100 dists100)
        @?= expected100
    ]

lookahead :: DfTest -> Bool
lookahead (DfTest (_, dBest@(BestDistance (MkQuantity best)), xs)) =
    (\(Lookahead n) -> n >= 30 && n <= max 30 chunks)
    $ FS.lookahead dBest xs
    where
        chunks =
            if null xs then 0
                       else round $ 30.0 * best / (fromIntegral . length $ xs)

difficulty :: DfTest -> Bool
difficulty (DfTest (dMin, dBest, xs)) =
    all isNormal $ f <$> diffFracs (FS.difficulty dMin dBest xs)
    where
        f (DifficultyFraction df) = df

diffFracs :: Difficulty -> [DifficultyFraction]
diffFracs = fmap frac . fractional
