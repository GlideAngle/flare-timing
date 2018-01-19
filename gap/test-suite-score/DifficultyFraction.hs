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
    , bestDistance'
    )

import TestNewtypes

min0 :: MinimumDistance (Quantity Double [u| km |])
min0 = MinimumDistance . MkQuantity $ 0

min1 :: MinimumDistance (Quantity Double [u| km |])
min1 = MinimumDistance . MkQuantity $ 1

min2 :: MinimumDistance (Quantity Double [u| km |])
min2 = MinimumDistance . MkQuantity $ 2

min3 :: MinimumDistance (Quantity Double [u| km |])
min3 = MinimumDistance . MkQuantity $ 3

min4 :: MinimumDistance (Quantity Double [u| km |])
min4 = MinimumDistance . MkQuantity $ 4

min5 :: MinimumDistance (Quantity Double [u| km |])
min5 = MinimumDistance . MkQuantity $ 5

min6 :: MinimumDistance (Quantity Double [u| km |])
min6 = MinimumDistance . MkQuantity $ 6

min7 :: MinimumDistance (Quantity Double [u| km |])
min7 = MinimumDistance . MkQuantity $ 7

min8 :: MinimumDistance (Quantity Double [u| km |])
min8 = MinimumDistance . MkQuantity $ 8

min9 :: MinimumDistance (Quantity Double [u| km |])
min9 = MinimumDistance . MkQuantity $ 9

min10 :: MinimumDistance (Quantity Double [u| km |])
min10 = MinimumDistance . MkQuantity $ 10

min11 :: MinimumDistance (Quantity Double [u| km |])
min11 = MinimumDistance . MkQuantity $ 11

min12 :: MinimumDistance (Quantity Double [u| km |])
min12 = MinimumDistance . MkQuantity $ 12

min13 :: MinimumDistance (Quantity Double [u| km |])
min13 = MinimumDistance . MkQuantity $ 13

min14 :: MinimumDistance (Quantity Double [u| km |])
min14 = MinimumDistance . MkQuantity $ 14

min15 :: MinimumDistance (Quantity Double [u| km |])
min15 = MinimumDistance . MkQuantity $ 15

min20 :: MinimumDistance (Quantity Double [u| km |])
min20 = MinimumDistance . MkQuantity $ 20

dists10 :: [PilotDistance (Quantity Double [u| km |])]
dists10 =
    [ PilotDistance . MkQuantity . fromRational $ 10 * x
    | x <- [ 1 .. 10 ]
    ]

best10 :: BestDistance (Quantity Double [u| km |])
best10 = fromMaybe (BestDistance . MkQuantity $ 0) $ bestDistance' dists10

dists100 :: [PilotDistance (Quantity Double [u| km |])]
dists100 =
    [ PilotDistance . MkQuantity . fromRational $ x
    | x <- [ 1 .. 100 ]
    ]

best100 :: BestDistance (Quantity Double [u| km |])
best100 = fromMaybe (BestDistance . MkQuantity $ 0) $ bestDistance' dists100

expected10 :: [DifficultyFraction]
expected10 =
    DifficultyFraction <$>
    take 7 [ n % 17 | n <- [1 .. ]]
    ++ [31 % (4 * 17), 33 % (4 * 17), 1 % 2]

expected100 :: [DifficultyFraction]
expected100 =
    DifficultyFraction <$>
    take 97 [ n % 197 | n <- [1 .. ]]
    ++ [391 % (4 * 197), 393 % (4 * 197), 1 % 2]

expected100_min2 :: [DifficultyFraction]
expected100_min2 =
    DifficultyFraction <$>
    take 96 [ n % 195 | n <- [1 .. ]]
    ++ [387 % (4 * 195), 389 % (4 * 195), 1 % 2]

expected100_min5 :: [DifficultyFraction]
expected100_min5 =
    DifficultyFraction <$>
    take 93 [ n % 189 | n <- [1 .. ]]
    ++ [375 % (4 * 189), 377 % (4 * 189), 1 % 2]

expected100_min10 :: [DifficultyFraction]
expected100_min10 =
    DifficultyFraction <$>
    take 88 [ n % 179 | n <- [1 .. ]]
    ++ [355 % (4 * 179), 357 % (4 * 179), 1 % 2]

expected100_min15 :: [DifficultyFraction]
expected100_min15 =
    DifficultyFraction <$>
    take 83 [ n % 169 | n <- [1 .. ]]
    ++ [335 % (4 * 169), 337 % (4 * 169), 1 % 2]

expected100_min20 :: [DifficultyFraction]
expected100_min20 =
    DifficultyFraction <$>
    take 78 [ n % 159 | n <- [1 .. ]]
    ++ [315 % (4 * 159), 317 % (4 * 159), 1 % 2]

difficultyUnits :: TestTree
difficultyUnits = testGroup "Difficulty fraction unit tests"
    [ testGroup "Lookahead"
        [ HU.testCase
            "10 pilots land out in 100 kms = 300 x 100 hm look ahead chunks or 30 kms"
            $ FS.lookahead best10 dists10 @?= Lookahead 300

        , HU.testCase
            "100 pilots land out in 100 kms = 30 x 100 hm look ahead chunks or 3 kms"
            $ FS.lookahead best100 dists100 @?= Lookahead 30
        ]

    , testGroup "10 pilots evenly land out"
        [ HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 0"
            $ diffFracs (FS.difficulty min0 best10 dists10)
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 1"
            $ diffFracs (FS.difficulty min1 best10 dists10)
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 2"
            $ diffFracs (FS.difficulty min2 best10 dists10)
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 3"
            $ diffFracs (FS.difficulty min3 best10 dists10)
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 4"
            $ diffFracs (FS.difficulty min4 best10 dists10)
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 5"
            $ diffFracs (FS.difficulty min5 best10 dists10)
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 6"
            $ diffFracs (FS.difficulty min6 best10 dists10)
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 7"
            $ diffFracs (FS.difficulty min7 best10 dists10)
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 8"
            $ diffFracs (FS.difficulty min8 best10 dists10)
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 9"
            $ diffFracs (FS.difficulty min9 best10 dists10)
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 10"
            $ diffFracs (FS.difficulty min10 best10 dists10)
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 11"
            $ diffFracs (FS.difficulty min11 best10 dists10)
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 12"
            $ diffFracs (FS.difficulty min12 best10 dists10)
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 13"
            $ diffFracs (FS.difficulty min13 best10 dists10)
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 14"
            $ diffFracs (FS.difficulty min14 best10 dists10)
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 15"
            $ diffFracs (FS.difficulty min15 best10 dists10)
            @?= expected10
        ]

    , testGroup "100 pilots evenly land out"
        [ HU.testCase
            "100 pilots evenly land out over 100 kms, minimum distance = 0"
            $ diffFracs (FS.difficulty min0 best100 dists100)
            @?= expected100

        , HU.testCase
            "100 pilots evenly land out over 100 kms, minimum distance = 1"
            $ diffFracs (FS.difficulty min1 best100 dists100)
            @?= expected100

        , HU.testCase
            "100 pilots evenly land out over 100 kms, minimum distance = 2"
            $ diffFracs (FS.difficulty min2 best100 dists100)
            @?= expected100_min2

        , HU.testCase
            "100 pilots evenly land out over 100 kms, minimum distance = 5"
            $ diffFracs (FS.difficulty min5 best100 dists100)
            @?= expected100_min5

        , HU.testCase
            "100 pilots evenly land out over 100 kms, minimum distance = 10"
            $ diffFracs (FS.difficulty min10 best100 dists100)
            @?= expected100_min10

        , HU.testCase
            "100 pilots evenly land out over 100 kms, minimum distance = 15"
            $ diffFracs (FS.difficulty min15 best100 dists100)
            @?= expected100_min15

        , HU.testCase
            "100 pilots evenly land out over 100 kms, minimum distance = 20"
            $ diffFracs (FS.difficulty min20 best100 dists100)
            @?= expected100_min20
        ]
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
