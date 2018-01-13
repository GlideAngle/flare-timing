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
    [ 1 % 17
    , 2 % 17
    , 3 % 17
    , 4 % 17
    , 5 % 17
    , 6 % 17
    , 7 % 17
    , 31 % 68
    , 33 % 68
    , 1 % 2
    ]

expected100 :: [DifficultyFraction]
expected100 =
    DifficultyFraction <$>
    [ 1 % 197
    , 2 % 197
    , 3 % 197
    , 4 % 197
    , 5 % 197
    , 6 % 197
    , 7 % 197
    , 8 % 197
    , 9 % 197
    , 10 % 197
    , 11 % 197
    , 12 % 197
    , 13 % 197
    , 14 % 197
    , 15 % 197
    , 16 % 197
    , 17 % 197
    , 18 % 197
    , 19 % 197
    , 20 % 197
    , 21 % 197
    , 22 % 197
    , 23 % 197
    , 24 % 197
    , 25 % 197
    , 26 % 197
    , 27 % 197
    , 28 % 197
    , 29 % 197
    , 30 % 197
    , 31 % 197
    , 32 % 197
    , 33 % 197
    , 34 % 197
    , 35 % 197
    , 36 % 197
    , 37 % 197
    , 38 % 197
    , 39 % 197
    , 40 % 197
    , 41 % 197
    , 42 % 197
    , 43 % 197
    , 44 % 197
    , 45 % 197
    , 46 % 197
    , 47 % 197
    , 48 % 197
    , 49 % 197
    , 50 % 197
    , 51 % 197
    , 52 % 197
    , 53 % 197
    , 54 % 197
    , 55 % 197
    , 56 % 197
    , 57 % 197
    , 58 % 197
    , 59 % 197
    , 60 % 197
    , 61 % 197
    , 62 % 197
    , 63 % 197
    , 64 % 197
    , 65 % 197
    , 66 % 197
    , 67 % 197
    , 68 % 197
    , 69 % 197
    , 70 % 197
    , 71 % 197
    , 72 % 197
    , 73 % 197
    , 74 % 197
    , 75 % 197
    , 76 % 197
    , 77 % 197
    , 78 % 197
    , 79 % 197
    , 80 % 197
    , 81 % 197
    , 82 % 197
    , 83 % 197
    , 84 % 197
    , 85 % 197
    , 86 % 197
    , 87 % 197
    , 88 % 197
    , 89 % 197
    , 90 % 197
    , 91 % 197
    , 92 % 197
    , 93 % 197
    , 94 % 197
    , 95 % 197
    , 96 % 197
    , 97 % 197
    , 391 % 788
    , 393 % 788
    , 1 % 2
    ]

expected100_min2 :: [DifficultyFraction]
expected100_min2 =
    DifficultyFraction <$>
    [ 1 % 195
    , 2 % 195
    , 1 % 65
    , 4 % 195
    , 1 % 39
    , 2 % 65
    , 7 % 195
    , 8 % 195
    , 3 % 65
    , 2 % 39
    , 11 % 195
    , 4 % 65
    , 1 % 15
    , 14 % 195
    , 1 % 13
    , 16 % 195
    , 17 % 195
    , 6 % 65
    , 19 % 195
    , 4 % 39
    , 7 % 65
    , 22 % 195
    , 23 % 195
    , 8 % 65
    , 5 % 39
    , 2 % 15
    , 9 % 65
    , 28 % 195
    , 29 % 195
    , 2 % 13
    , 31 % 195
    , 32 % 195
    , 11 % 65
    , 34 % 195
    , 7 % 39
    , 12 % 65
    , 37 % 195
    , 38 % 195
    , 1 % 5
    , 8 % 39
    , 41 % 195
    , 14 % 65
    , 43 % 195
    , 44 % 195
    , 3 % 13
    , 46 % 195
    , 47 % 195
    , 16 % 65
    , 49 % 195
    , 10 % 39
    , 17 % 65
    , 4 % 15
    , 53 % 195
    , 18 % 65
    , 11 % 39
    , 56 % 195
    , 19 % 65
    , 58 % 195
    , 59 % 195
    , 4 % 13
    , 61 % 195
    , 62 % 195
    , 21 % 65
    , 64 % 195
    , 1 % 3
    , 22 % 65
    , 67 % 195
    , 68 % 195
    , 23 % 65
    , 14 % 39
    , 71 % 195
    , 24 % 65
    , 73 % 195
    , 74 % 195
    , 5 % 13
    , 76 % 195
    , 77 % 195
    , 2 % 5
    , 79 % 195
    , 16 % 39
    , 27 % 65
    , 82 % 195
    , 83 % 195
    , 28 % 65
    , 17 % 39
    , 86 % 195
    , 29 % 65
    , 88 % 195
    , 89 % 195
    , 6 % 13
    , 7 % 15
    , 92 % 195
    , 31 % 65
    , 94 % 195
    , 19 % 39
    , 32 % 65
    , 129 % 260
    , 389 % 780
    , 1 % 2
    ]

expected100_min5 :: [DifficultyFraction]
expected100_min5 =
    DifficultyFraction <$>
    [ 1 % 189
    , 2 % 189
    , 1 % 63
    , 4 % 189
    , 5 % 189
    , 2 % 63
    , 1 % 27
    , 8 % 189
    , 1 % 21
    , 10 % 189
    , 11 % 189
    , 4 % 63
    , 13 % 189
    , 2 % 27
    , 5 % 63
    , 16 % 189
    , 17 % 189
    , 2 % 21
    , 19 % 189
    , 20 % 189
    , 1 % 9
    , 22 % 189
    , 23 % 189
    , 8 % 63
    , 25 % 189
    , 26 % 189
    , 1 % 7
    , 4 % 27
    , 29 % 189
    , 10 % 63
    , 31 % 189
    , 32 % 189
    , 11 % 63
    , 34 % 189
    , 5 % 27
    , 4 % 21
    , 37 % 189
    , 38 % 189
    , 13 % 63
    , 40 % 189
    , 41 % 189
    , 2 % 9
    , 43 % 189
    , 44 % 189
    , 5 % 21
    , 46 % 189
    , 47 % 189
    , 16 % 63
    , 7 % 27
    , 50 % 189
    , 17 % 63
    , 52 % 189
    , 53 % 189
    , 2 % 7
    , 55 % 189
    , 8 % 27
    , 19 % 63
    , 58 % 189
    , 59 % 189
    , 20 % 63
    , 61 % 189
    , 62 % 189
    , 1 % 3
    , 64 % 189
    , 65 % 189
    , 22 % 63
    , 67 % 189
    , 68 % 189
    , 23 % 63
    , 10 % 27
    , 71 % 189
    , 8 % 21
    , 73 % 189
    , 74 % 189
    , 25 % 63
    , 76 % 189
    , 11 % 27
    , 26 % 63
    , 79 % 189
    , 80 % 189
    , 3 % 7
    , 82 % 189
    , 83 % 189
    , 4 % 9
    , 85 % 189
    , 86 % 189
    , 29 % 63
    , 88 % 189
    , 89 % 189
    , 10 % 21
    , 13 % 27
    , 92 % 189
    , 31 % 63
    , 125 % 252
    , 377 % 756
    , 1 % 2
    ]

expected100_min10 :: [DifficultyFraction]
expected100_min10 =
    DifficultyFraction <$>
    [ 1 % 179
    , 2 % 179
    , 3 % 179
    , 4 % 179
    , 5 % 179
    , 6 % 179
    , 7 % 179
    , 8 % 179
    , 9 % 179
    , 10 % 179
    , 11 % 179
    , 12 % 179
    , 13 % 179
    , 14 % 179
    , 15 % 179
    , 16 % 179
    , 17 % 179
    , 18 % 179
    , 19 % 179
    , 20 % 179
    , 21 % 179
    , 22 % 179
    , 23 % 179
    , 24 % 179
    , 25 % 179
    , 26 % 179
    , 27 % 179
    , 28 % 179
    , 29 % 179
    , 30 % 179
    , 31 % 179
    , 32 % 179
    , 33 % 179
    , 34 % 179
    , 35 % 179
    , 36 % 179
    , 37 % 179
    , 38 % 179
    , 39 % 179
    , 40 % 179
    , 41 % 179
    , 42 % 179
    , 43 % 179
    , 44 % 179
    , 45 % 179
    , 46 % 179
    , 47 % 179
    , 48 % 179
    , 49 % 179
    , 50 % 179
    , 51 % 179
    , 52 % 179
    , 53 % 179
    , 54 % 179
    , 55 % 179
    , 56 % 179
    , 57 % 179
    , 58 % 179
    , 59 % 179
    , 60 % 179
    , 61 % 179
    , 62 % 179
    , 63 % 179
    , 64 % 179
    , 65 % 179
    , 66 % 179
    , 67 % 179
    , 68 % 179
    , 69 % 179
    , 70 % 179
    , 71 % 179
    , 72 % 179
    , 73 % 179
    , 74 % 179
    , 75 % 179
    , 76 % 179
    , 77 % 179
    , 78 % 179
    , 79 % 179
    , 80 % 179
    , 81 % 179
    , 82 % 179
    , 83 % 179
    , 84 % 179
    , 85 % 179
    , 86 % 179
    , 87 % 179
    , 88 % 179
    , 355 % 716
    , 357 % 716
    , 1 % 2
    ]

expected100_min15 :: [DifficultyFraction]
expected100_min15 =
    DifficultyFraction <$>
    [ 1 % 169
    , 2 % 169
    , 3 % 169
    , 4 % 169
    , 5 % 169
    , 6 % 169
    , 7 % 169
    , 8 % 169
    , 9 % 169
    , 10 % 169
    , 11 % 169
    , 12 % 169
    , 1 % 13
    , 14 % 169
    , 15 % 169
    , 16 % 169
    , 17 % 169
    , 18 % 169
    , 19 % 169
    , 20 % 169
    , 21 % 169
    , 22 % 169
    , 23 % 169
    , 24 % 169
    , 25 % 169
    , 2 % 13
    , 27 % 169
    , 28 % 169
    , 29 % 169
    , 30 % 169
    , 31 % 169
    , 32 % 169
    , 33 % 169
    , 34 % 169
    , 35 % 169
    , 36 % 169
    , 37 % 169
    , 38 % 169
    , 3 % 13
    , 40 % 169
    , 41 % 169
    , 42 % 169
    , 43 % 169
    , 44 % 169
    , 45 % 169
    , 46 % 169
    , 47 % 169
    , 48 % 169
    , 49 % 169
    , 50 % 169
    , 51 % 169
    , 4 % 13
    , 53 % 169
    , 54 % 169
    , 55 % 169
    , 56 % 169
    , 57 % 169
    , 58 % 169
    , 59 % 169
    , 60 % 169
    , 61 % 169
    , 62 % 169
    , 63 % 169
    , 64 % 169
    , 5 % 13
    , 66 % 169
    , 67 % 169
    , 68 % 169
    , 69 % 169
    , 70 % 169
    , 71 % 169
    , 72 % 169
    , 73 % 169
    , 74 % 169
    , 75 % 169
    , 76 % 169
    , 77 % 169
    , 6 % 13
    , 79 % 169
    , 80 % 169
    , 81 % 169
    , 82 % 169
    , 83 % 169
    , 335 % 676
    , 337 % 676
    , 1 % 2
    ]

expected100_min20 :: [DifficultyFraction]
expected100_min20 =
    DifficultyFraction <$>
    [ 1 % 159
    , 2 % 159
    , 1 % 53
    , 4 % 159
    , 5 % 159
    , 2 % 53
    , 7 % 159
    , 8 % 159
    , 3 % 53
    , 10 % 159
    , 11 % 159
    , 4 % 53
    , 13 % 159
    , 14 % 159
    , 5 % 53
    , 16 % 159
    , 17 % 159
    , 6 % 53
    , 19 % 159
    , 20 % 159
    , 7 % 53
    , 22 % 159
    , 23 % 159
    , 8 % 53
    , 25 % 159
    , 26 % 159
    , 9 % 53
    , 28 % 159
    , 29 % 159
    , 10 % 53
    , 31 % 159
    , 32 % 159
    , 11 % 53
    , 34 % 159
    , 35 % 159
    , 12 % 53
    , 37 % 159
    , 38 % 159
    , 13 % 53
    , 40 % 159
    , 41 % 159
    , 14 % 53
    , 43 % 159
    , 44 % 159
    , 15 % 53
    , 46 % 159
    , 47 % 159
    , 16 % 53
    , 49 % 159
    , 50 % 159
    , 17 % 53
    , 52 % 159
    , 1 % 3
    , 18 % 53
    , 55 % 159
    , 56 % 159
    , 19 % 53
    , 58 % 159
    , 59 % 159
    , 20 % 53
    , 61 % 159
    , 62 % 159
    , 21 % 53
    , 64 % 159
    , 65 % 159
    , 22 % 53
    , 67 % 159
    , 68 % 159
    , 23 % 53
    , 70 % 159
    , 71 % 159
    , 24 % 53
    , 73 % 159
    , 74 % 159
    , 25 % 53
    , 76 % 159
    , 77 % 159
    , 26 % 53
    , 105 % 212
    , 317 % 636
    , 1 % 2
    ]

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
