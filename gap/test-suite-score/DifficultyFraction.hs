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

min15 :: MinimumDistance (Quantity Double [u| km |])
min15 = MinimumDistance . MkQuantity $ 15

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
        "10 pilots land out in 100 kms = 300 x 100 hm look ahead chunks or 30 kms"
        $ FS.lookahead best10 dists10 @?= Lookahead 300

    , HU.testCase
        "100 pilots land out in 100 kms = 30 x 100 hm look ahead chunks or 3 kms"
        $ FS.lookahead best100 dists100 @?= Lookahead 30

    -- WARNING: Distance fraction test fail.
    -- expected: [DifficultyFraction (1 % 11),DifficultyFraction (9 % 110),DifficultyFraction (4 % 55),DifficultyFraction (7 % 110),DifficultyFraction (3 % 55),DifficultyFraction (1 % 22),DifficultyFraction (2 % 55),DifficultyFraction (3 % 110),DifficultyFraction (1 % 55),DifficultyFraction (1 % 110)]
    -- but got: [DifficultyFraction (1 % 17),DifficultyFraction (2 % 17),DifficultyFraction (3 % 17),DifficultyFraction (4 % 17),DifficultyFraction (5 % 17),DifficultyFraction (6 % 17),DifficultyFraction (7 % 17),DifficultyFraction (31 % 68),DifficultyFraction (33 % 68),DifficultyFraction (1 % 2)]
    , HU.testCase
        "10 pilots evenly land out over 100 kms, minimum distance = 0"
        $ diffFracs (FS.difficulty min0 best10 dists10)
        @?= expected10

    -- WARNING: Distance fraction test fail.
    -- expected: [DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (3 % 527),DifficultyFraction (29 % 5270),DifficultyFraction (14 % 2635),DifficultyFraction (27 % 5270),DifficultyFraction (13 % 2635),DifficultyFraction (5 % 1054),DifficultyFraction (12 % 2635),DifficultyFraction (23 % 5270),DifficultyFraction (11 % 2635),DifficultyFraction (21 % 5270),DifficultyFraction (2 % 527),DifficultyFraction (19 % 5270),DifficultyFraction (9 % 2635),DifficultyFraction (1 % 310),DifficultyFraction (8 % 2635),DifficultyFraction (3 % 1054),DifficultyFraction (7 % 2635),DifficultyFraction (13 % 5270),DifficultyFraction (6 % 2635),DifficultyFraction (11 % 5270),DifficultyFraction (1 % 527),DifficultyFraction (9 % 5270),DifficultyFraction (4 % 2635),DifficultyFraction (7 % 5270),DifficultyFraction (3 % 2635),DifficultyFraction (1 % 1054),DifficultyFraction (2 % 2635),DifficultyFraction (3 % 5270),DifficultyFraction (1 % 2635),DifficultyFraction (1 % 5270)]
    -- but got: [DifficultyFraction (1 % 197),DifficultyFraction (2 % 197),DifficultyFraction (3 % 197),DifficultyFraction (4 % 197),DifficultyFraction (5 % 197),DifficultyFraction (6 % 197),DifficultyFraction (7 % 197),DifficultyFraction (8 % 197),DifficultyFraction (9 % 197),DifficultyFraction (10 % 197),DifficultyFraction (11 % 197),DifficultyFraction (12 % 197),DifficultyFraction (13 % 197),DifficultyFraction (14 % 197),DifficultyFraction (15 % 197),DifficultyFraction (16 % 197),DifficultyFraction (17 % 197),DifficultyFraction (18 % 197),DifficultyFraction (19 % 197),DifficultyFraction (20 % 197),DifficultyFraction (21 % 197),DifficultyFraction (22 % 197),DifficultyFraction (23 % 197),DifficultyFraction (24 % 197),DifficultyFraction (25 % 197),DifficultyFraction (26 % 197),DifficultyFraction (27 % 197),DifficultyFraction (28 % 197),DifficultyFraction (29 % 197),DifficultyFraction (30 % 197),DifficultyFraction (31 % 197),DifficultyFraction (32 % 197),DifficultyFraction (33 % 197),DifficultyFraction (34 % 197),DifficultyFraction (35 % 197),DifficultyFraction (36 % 197),DifficultyFraction (37 % 197),DifficultyFraction (38 % 197),DifficultyFraction (39 % 197),DifficultyFraction (40 % 197),DifficultyFraction (41 % 197),DifficultyFraction (42 % 197),DifficultyFraction (43 % 197),DifficultyFraction (44 % 197),DifficultyFraction (45 % 197),DifficultyFraction (46 % 197),DifficultyFraction (47 % 197),DifficultyFraction (48 % 197),DifficultyFraction (49 % 197),DifficultyFraction (50 % 197),DifficultyFraction (51 % 197),DifficultyFraction (52 % 197),DifficultyFraction (53 % 197),DifficultyFraction (54 % 197),DifficultyFraction (55 % 197),DifficultyFraction (56 % 197),DifficultyFraction (57 % 197),DifficultyFraction (58 % 197),DifficultyFraction (59 % 197),DifficultyFraction (60 % 197),DifficultyFraction (61 % 197),DifficultyFraction (62 % 197),DifficultyFraction (63 % 197),DifficultyFraction (64 % 197),DifficultyFraction (65 % 197),DifficultyFraction (66 % 197),DifficultyFraction (67 % 197),DifficultyFraction (68 % 197),DifficultyFraction (69 % 197),DifficultyFraction (70 % 197),DifficultyFraction (71 % 197),DifficultyFraction (72 % 197),DifficultyFraction (73 % 197),DifficultyFraction (74 % 197),DifficultyFraction (75 % 197),DifficultyFraction (76 % 197),DifficultyFraction (77 % 197),DifficultyFraction (78 % 197),DifficultyFraction (79 % 197),DifficultyFraction (80 % 197),DifficultyFraction (81 % 197),DifficultyFraction (82 % 197),DifficultyFraction (83 % 197),DifficultyFraction (84 % 197),DifficultyFraction (85 % 197),DifficultyFraction (86 % 197),DifficultyFraction (87 % 197),DifficultyFraction (88 % 197),DifficultyFraction (89 % 197),DifficultyFraction (90 % 197),DifficultyFraction (91 % 197),DifficultyFraction (92 % 197),DifficultyFraction (93 % 197),DifficultyFraction (94 % 197),DifficultyFraction (95 % 197),DifficultyFraction (96 % 197),DifficultyFraction (97 % 197),DifficultyFraction (391 % 788),DifficultyFraction (393 % 788),DifficultyFraction (1 % 2)]
    , HU.testCase
        "100 pilots evenly land out over 100 kms, minimum distance = 0"
        $ diffFracs (FS.difficulty min0 best100 dists100)
        @?= expected100

    -- WARNING: Distance fraction test fail.
    -- expected: [DifficultyFraction (1 % 11),DifficultyFraction (9 % 110),DifficultyFraction (4 % 55),DifficultyFraction (7 % 110),DifficultyFraction (3 % 55),DifficultyFraction (1 % 22),DifficultyFraction (2 % 55),DifficultyFraction (3 % 110),DifficultyFraction (1 % 55),DifficultyFraction (1 % 110)]
    -- but got: [DifficultyFraction (1 % 17),DifficultyFraction (2 % 17),DifficultyFraction (3 % 17),DifficultyFraction (4 % 17),DifficultyFraction (5 % 17),DifficultyFraction (6 % 17),DifficultyFraction (7 % 17),DifficultyFraction (31 % 68),DifficultyFraction (33 % 68),DifficultyFraction (1 % 2)]
    , HU.testCase
        "10 pilots evenly land out over 100 kms, minimum distance = 15"
        $ diffFracs (FS.difficulty min15 best10 dists10)
        @?= expected10

    -- WARNING: Distance fraction test fail.
    -- expected: [DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (1 % 170),DifficultyFraction (3 % 527),DifficultyFraction (29 % 5270),DifficultyFraction (14 % 2635),DifficultyFraction (27 % 5270),DifficultyFraction (13 % 2635),DifficultyFraction (5 % 1054),DifficultyFraction (12 % 2635),DifficultyFraction (23 % 5270),DifficultyFraction (11 % 2635),DifficultyFraction (21 % 5270),DifficultyFraction (2 % 527),DifficultyFraction (19 % 5270),DifficultyFraction (9 % 2635),DifficultyFraction (1 % 310),DifficultyFraction (8 % 2635),DifficultyFraction (3 % 1054),DifficultyFraction (7 % 2635),DifficultyFraction (13 % 5270),DifficultyFraction (6 % 2635),DifficultyFraction (11 % 5270),DifficultyFraction (1 % 527),DifficultyFraction (9 % 5270),DifficultyFraction (4 % 2635),DifficultyFraction (7 % 5270),DifficultyFraction (3 % 2635),DifficultyFraction (1 % 1054),DifficultyFraction (2 % 2635),DifficultyFraction (3 % 5270),DifficultyFraction (1 % 2635),DifficultyFraction (1 % 5270)]
    -- but got: [DifficultyFraction (1 % 169),DifficultyFraction (2 % 169),DifficultyFraction (3 % 169),DifficultyFraction (4 % 169),DifficultyFraction (5 % 169),DifficultyFraction (6 % 169),DifficultyFraction (7 % 169),DifficultyFraction (8 % 169),DifficultyFraction (9 % 169),DifficultyFraction (10 % 169),DifficultyFraction (11 % 169),DifficultyFraction (12 % 169),DifficultyFraction (1 % 13),DifficultyFraction (14 % 169),DifficultyFraction (15 % 169),DifficultyFraction (16 % 169),DifficultyFraction (17 % 169),DifficultyFraction (18 % 169),DifficultyFraction (19 % 169),DifficultyFraction (20 % 169),DifficultyFraction (21 % 169),DifficultyFraction (22 % 169),DifficultyFraction (23 % 169),DifficultyFraction (24 % 169),DifficultyFraction (25 % 169),DifficultyFraction (2 % 13),DifficultyFraction (27 % 169),DifficultyFraction (28 % 169),DifficultyFraction (29 % 169),DifficultyFraction (30 % 169),DifficultyFraction (31 % 169),DifficultyFraction (32 % 169),DifficultyFraction (33 % 169),DifficultyFraction (34 % 169),DifficultyFraction (35 % 169),DifficultyFraction (36 % 169),DifficultyFraction (37 % 169),DifficultyFraction (38 % 169),DifficultyFraction (3 % 13),DifficultyFraction (40 % 169),DifficultyFraction (41 % 169),DifficultyFraction (42 % 169),DifficultyFraction (43 % 169),DifficultyFraction (44 % 169),DifficultyFraction (45 % 169),DifficultyFraction (46 % 169),DifficultyFraction (47 % 169),DifficultyFraction (48 % 169),DifficultyFraction (49 % 169),DifficultyFraction (50 % 169),DifficultyFraction (51 % 169),DifficultyFraction (4 % 13),DifficultyFraction (53 % 169),DifficultyFraction (54 % 169),DifficultyFraction (55 % 169),DifficultyFraction (56 % 169),DifficultyFraction (57 % 169),DifficultyFraction (58 % 169),DifficultyFraction (59 % 169),DifficultyFraction (60 % 169),DifficultyFraction (61 % 169),DifficultyFraction (62 % 169),DifficultyFraction (63 % 169),DifficultyFraction (64 % 169),DifficultyFraction (5 % 13),DifficultyFraction (66 % 169),DifficultyFraction (67 % 169),DifficultyFraction (68 % 169),DifficultyFraction (69 % 169),DifficultyFraction (70 % 169),DifficultyFraction (71 % 169),DifficultyFraction (72 % 169),DifficultyFraction (73 % 169),DifficultyFraction (74 % 169),DifficultyFraction (75 % 169),DifficultyFraction (76 % 169),DifficultyFraction (77 % 169),DifficultyFraction (6 % 13),DifficultyFraction (79 % 169),DifficultyFraction (80 % 169),DifficultyFraction (81 % 169),DifficultyFraction (82 % 169),DifficultyFraction (83 % 169),DifficultyFraction (335 % 676),DifficultyFraction (337 % 676),DifficultyFraction (1 % 2)]
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
