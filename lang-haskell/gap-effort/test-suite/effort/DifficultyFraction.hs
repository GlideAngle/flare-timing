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

import qualified "flight-gap-effort" Flight.Score as FS (lookahead, gradeDifficulty)
import "flight-gap-allot" Flight.Score
    ( PilotDistance(..)
    , FlownMax(..)
    , Pilot(..)
    , PilotName(..)
    , PilotId(..)
    , isNormal
    , bestDistance'
    )
import "flight-gap-effort" Flight.Score
    ( Lookahead(..)
    , DifficultyFraction(..)
    , ChunkDifficultyFraction(..)
    , Difficulty(..)
    )

import TestNewtypes

dists10 :: Rational -> [PilotDistance (Quantity Double [u| km |])]
dists10 i =
    [ PilotDistance . MkQuantity . fromRational $ 10 * x
    | x <- [i .. 10]
    ]

best10 :: Rational -> FlownMax (Quantity Double [u| km |])
best10 i = fromMaybe (FlownMax . MkQuantity $ 0) $ bestDistance' (dists10 i)

dists100 :: Rational -> [PilotDistance (Quantity Double [u| km |])]
dists100 i =
    [ PilotDistance . MkQuantity . fromRational $ x
    | x <- [i .. 100]
    ]

best100 :: Rational -> FlownMax (Quantity Double [u| km |])
best100 i = fromMaybe (FlownMax . MkQuantity $ 0) $ bestDistance' (dists100 i)

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
            $ FS.lookahead (best10 1) (dists10 1) @?= Lookahead 300

        , HU.testCase
            "100 pilots land out in 100 kms = 30 x 100 hm look ahead chunks or 3 kms"
            $ FS.lookahead (best100 1) (dists100 1) @?= Lookahead 30
        ]

    , testGroup "10 pilots evenly land out"
        [ HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 0"
            $ (let d = 0 in diffFracs (FS.gradeDifficulty (best10 d) nullPilots (dists10 d)))
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 1"
            $ (let d = 1 in diffFracs (FS.gradeDifficulty (best10 d) nullPilots (dists10 d)))
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 2"
            $ (let d = 2 in diffFracs (FS.gradeDifficulty (best10 d) nullPilots (dists10 d)))
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 3"
            $ (let d = 3 in diffFracs (FS.gradeDifficulty (best10 d) nullPilots (dists10 d)))
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 4"
            $ (let d = 4 in diffFracs (FS.gradeDifficulty (best10 d) nullPilots (dists10 d)))
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 5"
            $ (let d = 5 in diffFracs (FS.gradeDifficulty (best10 d) nullPilots (dists10 d)))
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 6"
            $ (let d = 6 in diffFracs (FS.gradeDifficulty (best10 d) nullPilots (dists10 d)))
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 7"
            $ (let d = 7 in diffFracs (FS.gradeDifficulty (best10 d) nullPilots (dists10 d)))
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 8"
            $ (let d = 8 in diffFracs (FS.gradeDifficulty (best10 d) nullPilots (dists10 d)))
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 9"
            $ (let d = 9 in diffFracs (FS.gradeDifficulty (best10 d) nullPilots (dists10 d)))
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 10"
            $ (let d = 10 in diffFracs (FS.gradeDifficulty (best10 d) nullPilots (dists10 d)))
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 11"
            $ (let d = 11 in diffFracs (FS.gradeDifficulty (best10 d) nullPilots (dists10 d)))
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 12"
            $ (let d = 12 in diffFracs (FS.gradeDifficulty (best10 d) nullPilots (dists10 d)))
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 13"
            $ (let d = 13 in diffFracs (FS.gradeDifficulty (best10 d) nullPilots (dists10 d)))
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 14"
            $ (let d = 14 in diffFracs (FS.gradeDifficulty (best10 d) nullPilots (dists10 d)))
            @?= expected10

        , HU.testCase
            "10 pilots evenly land out over 100 kms, minimum distance = 15"
            $ (let d = 15 in diffFracs (FS.gradeDifficulty (best10 d) nullPilots (dists10 d)))
            @?= expected10
        ]

    , testGroup "100 pilots evenly land out"
        [ HU.testCase
            "100 pilots evenly land out over 100 kms, minimum distance = 0"
            $ (let d = 0 in diffFracs (FS.gradeDifficulty (best100 d) nullPilots (dists100 d)))
            @?= expected100

        , HU.testCase
            "100 pilots evenly land out over 100 kms, minimum distance = 1"
            $ (let d = 1 in diffFracs (FS.gradeDifficulty (best100 d) nullPilots (dists100 d)))
            @?= expected100

        , HU.testCase
            "100 pilots evenly land out over 100 kms, minimum distance = 2"
            $ (let d = 2 in diffFracs (FS.gradeDifficulty (best100 d) nullPilots (dists100 d)))
            @?= expected100_min2

        , HU.testCase
            "100 pilots evenly land out over 100 kms, minimum distance = 5"
            $ (let d = 5 in diffFracs (FS.gradeDifficulty (best100 d) nullPilots (dists100 d)))
            @?= expected100_min5

        , HU.testCase
            "100 pilots evenly land out over 100 kms, minimum distance = 10"
            $ (let d = 10 in diffFracs (FS.gradeDifficulty (best100 d) nullPilots (dists100 d)))
            @?= expected100_min10

        , HU.testCase
            "100 pilots evenly land out over 100 kms, minimum distance = 15"
            $ (let d = 15 in diffFracs (FS.gradeDifficulty (best100 d) nullPilots (dists100 d)))
            @?= expected100_min15

        , HU.testCase
            "100 pilots evenly land out over 100 kms, minimum distance = 20"
            $ (let d = 20 in diffFracs (FS.gradeDifficulty (best100 d) nullPilots (dists100 d)))
            @?= expected100_min20
        ]
    ]

nullPilot :: Pilot
nullPilot = Pilot (PilotId "", PilotName "")

nullPilots :: [Pilot]
nullPilots = repeat nullPilot

lookahead :: DfTest -> Bool
lookahead (DfTest (dBest@(FlownMax (MkQuantity best)), xs)) =
    (\(Lookahead n) -> n >= 30 && n <= max 30 chunks)
    $ FS.lookahead dBest xs
    where
        chunks =
            if null xs then 0
                       else round $ 30.0 * best / (fromIntegral . length $ xs)

difficulty :: DfTest -> Bool
difficulty (DfTest (dBest, xs)) =
    all isNormal $ f <$> diffFracs (FS.gradeDifficulty dBest nullPilots xs)
    where
        f (DifficultyFraction df) = df

diffFracs :: (a, Difficulty) -> [DifficultyFraction]
diffFracs = fmap frac . fractional . snd
