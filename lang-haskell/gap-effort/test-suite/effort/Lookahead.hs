module Lookahead (lookaheadUnits, lookahead) where

import Text.Printf (printf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Maybe (fromMaybe)
import Data.UnitsOfMeasure (u, zero, unQuantity)
import Data.UnitsOfMeasure.Show (showQuantity)
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

type Km = Quantity Double [u| km |]

mkKm :: Double -> Quantity Double [u| km |]
mkKm = [u| km |]

distKms :: [Double] -> [PilotDistance Km]
distKms = fmap (PilotDistance . mkKm)

bd :: [PilotDistance Km] -> FlownMax Km
bd xs = fromMaybe (FlownMax zero) $ bestDistance' xs

showKm :: Km -> String
showKm = showQuantity

showKms :: [PilotDistance Km] -> String
showKms xs =
    go $ f <$> xs
    where
        f (PilotDistance x) = unQuantity x

        go ys
            | length xs > 2
            , i : j : _ <- ys
            , k : _ <- reverse ys = printf "[%6.3f, %6.3f, ... %7.3f]" i j k
            | otherwise = show ys

laUnit :: [Double] -> Int -> _
laUnit xs expected =
    let ds = distKms xs
        dExpected = [u| km |] $ fromIntegral expected
    in
        HU.testCase
            (printf "%5d pilots @ %s km => look %s" (length ds) (showKms ds) (showKm dExpected))
            (FS.lookahead (bd ds) ds @?= Lookahead expected)

lookaheadUnits :: TestTree
lookaheadUnits = testGroup "Lookahead"
    [ testGroup "0 pilots" [ laUnit [] 30 ]

    , testGroup "1 pilot"
        [ laUnit [-2] 30
        , laUnit [-1] 30
        , laUnit [0] 30
        , laUnit [1] 30
        , laUnit [2] 60
        , laUnit [3] 90
        , laUnit [99] 2970
        , laUnit [100] 3000
        ]

    , testGroup "2 pilots over 1 km"
        [ laUnit [0, 0] 30
        , laUnit [0, 1] 30
        , laUnit [1, 1] 30
        ]

    , testGroup "2 pilots over 2 km"
        [ laUnit [0, 2] 30
        , laUnit [1, 2] 30
        , laUnit [2, 2] 30
        ]

    , testGroup "2 pilots over 3 km"
        [ laUnit [0, 3] 45
        , laUnit [1, 3] 45
        , laUnit [2, 3] 45
        , laUnit [3, 3] 45
        ]

    , testGroup "2 pilots over 100 km"
        [ laUnit [0,100] 1500
        , laUnit [1,100] 1500
        , laUnit [99, 100] 1500
        , laUnit [100, 100] 1500
        ]

    , testGroup "2 pilots over 1000 km"
        [ laUnit [0,1000] 15000
        , laUnit [1,1000] 15000
        , laUnit [999, 1000] 15000
        , laUnit [1000, 1000] 15000
        ]

    , testGroup "100 km (examples from the rules)"
        [ laUnit [10,20 .. 100] 300
        , laUnit [91,92 .. 100] 300
        , laUnit [1,2 .. 100] 30
        , laUnit [90.1,90.2 .. 100] 30
        ]

    , testGroup "100 km (many pilots)"
        [ laUnit [0.1,0.2 .. 100] 30
        , laUnit [90.01,90.02 .. 100] 30
        , laUnit [0.01,0.02 .. 100] 30
        , laUnit [90.001,90.002 .. 100] 30
        ]
    ]

lookahead :: DfTest -> Bool
lookahead (DfTest (dBest@(FlownMax (MkQuantity d)), xs)) =
    (\(Lookahead n) -> n >= 30 && n <= max 30 chunks)
    $ FS.lookahead dBest xs
    where
        chunks =
            if null xs then 0
                       else round $ 30.0 * d / (fromIntegral . length $ xs)
