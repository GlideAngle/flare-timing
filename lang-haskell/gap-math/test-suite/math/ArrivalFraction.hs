module ArrivalFraction (arrivalFractionUnits, arrivalFraction) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Ratio ((%))

import qualified "flight-gap-base" Flight.Score as FS
import "flight-gap-base" Flight.Score
    ( PilotsAtEss(..)
    , ArrivalPlacing(..)
    , ArrivalFraction(..)
    , isNormal
    )

import TestNewtypes

maxArrivalFraction :: ArrivalFraction
maxArrivalFraction = ArrivalFraction (1 % 1)

minArrivalFraction :: ArrivalFraction
minArrivalFraction = ArrivalFraction (1 % 5)

arrivalFractionUnits :: TestTree
arrivalFractionUnits = testGroup "Arrival fraction unit tests"
    [ HU.testCase "1 pilot at ESS, first at ESS = 1 arrival fraction" $
        FS.arrivalRankFraction (PilotsAtEss 1) (ArrivalPlacing 1) @?= maxArrivalFraction

    , HU.testCase "2 pilots at ESS, first at ESS = 1 arrival fraction" $
        FS.arrivalRankFraction (PilotsAtEss 2) (ArrivalPlacing 1) @?= maxArrivalFraction

    , HU.testCase "2 pilots at ESS, last at ESS > 0.2 arrival fraction" $
        FS.arrivalRankFraction (PilotsAtEss 2) (ArrivalPlacing 2) `compare` minArrivalFraction
        @?= GT

    , HU.testCase "1000 pilots at ESS, last at ESS > 0.2 arrival fraction" $
        FS.arrivalRankFraction (PilotsAtEss 1000) (ArrivalPlacing 1000) `compare` minArrivalFraction
        @?= GT
    ]

arrivalFraction :: AfTest -> Bool
arrivalFraction (AfTest (n, rank)) =
    (\af@(ArrivalFraction x) -> isNormal x && af > minArrivalFraction) $ FS.arrivalRankFraction n rank
