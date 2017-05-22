module ArrivalFraction (arrivalFractionUnits) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Ratio ((%))

import qualified Flight.Score as FS
import Flight.Score
    ( PilotsAtEss(..)
    , PositionAtEss(..)
    , ArrivalFraction(..)
    )

arrivalFractionUnits :: TestTree
arrivalFractionUnits = testGroup "Arrival fraction unit tests"
    [ HU.testCase "1 pilot at ESS, first at ESS == 1 arrival fraction" $
        FS.arrivalFraction (PilotsAtEss 1) (PositionAtEss 1) @?= ArrivalFraction (1 % 1)

    , HU.testCase "2 pilots at ESS, first at ESS == 1 arrival fraction" $
        FS.arrivalFraction (PilotsAtEss 2) (PositionAtEss 1) @?= ArrivalFraction (1 % 1)

    , HU.testCase "2 pilots at ESS, last at ESS > 0.2 arrival fraction" $
        FS.arrivalFraction (PilotsAtEss 2) (PositionAtEss 2) `compare` ArrivalFraction (1 % 5)
        @?= GT

    , HU.testCase "1000 pilots at ESS, last at ESS > 0.2 arrival fraction" $
        FS.arrivalFraction (PilotsAtEss 1000) (PositionAtEss 1000) `compare` ArrivalFraction (1 % 5)
        @?= GT
    ]

