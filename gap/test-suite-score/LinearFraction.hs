module LinearFraction (linearFractionUnits, linearFraction) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Ratio ((%))

import qualified Flight.Score as FS
import Flight.Score
    ( BestDistance(..)
    , PilotDistance(..)
    , LinearFraction(..)
    , isNormal
    )

import TestNewtypes

oneRat :: Rational
oneRat = 1 % 1

linearFractionUnits :: TestTree
linearFractionUnits = testGroup "Linear fraction unit tests"
    [ HU.testCase "1 best distance, 1 pilot distance = 1 linear fraction" $
        FS.linearFraction (BestDistance 1) (PilotDistance oneRat)
        @?= LinearFraction (1 % 1)

    , HU.testCase "2 best distance, 1 pilot distance = 0.5 linear fraction" $
        FS.linearFraction (BestDistance 2) (PilotDistance oneRat)
        @?= LinearFraction (1 % 2)

    , HU.testCase "10 best distance, 1 pilot distance = 0.1 linear fraction" $
        FS.linearFraction (BestDistance 10) (PilotDistance oneRat)
        @?= LinearFraction (1 % 10)

    , HU.testCase "100 best distance, 1 pilot distance = 0.01 linear fraction" $
        FS.linearFraction (BestDistance 100) (PilotDistance oneRat)
        @?= LinearFraction (1 % 100)
    ]

linearFraction :: LfTest -> Bool
linearFraction (LfTest (best, pilot)) =
    (\(LinearFraction x) -> isNormal x) $ FS.linearFraction best pilot
