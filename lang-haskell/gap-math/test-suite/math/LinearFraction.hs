module LinearFraction (linearFractionUnits, linearFraction) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Ratio ((%))
import Data.UnitsOfMeasure (u)

import qualified "flight-gap-allot" Flight.Score as FS
import "flight-gap-allot" Flight.Score
    ( FlownMax(..)
    , PilotDistance(..)
    , LinearFraction(..)
    , isNormal
    )

import TestNewtypes

linearFractionUnits :: TestTree
linearFractionUnits = testGroup "Linear fraction unit tests"
    [ HU.testCase "1 best distance, 1 pilot distance = 1 linear fraction" $
        FS.linearFraction (FlownMax [u| 1 km |]) (PilotDistance [u| 1 km |])
        @?= LinearFraction (1 % 1)

    , HU.testCase "2 best distance, 1 pilot distance = 0.5 linear fraction" $
        FS.linearFraction (FlownMax [u| 2 km |]) (PilotDistance [u| 1 km |])
        @?= LinearFraction (1 % 2)

    , HU.testCase "10 best distance, 1 pilot distance = 0.1 linear fraction" $
        FS.linearFraction (FlownMax [u| 10 km |]) (PilotDistance [u| 1 km |])
        @?= LinearFraction (1 % 10)

    , HU.testCase "100 best distance, 1 pilot distance = 0.01 linear fraction" $
        FS.linearFraction (FlownMax [u| 100 km |]) (PilotDistance [u| 1 km |])
        @?= LinearFraction (1 % 100)
    ]

linearFraction :: LfTest -> Bool
linearFraction (LfTest (best, pilot)) =
    (\(LinearFraction x) -> isNormal x)
    $ FS.linearFraction best pilot
