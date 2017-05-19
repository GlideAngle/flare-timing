module Weighting
    ( weightingUnits
    ) where

import qualified Flight.Score as FS
import Flight.Score (Lw(..), Aw(..))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Ratio ((%))

weightingUnits :: TestTree
weightingUnits = testGroup "Weighting unit tests"
    [ HU.testCase "Distance weight 0 == 0.9" $
        FS.distanceWeight (0 % 1) @?= (9 % 10)

    , HU.testCase "Arrival weight (Pg 0) == 0" $
        FS.arrivalWeight AwPg @?= (0 % 1)

    , HU.testCase "Arrival weight (Hg 0) == 0.8" $
        FS.arrivalWeight (AwHg (0 % 1)) @?= (1 % 8)

    , HU.testCase "Arrival weight (Hg 1) == 0" $
        FS.arrivalWeight (AwHg (1 % 1)) @?= (0 % 1)

    , HU.testCase "Leading weight (PgZ 0) == 0" $
        FS.leadingWeight (LwPgZ (0 % 1)) @?= (0 % 1)

    , HU.testCase "Leading weight (PgZ 1) == 0.1" $
        FS.leadingWeight (LwPgZ (1 % 1)) @?= (1 % 10)

    , HU.testCase "Leading weight (Pg 0) == 0.35" $
        FS.leadingWeight (LwPg (0 % 1)) @?= (7 % 20)

    , HU.testCase "Leading weight (Pg 1) == 0" $
        FS.leadingWeight (LwPg (1 % 1)) @?= (0 % 1)

    , HU.testCase "Leading weight (Hg 0) == 0.175" $
        FS.leadingWeight (LwHg (0 % 1)) @?= (7 % 40)

    , HU.testCase "Leading weight (Hg 1) == 0" $
        FS.leadingWeight (LwHg (1 % 1)) @?= (0 % 1)

    , HU.testCase "Time weight 0 0 0 == 1" $
        FS.timeWeight (0 % 1) (0 % 1) (0 % 1) @?= (1 % 1)
    ]
