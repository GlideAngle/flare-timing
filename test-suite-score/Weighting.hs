{-# LANGUAGE ScopedTypeVariables #-}
module Weighting
    ( weightingUnits
    , distanceWeight
    , qcArrivalWeightPgZ
    , qcArrivalWeight
    , qcLeadingWeight
    , timeWeight
    ) where

import qualified Flight.Score as FS
import Flight.Score
    ( Lw(..)
    , Aw(..)
    , GoalRatio
    , DistanceWeight
    , LeadingWeight
    , ArrivalWeight
    )

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QC
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

-- NOTE: Avoid orphan instance warnings with these newtypes.
newtype LwTest = LwTest (Lw Rational) deriving (Show)
newtype AwTestPgZ = AwTestPgZ (Aw ()) deriving (Show)
newtype AwTest = AwTest (Aw Rational) deriving (Show)

distanceWeight :: GoalRatio -> Bool
distanceWeight gr =
    let w = FS.distanceWeight gr
    in w >= (0 % 1) && w <= (1 % 1)

qcArrivalWeightPgZ :: AwTestPgZ -> Bool
qcArrivalWeightPgZ (AwTestPgZ x) =
    let w = FS.arrivalWeight x
    in w >= (0 % 1) && w <= (1 % 1)

qcArrivalWeight :: AwTest -> Bool
qcArrivalWeight (AwTest x) =
    let w = FS.arrivalWeight x
    in w >= (0 % 1) && w <= (1 % 1)

qcLeadingWeight :: LwTest -> Bool
qcLeadingWeight (LwTest x) =
    let w = FS.leadingWeight x
    in w >= (0 % 1) && w <= (1 % 1)

timeWeight :: DistanceWeight
                -> LeadingWeight
                -> ArrivalWeight
                -> Bool
timeWeight d l a =
    let w = FS.timeWeight d l a
    in w >= (0 % 1) && w <= (1 % 1)

instance QC.Arbitrary LwTest where arbitrary = LwTest <$> lwArb
instance QC.Arbitrary AwTestPgZ where arbitrary = AwTestPgZ <$> awArbPgZ
instance QC.Arbitrary AwTest where arbitrary = AwTest <$> awArb

lwArb :: Gen (Lw Rational)
lwArb =
    QC.oneof
        [ do
            (QC.NonNegative n) <- arbitrary
            (QC.Positive d) <- arbitrary
            return $ LwHg (n % d)
        , do
            (QC.NonNegative n) <- arbitrary
            (QC.Positive d) <- arbitrary
            return $ LwPgZ (n % d)
        , do
            (QC.NonNegative n) <- arbitrary
            (QC.Positive d) <- arbitrary
            return $ LwPg (n % d)
        ]

awArbPgZ :: Gen (Aw ())
awArbPgZ = return AwPg

awArb :: Gen (Aw Rational)
awArb =
    QC.oneof
        [ do
            (QC.NonNegative n) <- arbitrary
            (QC.Positive d) <- arbitrary
            return $ AwHg (n % d)
        ]
