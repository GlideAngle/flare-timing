{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Weighting
    ( weightingUnits
    , distanceWeight
    , arrivalWeightPgZ
    , arrivalWeight
    , leadingWeight
    , timeWeight
    ) where

import Control.Applicative (pure, empty)
import Test.Tasty (TestTree, testGroup)
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Ratio ((%))

import qualified Flight.Score as FS
import Flight.Score
    ( Lw(..)
    , Aw(..)
    , GoalRatio
    , DistanceWeight
    , LeadingWeight
    , ArrivalWeight
    , isNormal
    )

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
newtype GrTest = GrTest GoalRatio deriving (Show)
newtype Normal = Normal Rational deriving (Show)

distanceWeight :: GrTest -> Bool
distanceWeight (GrTest gr) = isNormal $ FS.distanceWeight gr

arrivalWeightPgZ :: AwTestPgZ -> Bool
arrivalWeightPgZ (AwTestPgZ x) = isNormal $ FS.arrivalWeight x

arrivalWeight :: AwTest -> Bool
arrivalWeight (AwTest x) = isNormal $ FS.arrivalWeight x

leadingWeight :: LwTest -> Bool
leadingWeight (LwTest x) = isNormal $ FS.leadingWeight x

timeWeight :: DistanceWeight
                -> LeadingWeight
                -> ArrivalWeight
                -> Bool
timeWeight d l a = isNormal $ FS.timeWeight d l a

instance Monad m => SC.Serial m Normal where
    series = xs `isSuchThat` \(Normal x) -> isNormal x
        where
        xs = cons2 $ \(SC.NonNegative n) (SC.Positive d) -> Normal (n % d)

        -- SEE: https://github.com/feuerbach/smallcheck/blob/master/src/Test/SmallCheck/Series.hs
        isSuchThat :: Monad m => Series m a -> (a -> Bool) -> Series m a
        isSuchThat s p = s >>= \x -> if p x then pure x else empty

instance Monad m => SC.Serial m GrTest where
    series = cons1 $ \(Normal x) -> GrTest x

instance Monad m => SC.Serial m LwTest where
    series = LwTest <$> (xs \/ ys)
        where
            xs = cons1 $ \(Normal x) -> LwHg x
            ys = cons1 $ \(Normal x) -> LwPg x

instance Monad m => SC.Serial m AwTestPgZ where
    series = cons0 $ AwTestPgZ AwPg

instance Monad m => SC.Serial m AwTest where
    series = cons1 $ \(Normal x) -> AwTest (AwHg x)

instance QC.Arbitrary Normal where
    arbitrary = Normal <$> QC.suchThat arb isNormal
        where
        arb = do
            (QC.NonNegative n) <- arbitrary
            (QC.Positive d) <- arbitrary 
            return $ n % d

instance QC.Arbitrary GrTest where
    arbitrary = arbitrary >>= \(Normal x) -> return $ GrTest x

instance QC.Arbitrary LwTest where
    arbitrary = LwTest <$> arb
        where
        arb = do
            (Normal r) <- arbitrary
            QC.oneof $ return <$> [ LwHg r, LwPgZ r, LwPg r ]

instance QC.Arbitrary AwTestPgZ where
    arbitrary = arbitrary >>= \() -> return $ AwTestPgZ AwPg

instance QC.Arbitrary AwTest where
    arbitrary = arbitrary >>= \(Normal x) -> return $ AwTest (AwHg x)
