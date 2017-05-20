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
    , GoalRatio(..)
    , DistanceRatio(..)
    , DistanceWeight(..)
    , LeadingWeight(..)
    , ArrivalWeight(..)
    , TimeWeight(..)
    , isNormal
    )

weightingUnits :: TestTree
weightingUnits = testGroup "Weighting unit tests"
    [ HU.testCase "Distance weight 0 == 0.9" $
        FS.distanceWeight (GoalRatio (0 % 1)) @?= DistanceWeight (9 % 10)

    , HU.testCase "Arrival weight (Pg 0) == 0" $
        FS.arrivalWeight AwPg @?= ArrivalWeight (0 % 1)

    , HU.testCase "Arrival weight (Hg 0) == 0.8" $
        FS.arrivalWeight (AwHg (DistanceWeight (0 % 1))) @?= ArrivalWeight (1 % 8)

    , HU.testCase "Arrival weight (Hg 1) == 0" $
        FS.arrivalWeight (AwHg (DistanceWeight (1 % 1))) @?= ArrivalWeight (0 % 1)

    , HU.testCase "Leading weight (PgZ 0) == 0" $
        FS.leadingWeight (LwPgZ (DistanceRatio (0 % 1))) @?= LeadingWeight (0 % 1)

    , HU.testCase "Leading weight (PgZ 1) == 0.1" $
        FS.leadingWeight (LwPgZ (DistanceRatio (1 % 1))) @?= LeadingWeight (1 % 10)

    , HU.testCase "Leading weight (Pg 0) == 0.35" $
        FS.leadingWeight (LwPg (DistanceWeight (0 % 1))) @?= LeadingWeight (7 % 20)

    , HU.testCase "Leading weight (Pg 1) == 0" $
        FS.leadingWeight (LwPg (DistanceWeight (1 % 1))) @?= LeadingWeight (0 % 1)

    , HU.testCase "Leading weight (Hg 0) == 0.175" $
        FS.leadingWeight (LwHg (DistanceWeight (0 % 1))) @?= LeadingWeight (7 % 40)

    , HU.testCase "Leading weight (Hg 1) == 0" $
        FS.leadingWeight (LwHg (DistanceWeight (1 % 1))) @?= LeadingWeight (0 % 1)

    , HU.testCase "Time weight 0 0 0 == 1" $
        FS.timeWeight
            (DistanceWeight (0 % 1))
            (LeadingWeight (0 % 1))
            (ArrivalWeight (0 % 1))
            @?=
            TimeWeight (1 % 1)
    ]

-- NOTE: Avoid orphan instance warnings with these newtypes.
newtype LwTest = LwTest (Lw Rational) deriving Show
newtype AwTestPgZ = AwTestPgZ (Aw ()) deriving Show
newtype AwTest = AwTest (Aw Rational) deriving Show
newtype GrTest = GrTest GoalRatio deriving Show
newtype Normal = Normal Rational deriving Show
newtype TwTest = TwTest (DistanceWeight, LeadingWeight, ArrivalWeight) deriving Show

distanceWeight :: GrTest -> Bool
distanceWeight (GrTest gr) =
    (\(DistanceWeight w) -> isNormal w) $ FS.distanceWeight gr

arrivalWeightPgZ :: AwTestPgZ -> Bool
arrivalWeightPgZ (AwTestPgZ x) =
    (\(ArrivalWeight w) -> isNormal w) $ FS.arrivalWeight x

arrivalWeight :: AwTest -> Bool
arrivalWeight (AwTest x) =
    (\(ArrivalWeight w) -> isNormal w) $ FS.arrivalWeight x

leadingWeight :: LwTest -> Bool
leadingWeight (LwTest x) =
    (\(LeadingWeight w) -> isNormal w) $ FS.leadingWeight x

timeWeight :: TwTest -> Bool
timeWeight (TwTest (d, l, a)) =
    (\(TimeWeight w) -> isNormal w) $ FS.timeWeight d l a

instance Monad m => SC.Serial m Normal where
    series = xs `isSuchThat` \(Normal x) -> isNormal x
        where
        xs = cons2 $ \(SC.NonNegative n) (SC.Positive d) -> Normal (n % d)

        -- SEE: https://github.com/feuerbach/smallcheck/blob/master/src/Test/SmallCheck/Series.hs
        isSuchThat :: Monad m => Series m a -> (a -> Bool) -> Series m a
        isSuchThat s p = s >>= \x -> if p x then pure x else empty

instance Monad m => SC.Serial m GrTest where
    series = cons1 $ \(Normal x) -> GrTest (GoalRatio x)

instance Monad m => SC.Serial m LwTest where
    series = LwTest <$> (xs \/ ys)
        where
            xs = cons1 $ \(Normal x) -> LwHg (DistanceWeight x)
            ys = cons1 $ \(Normal x) -> LwPg (DistanceWeight x)

instance Monad m => SC.Serial m AwTestPgZ where
    series = cons0 $ AwTestPgZ AwPg

instance Monad m => SC.Serial m AwTest where
    series = cons1 $ \(Normal x) -> AwTest (AwHg (DistanceWeight x))

instance Monad m => SC.Serial m TwTest where
    series =
        cons3 $ \(Normal x) (Normal y) (Normal z) ->
             TwTest (DistanceWeight x, LeadingWeight y, ArrivalWeight z)

instance QC.Arbitrary Normal where
    arbitrary = Normal <$> QC.suchThat arb isNormal
        where
        arb = do
            (QC.NonNegative n) <- arbitrary
            (QC.Positive d) <- arbitrary 
            return $ n % d

instance QC.Arbitrary GrTest where
    arbitrary = arbitrary >>= \(Normal x) -> return $ GrTest (GoalRatio x)

instance QC.Arbitrary LwTest where
    arbitrary = LwTest <$> arb
        where
        arb = do
            (Normal r) <- arbitrary
            QC.oneof $ return <$> [ LwHg (DistanceWeight r)
                                  , LwPgZ (DistanceRatio r)
                                  , LwPg (DistanceWeight r)
                                  ]

instance QC.Arbitrary AwTestPgZ where
    arbitrary = arbitrary >>= \() -> return $ AwTestPgZ AwPg

instance QC.Arbitrary AwTest where
    arbitrary = do
        (Normal x) <- arbitrary
        return $ AwTest (AwHg (DistanceWeight x))

instance QC.Arbitrary TwTest where
    arbitrary = do
        (Normal x) <- arbitrary
        (Normal y) <- arbitrary
        (Normal z) <- arbitrary
        return $ TwTest (DistanceWeight x, LeadingWeight y, ArrivalWeight z)
