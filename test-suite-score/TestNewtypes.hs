{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TestNewtypes where

-- NOTE: Avoid orphan instance warnings with these newtypes.

import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC

import Normal (Normal(..))
import Flight.Score
    ( Lw(..)
    , Aw(..)
    , GoalRatio(..)
    , NominalGoal(..)
    , NominalDistance(..)
    , DistanceRatio(..)
    , DistanceWeight(..)
    , LeadingWeight(..)
    , ArrivalWeight(..)
    )

-- | Nominal goal.
newtype NgTest = NgTest NominalGoal deriving Show

instance Monad m => SC.Serial m NgTest where
    series = cons1 $ \(Normal x) -> NgTest (NominalGoal x)

instance QC.Arbitrary NgTest where
    arbitrary = arbitrary >>= \(Normal x) -> return $ NgTest (NominalGoal x)

-- | Nominal distance.
newtype NdTest = NdTest NominalDistance deriving Show

instance Monad m => SC.Serial m NdTest where
    series = cons1 $ \(SC.NonNegative x) -> NdTest (NominalDistance x)

instance QC.Arbitrary NdTest where
    arbitrary = arbitrary >>= \(QC.NonNegative x) -> return $ NdTest (NominalDistance x)

-- | Leading weight.
newtype LwTest = LwTest (Lw Rational) deriving Show

instance Monad m => SC.Serial m LwTest where
    series = LwTest <$> (xs \/ ys)
        where
            xs = cons1 $ \(Normal x) -> LwHg (DistanceWeight x)
            ys = cons1 $ \(Normal x) -> LwPg (DistanceWeight x)

instance QC.Arbitrary LwTest where
    arbitrary = LwTest <$> arb
        where
        arb = do
            (Normal r) <- arbitrary
            QC.oneof $ return <$> [ LwHg (DistanceWeight r)
                                  , LwPgZ (DistanceRatio r)
                                  , LwPg (DistanceWeight r)
                                  ]

-- | Arrival weight for paragliding when goal ratio is zero.
newtype AwTestPgZ = AwTestPgZ (Aw ()) deriving Show

instance Monad m => SC.Serial m AwTestPgZ where
    series = cons0 $ AwTestPgZ AwPg

instance QC.Arbitrary AwTestPgZ where
    arbitrary = arbitrary >>= \() -> return $ AwTestPgZ AwPg

-- | Arrival weight.
newtype AwTest = AwTest (Aw Rational) deriving Show

instance Monad m => SC.Serial m AwTest where
    series = cons1 $ \(Normal x) -> AwTest (AwHg (DistanceWeight x))

instance QC.Arbitrary AwTest where
    arbitrary = do
        (Normal x) <- arbitrary
        return $ AwTest (AwHg (DistanceWeight x))

-- | Goal ratio.
newtype GrTest = GrTest GoalRatio deriving Show

instance Monad m => SC.Serial m GrTest where
    series = cons1 $ \(Normal x) -> GrTest (GoalRatio x)

instance QC.Arbitrary GrTest where
    arbitrary = arbitrary >>= \(Normal x) -> return $ GrTest (GoalRatio x)

-- | Time weight.
newtype TwTest = TwTest (DistanceWeight, LeadingWeight, ArrivalWeight) deriving Show

instance Monad m => SC.Serial m TwTest where
    series =
        cons3 $ \(Normal x) (Normal y) (Normal z) ->
             TwTest (DistanceWeight x, LeadingWeight y, ArrivalWeight z)

instance QC.Arbitrary TwTest where
    arbitrary = do
        (Normal x) <- arbitrary
        (Normal y) <- arbitrary
        (Normal z) <- arbitrary
        return $ TwTest (DistanceWeight x, LeadingWeight y, ArrivalWeight z)

