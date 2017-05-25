{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# lANGUAGE PatternSynonyms #-}
module TestNewtypes where

-- NOTE: Avoid orphan instance warnings with these newtypes.

import Data.Ratio ((%))
import Data.List (sortBy)
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC

import Flight.Ratio (pattern (:%))
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
    , LaunchValidity(..)
    , TimeValidity(..)
    , DistanceValidity(..)
    , PilotsAtEss(..)
    , PositionAtEss(..)
    , BestTime(..)
    , PilotTime(..)
    , BestDistance(..)
    , PilotDistance(..)
    , TaskTime(..)
    , DistanceToEss(..)
    , LcTrack(..)
    , TaskDeadline(..)
    , LengthOfSs(..)
    )
import Normal (Normal(..), NormalSum(..))

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
        cons1 $ \(NormalSum (x, y, z)) ->
             TwTest (DistanceWeight x, LeadingWeight y, ArrivalWeight z)

instance QC.Arbitrary TwTest where
    arbitrary = do
        (NormalSum (x, y, z)) <- arbitrary
        return $ TwTest (DistanceWeight x, LeadingWeight y, ArrivalWeight z)

-- | Task validity
newtype TvTest = TvTest (LaunchValidity, TimeValidity, DistanceValidity) deriving Show

instance Monad m => SC.Serial m TvTest where
    series =
        cons1 $ \(NormalSum (x, y, z)) ->
             TvTest (LaunchValidity x, TimeValidity y, DistanceValidity z)

instance QC.Arbitrary TvTest where
    arbitrary = do
        (NormalSum (x, y, z)) <- arbitrary
        return $ TvTest (LaunchValidity x, TimeValidity y, DistanceValidity z)

-- | Arrival fraction
newtype AfTest = AfTest (PilotsAtEss, PositionAtEss) deriving Show

instance Monad m => SC.Serial m AfTest where
    series =
        cons1 $ \(Normal (rank :% n)) ->
             AfTest (PilotsAtEss n, PositionAtEss $ max 1 rank)

instance QC.Arbitrary AfTest where
    arbitrary = do
        (Normal (rank :% n)) <- arbitrary
        return $ AfTest (PilotsAtEss n, PositionAtEss $ max 1 rank)

-- | Speed fraction
newtype SfTest = SfTest (BestTime, PilotTime) deriving Show

instance Monad m => SC.Serial m SfTest where
    series =
        cons1 $ \(Normal (n :% d)) ->
             SfTest (BestTime (d % 1), PilotTime $ (d + n) % 1)

instance QC.Arbitrary SfTest where
    arbitrary = do
        (Normal (n :% d)) <- arbitrary
        return $ SfTest (BestTime (d % 1), PilotTime $ (d + n) % 1)

-- | Linear fraction 
newtype LfTest = LfTest (BestDistance, PilotDistance) deriving Show

instance Monad m => SC.Serial m LfTest where
    series =
        cons1 $ \(Normal (n :% d)) ->
             LfTest (BestDistance $ (d + n) % 1, PilotDistance (d % 1))

instance QC.Arbitrary LfTest where
    arbitrary = do
        (Normal (n :% d)) <- arbitrary
        return $ LfTest (BestDistance $ (d + n) % 1, PilotDistance (d % 1))

-- | Difficulty fraction
newtype DfTest = DfTest [PilotDistance] deriving Show

mkDfTest :: [Int] -> DfTest
mkDfTest xs =
    case toRational <$> sortBy (flip compare) (abs <$> xs) of
         [] -> DfTest []
         ys -> DfTest (PilotDistance <$> reverse ys)

instance Monad m => SC.Serial m DfTest where
    series = cons1 mkDfTest

instance QC.Arbitrary DfTest where
    arbitrary = do
        xs <- listOf $ choose (1, 1000000)
        return $ mkDfTest xs

-- | Leading coefficient, clean task.
newtype LcCleanTest = LcCleanTest (LengthOfSs, LcTrack) deriving Show

mkLcTrack :: [Int] -> LcTrack
mkLcTrack xs =
    case toRational <$> xs of
         [] -> LcTrack []
         ys -> LcTrack $ zip (TaskTime <$> [1 .. ]) (DistanceToEss <$> ys)

mkLcCleanTest :: Rational -> [Int] -> LcCleanTest
mkLcCleanTest len xs =
    LcCleanTest (LengthOfSs len, mkLcTrack xs)

instance Monad m => SC.Serial m LcCleanTest where
    series = cons2 (\(SC.Positive len) xs -> mkLcCleanTest len xs)

instance QC.Arbitrary LcCleanTest where
    arbitrary = do
        (QC.Positive len) <- arbitrary
        xs <- listOf $ choose (1, 1000000)
        return $ mkLcCleanTest len xs

-- | Leading coefficient, leading fractions.
newtype LcTest = LcTest (TaskDeadline, LengthOfSs, [LcTrack]) deriving Show

mkLcTest :: Rational -> Rational -> [[Int]] -> LcTest
mkLcTest deadline len xs =
    LcTest (TaskDeadline deadline, LengthOfSs len, mkLcTrack <$> xs)

instance Monad m => SC.Serial m LcTest where
    series =
        cons3
            (\(SC.Positive deadline) (SC.Positive len) xs ->
                mkLcTest deadline len xs)

instance QC.Arbitrary LcTest where
    arbitrary = do
        (QC.Positive deadline) <- arbitrary
        (QC.Positive len) <- arbitrary
        xs <- listOf $ listOf $ choose (1, 1000000)
        return $ mkLcTest deadline len xs
