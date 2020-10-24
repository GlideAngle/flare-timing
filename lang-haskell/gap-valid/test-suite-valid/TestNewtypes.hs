module TestNewtypes where

-- NOTE: Avoid orphan instance warnings with these newtypes.

import Prelude hiding (max)
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import qualified Statistics.Sample as Stats (meanVariance)
import qualified Data.Vector as V (fromList)
import Data.UnitsOfMeasure (u, zero)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Test.QuickCheck.Instances ()

import "flight-gap-allot" Flight.Score
import "flight-gap-valid" Flight.Score

import Normal (Normal(..), NormalSum(..))
import Flight.Units()

-- | Nominal goal.
newtype NgTest = NgTest NominalGoal deriving Show

instance Monad m => SC.Serial m NgTest where
    series = cons1 $ \(Normal x) -> NgTest (NominalGoal x)

instance QC.Arbitrary NgTest where
    arbitrary = arbitrary >>= \(Normal x) -> return $ NgTest (NominalGoal x)

-- | Nominal distance.
newtype NdTest = NdTest (NominalDistance (Quantity Double [u| km |]))
    deriving Show

instance Monad m => SC.Serial m NdTest where
    series =
        cons1
        $ \(SC.NonNegative x) ->
            NdTest (NominalDistance $ MkQuantity x)

instance QC.Arbitrary NdTest where
    arbitrary =
        arbitrary
        >>= \(QC.NonNegative x) ->
            return $ NdTest (NominalDistance $ MkQuantity x)

-- | Task validity
newtype TvTest = TvTest (LaunchValidity, DistanceValidity, TimeValidity) deriving Show

instance Monad m => SC.Serial m TvTest where
    series =
        cons1 $ \(NormalSum (x, y, z)) ->
             TvTest (LaunchValidity x, DistanceValidity y, TimeValidity z)

instance QC.Arbitrary TvTest where
    arbitrary = do
        (NormalSum (x, y, z)) <- arbitrary
        return $ TvTest (LaunchValidity x, DistanceValidity y, TimeValidity z)

mkReachStats :: [Double] -> ReachToggle (Maybe ReachStats)

mkReachStats [] =
    let x =
            Just
                ReachStats
                    { max = FlownMax zero
                    , mean = FlownMean zero
                    , stdDev = FlownStdDev zero
                    }

    in ReachToggle{extra = x, flown = x}

mkReachStats xs =
    let (xsMean, xsVariance) = Stats.meanVariance $ V.fromList xs
        x =
            Just
                ReachStats
                    { max = FlownMax . MkQuantity $ maximum xs
                    , mean = FlownMean $ MkQuantity xsMean
                    , stdDev = FlownStdDev . MkQuantity $ sqrt xsVariance
                    }

    in ReachToggle{extra = x, flown = x}

-- | Stopped task validity.
newtype StopValidityTest =
    StopValidityTest
        (
            ( PilotsFlying
            , PilotsAtEss
            , PilotsLanded
            , PilotsFlying
            , LaunchToEss (Quantity Double [u| km |])
            )
        ,
            ReachToggle (Maybe ReachStats)
        )
        deriving Show

instance Monad m => SC.Serial m StopValidityTest where
    series = decDepth $ curry StopValidityTest <$> x <~> cons1 mkReachStats
        where
            x = cons4 (\(SC.NonNegative notLanded)
                        (SC.NonNegative atEss)
                        (SC.NonNegative landed)
                        (SC.Positive d) ->
                            ( PilotsFlying (notLanded + atEss + landed)
                            , PilotsAtEss $ fromIntegral atEss
                            , PilotsLanded $ fromIntegral landed
                            , PilotsFlying notLanded
                            , LaunchToEss $ MkQuantity d))

instance QC.Arbitrary StopValidityTest where
    arbitrary = StopValidityTest <$> do
        (QC.NonNegative notLanded) <- arbitrary
        (QC.NonNegative atEss) <- arbitrary
        (QC.NonNegative landed) <- arbitrary
        (QC.Positive d) <- arbitrary
        xs <- listOf $ choose (1, 10000)

        let x =
                ( PilotsFlying (notLanded + atEss + landed)
                , PilotsAtEss $ fromIntegral atEss
                , PilotsLanded $ fromIntegral landed
                , PilotsFlying notLanded
                , LaunchToEss $ MkQuantity d
                )

        return (x, mkReachStats xs)

