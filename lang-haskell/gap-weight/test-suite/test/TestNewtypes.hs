module TestNewtypes where

-- NOTE: Avoid orphan instance warnings with these newtypes.

import Prelude hiding (max)
import qualified Prelude (max)
import Data.Refined (assumeProp, refined)
import Data.Ratio ((%))
import Data.List (sortBy)
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import qualified Statistics.Sample as Stats (meanVariance)
import qualified Data.Vector as V (fromList)
import Data.UnitsOfMeasure (u, zero)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Test.QuickCheck.Instances ()

import Flight.Ratio (pattern (:%))
import "flight-gap-allot" Flight.Score
import "flight-gap-weight" Flight.Score

import Normal (Normal(..), NormalSum(..))
import Flight.Units()

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

-- | Leading weight.
newtype LwTest = LwTest Lw deriving Show

instance Monad m => SC.Serial m LwTest where
    series = LwTest <$> (xs \/ ys \/ zs)
        where
            xs = cons1 $ \(Normal x) -> LwHg (DistanceWeight x)
            ys = cons1 $ \(Normal r) -> LwPgZ (DistanceRatio r)
            zs = cons1 $ \(Normal x) -> LwPg (DistanceWeight x)

instance QC.Arbitrary LwTest where
    arbitrary = LwTest <$> arb
        where
        arb = do
            (Normal r) <- arbitrary
            QC.oneof $ return <$> [ LwHg (DistanceWeight r)
                                  , LwPgZ (DistanceRatio r)
                                  , LwPg (DistanceWeight r)
                                  ]

-- | Arrival weight.
newtype AwTest = AwTest Aw deriving Show

instance Monad m => SC.Serial m AwTest where
    series = AwTest <$> (xs \/ ys \/ zs)
        where
            xs = cons0 AwZero
            ys = cons1 $ \(Normal x) -> AwHgRank (DistanceWeight x)
            zs = cons1 $ \(Normal x) -> AwHgTime (DistanceWeight x)

instance QC.Arbitrary AwTest where
    arbitrary = AwTest <$> arb
        where
            arb = do
                (Normal x) <- arbitrary
                QC.oneof $ return <$> [ AwZero
                                      , AwHgRank (DistanceWeight x)
                                      , AwHgTime (DistanceWeight x)
                                      ]


