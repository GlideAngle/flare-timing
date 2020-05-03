module TestNewtypes where

-- NOTE: Avoid orphan instance warnings with these newtypes.

import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Test.QuickCheck.Instances ()

import "flight-gap-lead" Flight.Score

import Flight.Units()

-- | Leading coefficient, clean task.
newtype LcCleanTest = LcCleanTest (LengthOfSs, LcTrack) deriving Show

mkLcTrack :: [Int] -> LcTrack
mkLcTrack xs =
    case toRational <$> xs of
         [] -> LcSeq []
         ys ->
             LcSeq{seq = pts}
             where
                 ts = TaskTime . MkQuantity <$> [1 .. ]
                 ds = DistanceToEss . MkQuantity <$> ys
                 pts =
                     zipWith
                         (\t d -> LcPoint{leg = RaceLeg 0, mark = t, togo = d})
                         ts
                         ds

mkLcCleanTest :: Rational -> [Int] -> LcCleanTest
mkLcCleanTest len xs =
    LcCleanTest (LengthOfSs $ MkQuantity len, mkLcTrack xs)

instance Monad m => SC.Serial m LcCleanTest where
    series = cons2 (\(SC.Positive len) xs -> mkLcCleanTest len xs)

instance QC.Arbitrary LcCleanTest where
    arbitrary = do
        (QC.Positive len) <- arbitrary
        xs <- listOf $ choose (1, 1000000)
        return $ mkLcCleanTest len xs
