{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Normal (Normal(..)) where

import Control.Applicative (pure, empty)
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Data.Ratio ((%))

import Flight.Score (isNormal)

newtype Normal = Normal Rational deriving Show

instance Monad m => SC.Serial m Normal where
    series = xs `isSuchThat` \(Normal x) -> isNormal x
        where
        xs = cons2 $ \(SC.NonNegative n) (SC.Positive d) -> Normal (n % d)

        -- SEE: https://github.com/feuerbach/smallcheck/blob/master/src/Test/SmallCheck/Series.hs
        isSuchThat :: Monad m => Series m a -> (a -> Bool) -> Series m a
        isSuchThat s p = s >>= \x -> if p x then pure x else empty

instance QC.Arbitrary Normal where
    arbitrary = Normal <$> QC.suchThat arb isNormal
        where
        arb = do
            (QC.NonNegative n) <- arbitrary
            (QC.Positive d) <- arbitrary 
            return $ n % d
