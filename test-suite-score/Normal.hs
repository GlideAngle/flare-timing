{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Normal (Normal(..), NormalTriple(..)) where

import Control.Applicative (pure, empty)
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Data.Ratio ((%))

import Flight.Score (isNormal)

isNormalTriple :: NormalTriple -> Bool
isNormalTriple (NormalTriple (a, b, c)) = isNormal $ a * b * c

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

newtype NormalTriple = NormalTriple (Rational, Rational, Rational) deriving Show

instance Monad m => SC.Serial m NormalTriple where
    series = xs `isSuchThat` isNormalTriple
        where
        xs = cons3 $ \(Normal a) (Normal b) (Normal c) -> NormalTriple (a, b, c)

        -- SEE: https://github.com/feuerbach/smallcheck/blob/master/src/Test/SmallCheck/Series.hs
        isSuchThat :: Monad m => Series m a -> (a -> Bool) -> Series m a
        isSuchThat s p = s >>= \x -> if p x then pure x else empty

instance QC.Arbitrary NormalTriple where
    arbitrary = QC.suchThat arb isNormalTriple
        where
        arb = do
            (Normal a) <- arbitrary
            (Normal b) <- arbitrary
            (Normal c) <- arbitrary
            return $ NormalTriple (a, b, c)
