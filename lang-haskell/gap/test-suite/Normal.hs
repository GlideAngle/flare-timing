{-# LANGUAGE CPP #-}

module Normal (Normal(..), NormalProduct(..), NormalSum(..)) where

import Control.Applicative
  ( empty
#if !MIN_VERSION_base(4,9,0)
  , pure
#endif
  )
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Data.Ratio ((%))

import "flight-gap-allot" Flight.Score (isNormal, isFoldNormal)

newtype Normal a = Normal a deriving Show
newtype NormalProduct a = NormalProduct a deriving Show
newtype NormalSum a = NormalSum a deriving Show
--
-- SEE: https://github.com/feuerbach/smallcheck/blob/master/src/Test/SmallCheck/Series.hs
isSuchThat :: Monad m => Series m a -> (a -> Bool) -> Series m a
isSuchThat s p = s >>= \x -> if p x then pure x else empty

joinProduct :: (Normal Rational, Normal Rational, Normal Rational)
               -> NormalProduct (Rational, Rational, Rational)
joinProduct (Normal a, Normal b, Normal c) = NormalProduct (a, b, c)

joinSum :: (Normal Rational, Normal Rational, Normal Rational)
           -> NormalSum (Rational, Rational, Rational)
joinSum (Normal a, Normal b, Normal c) = NormalSum (a, b, c)

isNormalProduct :: (Normal Rational, Normal Rational, Normal Rational) -> Bool
isNormalProduct (Normal a, Normal b, Normal c) = isFoldNormal (*) (1 % 1) [a, b, c]

isNormalSum :: (Normal Rational, Normal Rational, Normal Rational) -> Bool
isNormalSum (Normal a, Normal b, Normal c) = isFoldNormal (+) (0 % 1) [a, b, c]

arbTriple :: Gen (Normal Rational, Normal Rational, Normal Rational)
arbTriple = do
    (Normal a) <- arbitrary
    (Normal b) <- arbitrary
    (Normal c) <- arbitrary
    return (Normal a, Normal b, Normal c)

instance Monad m => SC.Serial m (Normal Rational) where
    series = xs `isSuchThat` \(Normal x) -> isNormal x
        where
        xs = cons2 $ \(SC.NonNegative n) (SC.Positive d) -> Normal (n % d)

instance QC.Arbitrary (Normal Rational) where
    arbitrary = Normal <$> QC.suchThat arb isNormal
        where
        arb = do
            (QC.NonNegative n) <- arbitrary
            (QC.Positive d) <- arbitrary 
            return $ n % d

instance Monad m => SC.Serial m (NormalProduct (Rational, Rational, Rational)) where
    series = joinProduct <$> xs `isSuchThat` isNormalProduct
        where
        xs = cons3 $ \(Normal a) (Normal b) (Normal c) -> (Normal a, Normal b, Normal c)

instance QC.Arbitrary (NormalProduct (Rational, Rational, Rational)) where
    arbitrary = joinProduct <$> xs
        where
        xs = QC.suchThat arbTriple isNormalProduct

instance Monad m => SC.Serial m (NormalSum (Rational, Rational, Rational)) where
    series = joinSum <$> xs `isSuchThat` isNormalSum
        where
        xs = cons3 $ \(Normal a) (Normal b) (Normal c) -> (Normal a, Normal b, Normal c)

instance QC.Arbitrary (NormalSum (Rational, Rational, Rational)) where
    arbitrary = joinSum <$> xs
        where
        xs = QC.suchThat arbTriple isNormalSum
