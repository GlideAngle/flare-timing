{-|
Module: Data.Ratio.Rounding
Copyright:
    © 2018 Phil de Joux
    © 2018 Block Scope Limited
License: MPL-2.0
Maintainer: Phil de Joux <phil.dejoux@blockscope.com>
Stability: experimental

Rounding rationals to significant digits and decimal places.

The 'round' function from the prelude returns an integer. The standard librarys
of C and C++ have round functions that return floating point numbers. Rounding
in this library takes and returns 'Rational's and can round to a number of
significant digits or a number of decimal places.
-}
module Data.Ratio.Rounding
    (
    -- * About the Name
    -- $name
    
    -- * Rounding to Decimal Places
      dpRound
    -- * Rounding to Significant Digits
    , sdRound
    ) where

import Numeric.Natural (Natural)

-- | Rounds to a non-negative number of __d__ecimal __p__laces.  After rounding
-- the result would have the given number of decimal places if converted to
-- a floating point number, such as by using 'fromRational'.
--
-- >>> dpRound 2 (1234.56789 :: Rational)
-- 123457 % 100
-- >>> dpRound 2 (123456789 :: Rational)
-- 123456789 % 1
--
-- Some examples that may be easier to read using decimal point notation.
-- 
-- >>> dpRound 2 (123456789 :: Rational) == (123456789 :: Rational)
-- True
-- >>> dpRound 2 (1234.56789 :: Rational) == (1234.57 :: Rational)
-- True
-- >>> dpRound 2 (123.456789 :: Rational) == (123.46 :: Rational)
-- True
-- >>> dpRound 2 (12.3456789 :: Rational) == (12.35 :: Rational)
-- True
-- >>> dpRound 2 (1.23456789 :: Rational) == (1.23 :: Rational)
-- True
-- >>> dpRound 2 (0.123456789 :: Rational) == (0.12 :: Rational)
-- True
-- >>> dpRound 2 (0.0123456789 :: Rational) == (0.01 :: Rational)
-- True
-- >>> dpRound 2 (0.0000123456789 :: Rational) == (0.0 :: Rational)
-- True
--
-- If the required number of decimal places is less than zero it is taken to
-- be zero.
--
-- >>> dpRound 0 (1234.56789 :: Rational)
-- 1235 % 1
-- >>> dpRound (-1) (1234.56789 :: Rational)
-- 1235 % 1
-- >>> dpRound 0 (123456789 :: Rational)
-- 123456789 % 1
-- >>> dpRound (-1) (123456789 :: Rational)
-- 123456789 % 1
--
-- Rounding to the existing number of decimal places or more makes no
-- difference.
--
-- >>> 1234.56789 :: Rational
-- 123456789 % 100000
-- >>> dpRound 5 (1234.56789 :: Rational)
-- 123456789 % 100000
-- >>> dpRound 6 (1234.56789 :: Rational)
-- 123456789 % 100000

-- SEE: https://stackoverflow.com/questions/12450501/round-number-to-specified-number-of-digits
dpRound :: RealFrac a => Integer -> a -> a
dpRound n f
    | n < 0 = dpRound 0 f
    | otherwise =
        fromInteger (round $ f * (10^n)) / (10.0^^n)
{-# INLINABLE dpRound #-}
{-# SPECIALIZE dpRound :: Integer -> Double -> Double #-}
{-# SPECIALIZE dpRound :: Integer -> Rational -> Rational #-}

-- | Rounds to a non-negative number of __s__ignificant __d__igits.
--
-- >>> sdRound 1 (123456789 :: Rational)
-- 100000000 % 1
-- >>> sdRound 4 (123456789 :: Rational)
-- 123500000 % 1
-- >>> sdRound 8 (1234.56789 :: Rational)
-- 12345679 % 10000
--
-- More examples using decimal point notation.
-- 
-- >>> sdRound 4 (123456789 :: Rational) == (123500000 :: Rational)
-- True
-- >>> sdRound 4 (1234.56789 :: Rational) == (1235 :: Rational)
-- True
-- >>> sdRound 4 (123.456789 :: Rational) == (123.5 :: Rational)
-- True
-- >>> sdRound 4 (12.3456789 :: Rational) == (12.35 :: Rational)
-- True
-- >>> sdRound 4 (1.23456789 :: Rational) == (1.235 :: Rational)
-- True
-- >>> sdRound 4 (0.123456789 :: Rational) == (0.1235 :: Rational)
-- True
-- >>> sdRound 4 (0.0123456789 :: Rational) == (0.01235 :: Rational)
-- True
-- >>> sdRound 4 (0.0000123456789 :: Rational) == (0.00001235 :: Rational)
-- True
--
-- Rounding to the existing number of significant digits or more makes no
-- difference.
--
-- >>> 1234.56789 :: Rational
-- 123456789 % 100000
-- >>> sdRound 9 (1234.56789 :: Rational)
-- 123456789 % 100000
-- >>> sdRound 10 (1234.56789 :: Rational)
-- 123456789 % 100000
--
-- Rounding to zero significant digits is always zero.
--
-- >>> sdRound 0 (123456789 :: Rational)
-- 0 % 1
-- >>> sdRound 0 (1234.56789 :: Rational)
-- 0 % 1
-- >>> sdRound 0 (0.123456789 :: Rational)
-- 0 % 1
-- >>> sdRound 0 (0.0000123456789 :: Rational)
-- 0 % 1
sdRound :: RealFrac a => Natural -> a -> a
sdRound sd' f =
    if m < 0
        then
            dpRound sd gZ / 10^^pZ
        else
            case compare n 0 of
                EQ -> dpRound n f
                GT -> dpRound n f
                LT -> 10^^p * fromInteger (round g)
    where
        sd = toInteger sd'

        f' :: Double
        f' = fromRational $ toRational f 

        m = logBase 10 f'
        mZ = truncate m

        n = sd - (mZ + 1)

        p = negate n
        pZ = negate mZ

        g = f / 10^p
        gZ = f * 10^pZ
{-# INLINABLE sdRound #-}
{-# SPECIALIZE sdRound :: Natural -> Double -> Double #-}
{-# SPECIALIZE sdRound :: Natural -> Rational -> Rational #-}

-- $name
-- Rounding to decimal places is a special case of rounding significant digits.
-- When the number is split into whole and fractional parts, rounding to
-- decimal places is rounding to significant digits in the fractional part.
-- 
-- The digits that are discarded become dust and a digit when written down is
-- a char.
--
-- The package name is __siggy__ for significant digits and __chardust__ for
-- the digits that are discarded.
