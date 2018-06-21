﻿{-|
Module: Data.Number.Rounding
Copyright:
    © 2018 Phil de Joux
    © 2018 Block Scope Limited
License: MPL-2.0
Maintainer: Phil de Joux <phil.dejoux@blockscope.com>
Stability: experimental

Rounding rationals to significant digits and decimal places.

The round function from the prelude returns an integer. The standard librarys
of C and C++ have round functions that return floating point numbers. Rounding
in this library takes and returns 'Rational's and can round to a number of
significant digits or a number of decimal places.
-}
module Data.Number.Rounding
    (
    -- * Usage
    -- $use
    
    -- * About the Name
    -- $name
    
    -- * Rounding to Decimal Places
      dpRound
    -- * Rounding to Significant Digits
    , sdRound
    ) where

import Data.Word (Word8)

-- | Rounds to a number of __d__ecimal __p__laces.
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

-- SEE: https://stackoverflow.com/questions/12450501/round-number-to-specified-number-of-digits
dpRound :: Integer -> Rational -> Rational
dpRound n f
    | n < 0 = f
    | otherwise =
        fromInteger (round $ f * (10^n)) / (10.0^^n)

-- | Rounds to a number of __s__ignificant __d__igits.
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
sdRound :: Word8 -> Rational -> Rational
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
        f' = fromRational f :: Double

        m = logBase 10 f'
        mZ = truncate m

        n = sd - (mZ + 1)

        p = negate n
        pZ = negate mZ

        g = f / 10^p
        gZ = f * 10^pZ

-- $use
-- After rounding the result would have a given number of significant digits or
-- decimal places if converted to a floating point number, such as by using
-- 'fromRational'.

-- $name
-- Rounding to decimal places is a special case of rounding significant digits.
-- When the number is split into whole and fractional parts, rounding to
-- decimal places is rounding to significant digits in the fractional part.
-- The digits that are discarded become dust and a digit when written down is
-- a char.  So the name is __siggy__ for significant digits and __chardust__
-- for the digits that are discarded.
