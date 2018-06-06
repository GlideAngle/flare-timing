{-|
Module      : Data.Number.RoundingFunctions 
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Functions rounding to decimal places and significant digits.
-}
module Data.Number.RoundingFunctions (dpRound, sdRound) where

import Data.Word (Word8)

-- | Rounds to a number of decimal places.

-- SEE: https://stackoverflow.com/questions/12450501/round-number-to-specified-number-of-digits
dpRound
    :: Integer
    -- ^ A positive number of decimal places to keep
    -> Rational
    -- ^ The number to round
    -> Rational
dpRound n f
    | n < 0 = f
    | otherwise =
        fromInteger (round $ f * (10^n)) / (10.0^^n)

-- | Rounds to a number of significant digits.
sdRound
    :: Word8
    -- ^ A positive number of significant digits to keep
    -> Rational
    -- ^ The number to round
    -> Rational
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
