module Data.Ratio.Rounding (dpRound) where

dpRound :: Integer -> Rational -> Rational
dpRound n f
    | n < 0 = dpRound 0 f
    | otherwise =
        fromInteger (round $ f * (10^n)) / (10.0^^n)
