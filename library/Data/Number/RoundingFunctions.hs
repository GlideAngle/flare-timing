module Data.Number.RoundingFunctions (dpRound , sdRound) where

-- | Rounds to the given number of decimal places.
-- SEE: https://stackoverflow.com/questions/12450501/round-number-to-specified-number-of-digits
dpRound :: Integer -> Rational -> Double
dpRound n f
    | n < 0 = fromRational f
    | otherwise = fromInteger (round $ f * (10^n)) / (10.0^^n)

-- | Keeps the given number of significant digits with rounding.
sdRound :: Integer -> Rational -> Double
sdRound sd f
    | sd <= 0 = fromRational f
    | otherwise =
        if m < 0
            then
                (dpRound sd gZ) / 10^^pZ
            else
                case compare n 0 of
                    EQ -> dpRound n f
                    GT -> dpRound n f
                    LT -> 10^^p * (fromInteger $ round g)
    where
        f' = fromRational f :: Double

        m = logBase 10 $ f'
        mZ = truncate m

        n = sd - (mZ + 1)

        p = negate n
        pZ = negate mZ

        g = f' / 10^^p
        gZ = f * 10^pZ
