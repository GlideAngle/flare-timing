module Flight.Gap.Equation
    ( powerFraction
    ) where

-- |
-- prop> powerFraction 0 x == 0
-- prop> powerFraction x 0 == 0
-- prop> powerFraction x y >= 0
-- prop> powerFraction x y <= 1
-- False
powerFraction :: Double -> Double -> Double
powerFraction 0 _ = 0
powerFraction _ 0 = 0
powerFraction c x =
    max 0 $ 1 - frac
    where
        numerator = x - c
        denominator = c ** (1/2)
        frac = (numerator ** 2 / denominator) ** (1/3)
