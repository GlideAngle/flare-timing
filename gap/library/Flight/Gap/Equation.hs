module Flight.Gap.Equation
    ( powerFraction
    ) where

powerFraction :: Double -> Double -> Double
powerFraction 0 _ = 0
powerFraction _ 0 = 0
powerFraction c x =
    max 0 $ 1 - frac
    where
        numerator = x - c
        denominator = c ** (1/2)
        frac = (numerator ** 2 / denominator) ** (1/3)
