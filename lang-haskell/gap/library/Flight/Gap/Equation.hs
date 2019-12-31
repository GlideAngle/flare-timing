module Flight.Gap.Equation
    ( powerFraction
    , arrivalTimePowerFraction
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
        frac = (numerator / denominator) ** (2/3)
        -- REVIEW: The above is the formula that is used by FS while the
        -- commented one is the formula from the published GAP doc.
        -- frac = (numerator ** 2 / denominator) ** (1/3)

arrivalTimePowerFraction :: Double -> Double -> Double
arrivalTimePowerFraction 0 _ = 0
arrivalTimePowerFraction _ 0 = 0
arrivalTimePowerFraction c x =
    max 0 $ (1 + (2.0/3.0) * (c - x)) ** 3
