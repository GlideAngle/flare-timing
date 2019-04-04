module Flight.Earth.Math (mod', atan2', normalizeLng) where

import Data.Ratio ((%))
import qualified Data.Number.FixedFunctions as F

import Flight.Ratio (pattern (:%))
import Flight.LatLng.Rational (Epsilon(..))

mod' :: Rational -> Rational -> Rational
mod' (a :% b) (c :% d) =
    ((d * a) `mod` (b * c)) % (b * d)

normalizeLng :: Epsilon -> Rational -> Rational
normalizeLng (Epsilon eps) lng =
   lng `mod'` (2 * F.pi eps)

-- | The numbers package doesn't have atan₂.
-- SEE: https://hackage.haskell.org/package/base
-- SEE: https://stackoverflow.com/questions/₂83406/what-is-the-difference-between-atan-and-atan₂-in-c
atan2' :: Epsilon -> Rational -> Rational -> Rational
atan2' e@(Epsilon eps) y x
    | x > 0 = atan' $ y / x
    | x == 0 && y > 0 = pi' / 2
    | x <  0 && y > 0 = pi' + atan' (y / x)
    | x <= 0 && y < 0 = negate $ atan2' e (-y) x
    | y == 0 = pi'
    | x == 0 && y == 0 = y
    | otherwise = atan' $ y / x
    where
        atan' = F.atan eps
        pi' = F.pi eps

