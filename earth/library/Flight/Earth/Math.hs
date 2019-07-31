module Flight.Earth.Math
    ( mod'
    , atan2'
    , cos2
    , normalizeLng
    , normalizeLngR
    ) where

import Data.Ratio ((%))
import qualified Data.Number.FixedFunctions as F
import qualified Data.Fixed as F (mod')

import Flight.Ratio (pattern (:%))
import Flight.LatLng.Rational (Epsilon(..))

mod' :: Rational -> Rational -> Rational
mod' (a :% b) (c :% d) =
    ((d * a) `mod` (b * c)) % (b * d)

-- | In Vincenty's paper he says,
--
-- "The inverse formula may give no solution over a line between
-- two nearly antipodal points. This will occur when λ, as computed
-- by eqn. (11), is greater than π in absolute value."
--
-- (45°,-179°59'58.17367'') to (45°,180°)
-- Comparing the above two points, longitudes are less than a minute apart.  To
-- be able to get the difference using simple subtraction normalize the
-- longitudes to a range 0 <= lng <= 2π.
normalizeLng :: (Floating a, Real a) => a -> a
normalizeLng lng =
    lng `F.mod'` (2 * pi)

normalizeLngR :: Epsilon -> Rational -> Rational
normalizeLngR (Epsilon eps) lng =
   lng `mod'` (2 * F.pi eps)

-- | The numbers package doesn't have atan2.
-- SEE: https://hackage.haskell.org/package/base
-- SEE: https://stackoverflow.com/questions/283406/what-is-the-difference-between-atan-and-atan2-in-c
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

cos2 :: (Num a, Num p) => (p -> a) -> p -> p -> (a, a)
cos2 cos' σ1 σ = (cos2σm, cos²2σm)
    where
        _2σm = 2 * σ1 + σ
        cos2σm = cos' _2σm
        cos²2σm = cos2σm * cos2σm
