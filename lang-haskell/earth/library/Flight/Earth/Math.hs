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
--
-- >>> atan2 0 1
-- 0.0
--
-- >>> atan2 1 0
-- 1.5707963267948966
--
-- >>> atan2 0 (negate 1)
-- 3.141592653589793
--
-- >>> atan2 (negate 1) 0
-- -1.5707963267948966
--
-- >>> atan2 4 3
-- 0.9272952180016122
--
-- >>> atan2 (negate 4) 3
-- -0.9272952180016122
--
-- >>> atan2 4 (negate 3)
-- 2.214297435588181
--
-- >>> atan2 (negate 4) (negate 3)
-- -2.214297435588181
--
-- >>> atan2 4 3 == atan (4 / 3)
-- True
--
-- >>> atan2 (negate 4) 3 == - atan (4 / 3)
-- True
--
-- >>> atan2'' e3 0 1
-- 0.0
--
-- >>> atan2'' e21 0 1
-- 0.0
--
-- >>> atan2'' e12 1 0
-- 1.570796326794868
--
-- >>> atan2'' e15 1 0
-- 1.5707963267948966
--
-- >>> atan2'' e18 1 0
-- 1.5707963267948966
--
-- >>> atan2'' e21 1 0
-- 1.5707963267948966
--
-- >>> atan2'' e15 0 (negate 1)
-- 3.141592653589793
--
-- >>> atan2'' e18 0 (negate 1)
-- 3.141592653589793
--
-- >>> atan2'' e21 0 (negate 1)
-- 3.141592653589793
--
-- >>> atan2'' e15 (negate 1) 0
-- -1.5707963267948966
--
-- >>> atan2'' e18 (negate 1) 0
-- -1.5707963267948966
--
-- >>> atan2'' e21 (negate 1) 0
-- -1.5707963267948966
--
-- >>> atan2'' e15 4 3
-- 0.9272952180016129
--
-- >>> atan2'' e18 4 3
-- 0.9272952180016122
--
-- >>> atan2'' e21 4 3
-- 0.9272952180016122
--
-- >>> atan2'' e15 (negate 4) 3
-- -0.9272952180016129
--
-- >>> atan2'' e18 (negate 4) 3
-- -0.9272952180016122
--
-- >>> atan2'' e21 (negate 4) 3
-- -0.9272952180016122
--
-- >>> atan2'' e15 4 (negate 3)
-- 2.2142974355881804
--
-- >>> atan2'' e18 4 (negate 3)
-- 2.214297435588181
--
-- >>> atan2'' e21 4 (negate 3)
-- 2.214297435588181
--
-- >>> atan2'' e15 (negate 4) (negate 3)
-- -2.2142974355881804
--
-- >>> atan2'' e18 (negate 4) (negate 3)
-- -2.214297435588181
--
-- >>> atan2'' e21 (negate 4) (negate 3)
-- -2.214297435588181
--
-- >>> atan2'' e15 4 3 == atan (4 / 3)
-- False
--
-- >>> atan2'' e18 4 3 == atan (4 / 3)
-- True
--
-- >>> atan2'' e21 4 3 == atan (4 / 3)
-- True
--
-- >>> atan2'' e15 (negate 4) 3 == - atan (4 / 3)
-- False
--
-- >>> atan2'' e18 (negate 4) 3 == - atan (4 / 3)
-- True
--
-- >>> atan2'' e21 (negate 4) 3 == - atan (4 / 3)
-- True
--
-- >>> atan2'' e21 55 26
-- 1.1292039644876055
--
-- >>> atan (55 / 26)
-- 1.1292039644876053
--
-- prop> \(y :: Double) (x :: Double) -> y > 0 && x > 0 ==> let a = atan2'' e21 (toRational y) (toRational x) in let b = atan (y / x) in abs (a - b) <= delta
--
-- prop> \(y :: Double) (x :: Double) -> y > 0 && x > 0 ==> let a = atan2'' e21 (negate $ toRational y) (toRational x) in let b = - atan (y / x) in abs (a - b) <= delta
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

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Flight.LatLng.Rational (Epsilon(..), defEps)
--
-- >>> delta = 0.0000000000000003
--
-- >>> e21 = Epsilon $ 1 % 1000000000000000000000
-- >>> e18 = Epsilon $ 1 % 1000000000000000000
-- >>> e15 = Epsilon $ 1 % 1000000000000000
-- >>> e12 = Epsilon $ 1 % 1000000000000
-- >>>  e9 = Epsilon $ 1 % 1000000000
-- >>>  e6 = Epsilon $ 1 % 1000000
-- >>>  e3 = Epsilon $ 1 % 1000
--
-- >>> atan2'' eps y x = fromRational $ atan2' eps y x
