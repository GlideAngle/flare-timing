{-# lANGUAGE PatternSynonyms #-}
{-# lANGUAGE ScopedTypeVariables #-}
module Flight.Validity
    ( NominalLaunch(..)
    , NominalTime
    , NominalDistance
    , NominalGoal
    , LaunchValidity
    , TimeValidity
    , Seconds
    , Metres
    , launchValidity
    , distanceValidity
    , timeValidity
    , taskValidity
    ) where

import Data.Ratio ((%))
import Flight.Ratio (pattern (:%))

newtype NominalLaunch = NominalLaunch Rational deriving (Eq, Show)
type MinimumDistance = Metres
type MaximumDistance = Metres
type SumOfDistance = Metres
type NominalDistance = Integer
type NominalTime = Integer
type NominalGoal = Rational

type LaunchValidity = Rational
type TimeValidity = Rational
type DistanceValidity = Rational

-- | Also called Day Quality.
type TaskValidity = Rational

type Seconds = Integer
type Metres = Integer

launchValidity :: NominalLaunch -> Rational -> LaunchValidity
launchValidity (NominalLaunch (_ :% _)) (0 :% _) = 0 % 1
launchValidity (NominalLaunch (0 :% _)) (_ :% _) = 1 % 1
launchValidity (NominalLaunch (n :% d)) (flying :% present) =
    (27 % 1000) * lvr
    + (2917 % 1000) * lvr * lvr
    - (1944 % 1000) * lvr * lvr * lvr
    where
        lvr' = (flying * d) % (present * n)
        lvr = min lvr' (1 % 1)

tvrValidity :: Rational -> TimeValidity
tvrValidity (0 :% _) = 0
tvrValidity tvr =
    max 0 $ min 1 x
    where
        x =
            (- 271 % 1000)
            + (2912 % 1000) * tvr
            - (2098 % 1000) * tvr * tvr
            + (457 % 1000) * tvr * tvr * tvr

timeValidity :: NominalTime -> NominalDistance -> Maybe Seconds -> Metres -> TimeValidity
timeValidity 0 _ (Just 0) _ = tvrValidity (0 % 1)
timeValidity 0 _ (Just _) _ = tvrValidity (1 % 1)
timeValidity nt _ (Just t) _ = tvrValidity $ min (t % nt) (1 % 1)
timeValidity _ 0 Nothing 0 = tvrValidity (0 % 1)
timeValidity _ 0 Nothing _ = tvrValidity (1 % 1)
timeValidity _ nd Nothing d = tvrValidity $ min (d % nd) (1 % 1)

dvr :: Rational -> Integer -> Metres -> Rational
dvr (0 :% _) _ _ = 1 % 1
dvr (n :% d) nFly dSum = (dSum % 1) * (d % (nFly * n))

distanceValidity :: NominalGoal
                 -> NominalDistance
                 -> Integer
                 -> MinimumDistance
                 -> MaximumDistance
                 -> SumOfDistance
                 -> DistanceValidity
distanceValidity _ _ 0 _ _ _ = 0
distanceValidity _ _ _ _ 0 _ = 0
distanceValidity (0 :% _) 0 nFly _ _ dSum =
    min 1 $ dvr (0 % 1) nFly dSum
distanceValidity (0 :% _) nd nFly dMin _ dSum
    | nd < dMin = 1 % 1
    | otherwise =
    min 1 $ dvr area nFly dSum
    where
        area = num % (2 * den)
        (num :% den) = min 0 (nd - dMin) % 1
distanceValidity ng nd nFly dMin dMax dSum
    | nd < dMin = 1 % 1
    | otherwise =
    min 1 $ dvr area nFly dSum
    where
        area = num % (2 * den)
        (num :% den) =
            (ng + (1 % 1) * ((nd - dMin) % 1)) + max 0 (ng * ((dMax - nd) % 1))

taskValidity :: LaunchValidity -> TimeValidity -> DistanceValidity -> TaskValidity
taskValidity l t d = l * t * d

