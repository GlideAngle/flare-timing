{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}

module Flight.Gap.Validity
    ( NominalTime(..)
    , launchValidity
    , distanceValidity
    , timeValidity
    , taskValidity
    ) where

import Data.Ratio ((%))
import Data.UnitsOfMeasure (u, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Gap.Ratio (pattern (:%))
import Flight.Gap.Distance.Nominal (NominalDistance(..))
import Flight.Gap.Distance.Best (BestDistance(..))
import Flight.Gap.Distance.Min (MinimumDistance(..))
import Flight.Gap.Distance.Max (MaximumDistance(..))
import Flight.Gap.Distance.Sum (SumOfDistance(..))
import Flight.Gap.Validity.Launch (LaunchValidity(..))
import Flight.Gap.Validity.Distance (DistanceValidity(..))
import Flight.Gap.Validity.Time (TimeValidity(..))
import Flight.Gap.Validity.Task (TaskValidity(..))
import Flight.Gap.Ratio.Launch (NominalLaunch(..))
import Flight.Gap.Ratio.Goal (NominalGoal(..))
import Flight.Gap.Time.Nominal (NominalTime(..))
import Flight.Gap.Time.Best (BestTime(..))
import Flight.Gap.Pilots (PilotsPresent(..), PilotsFlying(..))

newtype NominalDistanceArea = NominalDistanceArea Rational
    deriving (Eq, Show)

launchValidity
    :: NominalLaunch
    -> PilotsPresent
    -> PilotsFlying
    -> LaunchValidity

launchValidity (NominalLaunch (_ :% _)) _ (PilotsFlying 0) =
    LaunchValidity (0 % 1)

launchValidity (NominalLaunch (0 :% _)) _ _ =
    LaunchValidity (1 % 1)

launchValidity
    (NominalLaunch (n :% d)) (PilotsPresent present) (PilotsFlying flying) =
    LaunchValidity $
    (27 % 1000) * lvr
    + (2917 % 1000) * lvr * lvr
    - (1944 % 1000) * lvr * lvr * lvr
    where
        lvr' = (toInteger flying * d) % (toInteger present * n)
        lvr = min lvr' (1 % 1)

tvrValidity :: TimeValidityRatio -> TimeValidity

tvrValidity
    TimeRatio
        { nominalTime = NominalTime tNom
        , bestTime = BestTime tBest
        } =
        tvrPolynomial (min 1 $ nom * bInv)
    where
        MkQuantity nom = toRational' tNom
        MkQuantity (nb :% db) = toRational' tBest
        bInv = db % nb

tvrValidity
    DistanceRatio
        { nominalDistance = NominalDistance dNom
        , bestDistance = BestDistance dBest
        } =
        tvrPolynomial (min 1 $ nom * bInv)
    where
        MkQuantity nom = toRational' dNom
        MkQuantity (nb :% db) = toRational' dBest
        bInv = db % nb

tvrPolynomial :: Rational -> TimeValidity
tvrPolynomial tvr =
    TimeValidity . max 0 . min 1
    $ (- 271 % 1000)
    + (2912 % 1000) * tvr
    - (2098 % 1000) * tvr * tvr
    + (457 % 1000) * tvr * tvr * tvr

-- | Time validity uses the ratio of times or distances depending on whether
-- any pilots make it to the end of the speed section.
data TimeValidityRatio
    = TimeRatio
        { nominalTime :: NominalTime (Quantity Double [u| s |])
        , bestTime :: BestTime (Quantity Double [u| s |])
        }
    | DistanceRatio
        { nominalDistance :: NominalDistance (Quantity Double [u| km |])
        , bestDistance :: BestDistance (Quantity Double [u| km |])
        }

-- | If a best time is given then at least one pilot has finished the speed
-- section.
timeValidity
    :: NominalTime (Quantity Double [u| s |])
    -> Maybe (BestTime (Quantity Double [u| s |]))
    -> NominalDistance (Quantity Double [u| km |])
    -> BestDistance (Quantity Double [u| km |])
    -> TimeValidity

timeValidity _ Nothing dNom@(NominalDistance nd) dBest@(BestDistance bd)
    | nd <= [u| 0 km |] = TimeValidity $ 0 % 1
    | bd <= [u| 0 km |] = TimeValidity $ 0 % 1
    | otherwise = tvrValidity $ DistanceRatio dNom dBest

timeValidity tNom@(NominalTime nt) (Just tBest@(BestTime bt)) _ _
    | nt <= [u| 0 s |] = TimeValidity $ 0 % 1
    | bt <= [u| 0 s |] = TimeValidity $ 0 % 1
    | otherwise = tvrValidity $ TimeRatio tNom tBest

dvr
    :: NominalDistanceArea
    -> Integer
    -> SumOfDistance (Quantity Double [u| km |])
    -> Rational
dvr (NominalDistanceArea dNom@(n :% d)) nFly (SumOfDistance dSum)
    | dNom <= 0 = 0 % 1
    | nFly <= 0 = 0 % 1
    | dSum <= [u| 0 km |] = 0 % 1
    | otherwise = dSum' * (d % (nFly * n))
    where
        MkQuantity dSum' = toRational' dSum

distanceValidity
    :: NominalGoal
    -> NominalDistance (Quantity Double [u| km |])
    -> Integer
    -> MinimumDistance (Quantity Double [u| km |])
    -> MaximumDistance (Quantity Double [u| km |])
    -> SumOfDistance (Quantity Double [u| km |])
    -> DistanceValidity
distanceValidity _ _ 0 _ _ _ =
    DistanceValidity 0

distanceValidity _ _ _ _ (MaximumDistance (MkQuantity 0)) _ =
    DistanceValidity 0

distanceValidity
    (NominalGoal (0 :% _)) (NominalDistance (MkQuantity 0)) nFly _ _ dSum =
    DistanceValidity $ min 1 $ dvr (NominalDistanceArea 0) nFly dSum

distanceValidity
    (NominalGoal (0 :% _))
    (NominalDistance dNom')
    nFly
    (MinimumDistance dMin')
    _
    dSum
    | dNom < dMin =
        DistanceValidity (1 % 1)
    | otherwise =
        DistanceValidity . min 1 $ dvr area nFly dSum
    where
        MkQuantity dNom = toRational' dNom'
        MkQuantity dMin = toRational' dMin'

        a :: Rational
        a = dNom - dMin

        area :: NominalDistanceArea
        area = NominalDistanceArea $ a * (1 % 2)

distanceValidity
    (NominalGoal gNom)
    (NominalDistance dNom')
    nFly
    (MinimumDistance dMin')
    (MaximumDistance dMax')
    dSum
    | dNom < dMin =
        DistanceValidity (1 % 1)
    | otherwise =
        DistanceValidity . min 1 $ dvr area nFly dSum
    where
        MkQuantity dNom = toRational' dNom'
        MkQuantity dMin = toRational' dMin'
        MkQuantity dMax = toRational' dMax'

        a :: Rational
        a = (gNom + 1) * (dNom - dMin)

        b :: Rational
        b = max 0 $ gNom * (dMax - dNom)

        area :: NominalDistanceArea
        area = NominalDistanceArea $ (a + b) * (1 % 2)

taskValidity
    :: LaunchValidity
    -> DistanceValidity
    -> TimeValidity
    -> TaskValidity
taskValidity (LaunchValidity l) (DistanceValidity d) (TimeValidity t) =
    TaskValidity $ l * t * d
