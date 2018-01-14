{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE PatternSynonyms #-}

module Flight.Gap.Validity
    ( NominalTime(..)
    , Seconds
    , Metres
    , launchValidity
    , distanceValidity
    , timeValidity
    , taskValidity
    ) where

import Data.Ratio ((%))
import Data.UnitsOfMeasure (u, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Gap.Ratio (pattern (:%))
import Flight.Gap.Distance.Min (MinimumDistance(..))
import Flight.Gap.Distance.Max (MaximumDistance(..))
import Flight.Gap.Distance.Sum (SumOfDistance(..))
import Flight.Gap.Validity.Launch (LaunchValidity(..))
import Flight.Gap.Validity.Distance (DistanceValidity(..))
import Flight.Gap.Validity.Time (TimeValidity(..))
import Flight.Gap.Validity.Task (TaskValidity(..))
import Flight.Gap.Nominal.Launch (NominalLaunch(..))
import Flight.Gap.Nominal.Goal (NominalGoal(..))
import Flight.Gap.Nominal.Distance (NominalDistance(..))

newtype NominalDistanceArea = NominalDistanceArea Rational
    deriving (Eq, Show)

newtype NominalTime = NominalTime Integer
    deriving (Eq, Show)

type Seconds = Integer
type Metres = Integer

launchValidity :: NominalLaunch -> Rational -> LaunchValidity
launchValidity (NominalLaunch (_ :% _)) (0 :% _) =
    LaunchValidity (0 % 1)
launchValidity (NominalLaunch (0 :% _)) (_ :% _) =
    LaunchValidity (1 % 1)
launchValidity (NominalLaunch (n :% d)) (flying :% present) =
    LaunchValidity $
    (27 % 1000) * lvr
    + (2917 % 1000) * lvr * lvr
    - (1944 % 1000) * lvr * lvr * lvr
    where
        lvr' = (flying * d) % (present * n)
        lvr = min lvr' (1 % 1)

tvrValidity :: Rational -> TimeValidity
tvrValidity (0 :% _) =
    TimeValidity 0
tvrValidity tvr =
    TimeValidity $ max 0 $ min 1 x
    where
        x =
            (- 271 % 1000)
            + (2912 % 1000) * tvr
            - (2098 % 1000) * tvr * tvr
            + (457 % 1000) * tvr * tvr * tvr

timeValidity
    :: NominalTime
    -> NominalDistance (Quantity Double [u| km |])
    -> Maybe Seconds
    -> Metres
    -> TimeValidity
timeValidity (NominalTime 0) _ (Just 0) _ =
    tvrValidity (0 % 1)
timeValidity (NominalTime 0) _ (Just _) _ =
    tvrValidity (1 % 1)
timeValidity (NominalTime nt) _ (Just t) _ = 
    tvrValidity $ min (t % nt) (1 % 1)
timeValidity _ (NominalDistance (MkQuantity 0)) Nothing 0 =
    tvrValidity (0 % 1)
timeValidity _ (NominalDistance (MkQuantity 0)) Nothing _ =
    tvrValidity (1 % 1)
timeValidity _ (NominalDistance dNom') Nothing d =
    tvrValidity $ min dNom (1 % 1)
    where
        MkQuantity dNom = toRational' dNom'

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
    -> TimeValidity
    -> DistanceValidity
    -> TaskValidity
taskValidity (LaunchValidity l) (TimeValidity t) (DistanceValidity d) =
    TaskValidity $ l * t * d
