{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-- WARNING: This extension needs to be enabled at the definition site of a set
-- of record fields in order for them to be re-exported by a single module.
-- SEE: https://ghc.haskell.org/trac/ghc/ticket/13352
{-# LANGUAGE DuplicateRecordFields #-}

module Flight.Gap.Validity
    ( NominalTime(..)
    , Validity(..)
    , ValidityWorking(..)
    , LaunchValidityWorking(..)
    , DistanceValidityWorking(..)
    , TimeValidityWorking(..)
    , launchValidity
    , distanceValidity
    , timeValidity
    , taskValidity
    ) where

import Data.Ratio ((%))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u, convert, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Gap.Ratio (pattern (:%))
import Flight.Gap.Distance.Nominal (NominalDistance(..))
import Flight.Gap.Distance.Best (BestDistance(..))
import Flight.Gap.Distance.Min (MinimumDistance(..))
import Flight.Gap.Distance.Max (MaximumDistance(..))
import Flight.Gap.Distance.Sum (SumOfDistance(..))
import Flight.Gap.Validity.Area (NominalDistanceArea(..))
import Flight.Gap.Validity.Launch (LaunchValidity(..))
import Flight.Gap.Validity.Distance (DistanceValidity(..))
import Flight.Gap.Validity.Time (TimeValidity(..))
import Flight.Gap.Validity.Task (TaskValidity(..))
import Flight.Gap.Ratio.Launch (NominalLaunch(..))
import Flight.Gap.Ratio.Goal (NominalGoal(..))
import Flight.Gap.Time.Nominal (NominalTime(..))
import Flight.Gap.Time.Best (BestTime(..))
import Flight.Gap.Pilots (PilotsPresent(..), PilotsFlying(..))

data Validity =
    Validity 
        { task :: TaskValidity
        , launch :: LaunchValidity
        , distance :: DistanceValidity
        , time :: TimeValidity
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data ValidityWorking =
    ValidityWorking 
        { launch :: LaunchValidityWorking
        , distance :: DistanceValidityWorking
        , time :: TimeValidityWorking
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data DistanceValidityWorking =
    DistanceValidityWorking 
        { sum :: SumOfDistance (Quantity Double [u| km |])
        , flying :: PilotsFlying
        , area :: NominalDistanceArea
        , nominalGoal :: NominalGoal
        , nominalDistance :: NominalDistance (Quantity Double [u| km |])
        , minimumDistance :: MinimumDistance (Quantity Double [u| km |])
        , bestDistance :: MaximumDistance (Quantity Double [u| km |])
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data LaunchValidityWorking =
    LaunchValidityWorking 
        { flying :: PilotsFlying
        , present :: PilotsPresent
        , nominalLaunch :: NominalLaunch
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data TimeValidityWorking =
    TimeValidityWorking 
        { bestTime :: Maybe (BestTime (Quantity Double [u| h |]))
        , bestDistance :: BestDistance (Quantity Double [u| km |])
        , nominalTime :: NominalTime (Quantity Double [u| h |])
        , nominalDistance :: NominalDistance (Quantity Double [u| km |])
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

launchValidity
    :: NominalLaunch
    -> PilotsPresent
    -> PilotsFlying
    -> (LaunchValidity, Maybe LaunchValidityWorking)

launchValidity (NominalLaunch (_ :% _)) _ (PilotsFlying 0) =
    (LaunchValidity (0 % 1), Nothing)

launchValidity (NominalLaunch (0 :% _)) _ _ =
    (LaunchValidity (1 % 1), Nothing)

launchValidity
    nl@(NominalLaunch nominal)
    pp@(PilotsPresent present)
    pf@(PilotsFlying flying) =
    (lvrPolynomial . min 1 $ f / (p * n), Just $ LaunchValidityWorking pf pp nl)
    where
        n = toRational nominal
        p = toRational present
        f = toRational flying

lvrPolynomial :: Rational -> LaunchValidity
lvrPolynomial lvr =
    LaunchValidity $
    (27 % 1000) * lvr
    + (2917 % 1000) * lvr * lvr
    - (1944 % 1000) * lvr * lvr * lvr

tvrValidity :: TimeValidityRatio -> TimeValidity

tvrValidity
    TimeRatio
        { nominalTime = NominalTime tNom
        , bestTime = BestTime tBest
        } =
    tvrPolynomial . min 1 $ b / n
    where
        MkQuantity n = toRational' tNom
        MkQuantity b = toRational' tBest

tvrValidity
    DistanceRatio
        { nominalDistance = NominalDistance dNom
        , bestDistance = BestDistance dBest
        } =
    tvrPolynomial . min 1 $ b / n
    where
        MkQuantity n = toRational' dNom
        MkQuantity b = toRational' dBest

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
    deriving Show

tvZero :: (TimeValidity, Maybe a)
tvZero = (TimeValidity 0, Nothing)

ntHours
    :: NominalTime (Quantity Double [u| s |])
    -> NominalTime (Quantity Double [u| h |])
ntHours (NominalTime t) =
    NominalTime (convert t :: Quantity Double [u| h |])

btHours
    :: BestTime (Quantity Double [u| s |])
    -> BestTime (Quantity Double [u| h |])
btHours (BestTime t) =
    BestTime (convert t :: Quantity Double [u| h |])

-- | If a best time is given then at least one pilot has finished the speed
-- section.
timeValidity
    :: NominalTime (Quantity Double [u| s |])
    -> Maybe (BestTime (Quantity Double [u| s |]))
    -> NominalDistance (Quantity Double [u| km |])
    -> BestDistance (Quantity Double [u| km |])
    -> (TimeValidity, Maybe TimeValidityWorking)

timeValidity tNom Nothing dNom@(NominalDistance nd) dBest@(BestDistance bd)
    | nd <= [u| 0 km |] = tvZero
    | bd <= [u| 0 km |] = tvZero
    | otherwise =
        ( tvrValidity $ DistanceRatio dNom dBest
        , Just $ TimeValidityWorking Nothing dBest (ntHours tNom) dNom
        )

timeValidity tNom@(NominalTime nt) (Just tBest@(BestTime bt)) dNom dBest
    | nt <= [u| 0 s |] = tvZero
    | bt <= [u| 0 s |] = tvZero
    | otherwise =
        ( tvrValidity $ TimeRatio tNom tBest
        , Just $ TimeValidityWorking (Just (btHours tBest)) dBest (ntHours tNom) dNom
        )

dvr
    :: NominalDistanceArea
    -> PilotsFlying
    -> SumOfDistance (Quantity Double [u| km |])
    -> Rational
dvr
    (NominalDistanceArea nda)
    (PilotsFlying flying)
    (SumOfDistance dSum)
    | nda <= 0 = 0
    | flying <= 0 = 0
    | dSum <= [u| 0 km |] = 0
    | otherwise = d / (f * nda)
    where
        f = toRational flying
        MkQuantity d = toRational' dSum

distanceValidity
    :: NominalGoal
    -> NominalDistance (Quantity Double [u| km |])
    -> PilotsFlying
    -> MinimumDistance (Quantity Double [u| km |])
    -> MaximumDistance (Quantity Double [u| km |])
    -> SumOfDistance (Quantity Double [u| km |])
    -> (DistanceValidity, Maybe DistanceValidityWorking)
distanceValidity _ _ (PilotsFlying 0) _ _ _ =
    (DistanceValidity 0, Nothing)

distanceValidity _ _ _ _ (MaximumDistance (MkQuantity 0)) _ =
    (DistanceValidity 0, Nothing)

distanceValidity
    ng@(NominalGoal (0 :% _))
    nd@(NominalDistance (MkQuantity 0))
    nFly md bd dSum =
    ( DistanceValidity $ min 1 $ dvr area nFly dSum
    , Just $ DistanceValidityWorking dSum nFly area ng nd md bd
    )
    where
        area = NominalDistanceArea 0

distanceValidity
    ng@(NominalGoal (0 :% _))
    nd@(NominalDistance dNom')
    nFly
    md@(MinimumDistance dMin')
    bd 
    dSum
    | dNom < dMin =
        ( DistanceValidity (1 % 1)
        , Nothing
        )
    | otherwise =
        ( DistanceValidity . min 1 $ dvr area nFly dSum
        , Just $ DistanceValidityWorking dSum nFly area ng nd md bd
        )
    where
        MkQuantity dNom = toRational' dNom'
        MkQuantity dMin = toRational' dMin'

        a :: Rational
        a = dNom - dMin

        area :: NominalDistanceArea
        area = NominalDistanceArea $ a * (1 % 2)

distanceValidity
    ng@(NominalGoal gNom)
    nd@(NominalDistance dNom')
    nFly
    md@(MinimumDistance dMin')
    bd@(MaximumDistance dMax')
    dSum
    | dNom < dMin =
        ( DistanceValidity (1 % 1)
        , Nothing
        )
    | otherwise =
        ( DistanceValidity . min 1 $ dvr area nFly dSum
        , Just $ DistanceValidityWorking dSum nFly area ng nd md bd
        ) 
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
