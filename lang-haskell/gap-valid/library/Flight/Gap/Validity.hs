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
    , StopValidity(..)
    , ReachToggle(..)
    , ReachStats(..)
    , StopValidityWorking(..)
    , LaunchToEss(..)
    , FlownMean(..)
    , FlownStdDev(..)
    , NominalDistanceArea(..)
    , launchValidity
    , distanceValidity
    , timeValidity
    , taskValidity
    , stopValidity
    ) where

import Prelude hiding (min, max)
import qualified Prelude as Stats (min, max)
import Data.Ratio ((%))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure
    ((+:), (-:), (*:), (/:), u, zero, convert, toRational', sqrt', unQuantity)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Ratio (pattern (:%))
import "flight-gap-allot" Flight.Score
    (FlownMax(..), FlownMean(..), FlownStdDev(..), LaunchToEss(..))
import "flight-gap-allot" Flight.Score (NominalDistance(..))
import "flight-gap-allot" Flight.Score (MinimumDistance(..))
import "flight-gap-allot" Flight.Score (SumOfDistance(..))
import "flight-gap-allot" Flight.Score (NominalLaunch(..))
import "flight-gap-allot" Flight.Score (NominalGoal(..))
import "flight-gap-allot" Flight.Score (NominalTime(..))
import "flight-gap-allot" Flight.Score (BestTime(..))
import "flight-gap-allot" Flight.Score
    (PilotsPresent(..), PilotsFlying(..), PilotsAtEss(..), PilotsLanded(..))
import Flight.Gap.Validity.Area (NominalDistanceArea(..))
import Flight.Gap.Validity.Launch (LaunchValidity(..))
import Flight.Gap.Validity.Distance (DistanceValidity(..))
import Flight.Gap.Validity.Time (TimeValidity(..))
import Flight.Gap.Validity.Task (TaskValidity(..))
import Flight.Gap.Validity.Stop (StopValidity(..))

data Validity =
    Validity
        { task :: TaskValidity
        , launch :: LaunchValidity
        , distance :: DistanceValidity
        , time :: TimeValidity
        , stop :: Maybe StopValidity
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data ValidityWorking =
    ValidityWorking
        { launch :: LaunchValidityWorking
        , distance :: DistanceValidityWorking
        , time :: TimeValidityWorking
        , stop :: Maybe StopValidityWorking
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
        , reachMax :: ReachToggle (FlownMax (Quantity Double [u| km |]))
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
        { ssBestTime :: Maybe (BestTime (Quantity Double [u| h |]))
        -- ^ For each task, the best time ignoring start gates.
        , gsBestTime :: Maybe (BestTime (Quantity Double [u| h |]))
        -- ^ For each task, the best time from the start gate taken.
        , nominalTime :: NominalTime (Quantity Double [u| h |])
        , nominalDistance :: NominalDistance (Quantity Double [u| km |])
        , reachMax :: ReachToggle (FlownMax (Quantity Double [u| km |]))
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data ReachToggle a =
    ReachToggle
        { extra :: a
        -- ^ The best bolstered reach with extra altitude above goal converted
        -- to extra reach via glide.
        , flown :: a
        -- ^ The maximuum bolstered reach.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data ReachStats =
    ReachStats
        { max :: FlownMax (Quantity Double [u| km |])
        , mean :: FlownMean (Quantity Double [u| km |])
        , stdDev :: FlownStdDev (Quantity Double [u| km |])
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data StopValidityWorking =
    StopValidityWorking
        { pilotsAtEss :: PilotsAtEss
        , landed :: PilotsLanded
        , stillFlying :: PilotsFlying
        , flying :: PilotsFlying
        , launchToEssDistance :: Maybe (LaunchToEss (Quantity Double [u| km |]))
        -- ^ The launch to ESS distance is parsed from an XML attribute that
        -- may not exist.
        , reachStats :: ReachToggle (Maybe ReachStats)
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
    (lvrPolynomial . Stats.min 1 $ f / (p * n), Just $ LaunchValidityWorking pf pp nl)
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
    tvrPolynomial . Stats.min 1 $ b / n
    where
        MkQuantity n = toRational' tNom
        MkQuantity b = toRational' tBest

tvrValidity
    DistanceRatio
        { nominalDistance = NominalDistance dNom
        , bestDistance = FlownMax dBest
        } =
    tvrPolynomial . Stats.min 1 $ b / n
    where
        MkQuantity n = toRational' dNom
        MkQuantity b = toRational' dBest

tvrPolynomial :: Rational -> TimeValidity
tvrPolynomial tvr =
    TimeValidity . Stats.max 0 . Stats.min 1
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
        , bestDistance :: FlownMax (Quantity Double [u| km |])
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
    -- ^ The best time from the start ignoring start gates
    -> Maybe (BestTime (Quantity Double [u| s |]))
    -- ^ The best time from the start gate taken
    -> NominalDistance (Quantity Double [u| km |])
    -> ReachToggle (FlownMax (Quantity Double [u| km |]))
    -> (TimeValidity, Maybe TimeValidityWorking)

timeValidity
    tNom
    ssBestTime
    Nothing
    dNom@(NominalDistance nd)
    reachMax@ReachToggle{extra = bdE@(FlownMax bd)}
    | nd <= [u| 0 km |] = tvZero
    | bd <= [u| 0 km |] = tvZero
    | otherwise =
        ( tvrValidity $ DistanceRatio dNom bdE
        , Just
        $ TimeValidityWorking
            (btHours <$> ssBestTime)
            Nothing
            (ntHours tNom)
            dNom
            reachMax
        )

timeValidity
    tNom@(NominalTime nt)
    ssBestTime
    (Just gsBestTime@(BestTime bt))
    dNom
    reachMax
    | nt <= [u| 0 s |] = tvZero
    | bt <= [u| 0 s |] = tvZero
    | otherwise =
        ( tvrValidity $ TimeRatio tNom gsBestTime
        , Just
        $ TimeValidityWorking
            (btHours <$> ssBestTime)
            (Just (btHours gsBestTime))
            (ntHours tNom)
            dNom
            reachMax
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
    -> ReachToggle (FlownMax (Quantity Double [u| km |]))
    -> SumOfDistance (Quantity Double [u| km |])
    -> (DistanceValidity, Maybe DistanceValidityWorking)
distanceValidity _ _ (PilotsFlying 0) _ _ _ =
    (DistanceValidity 0, Nothing)

distanceValidity _ _ _ _ ReachToggle{flown = FlownMax (MkQuantity 0)} _ =
    (DistanceValidity 0, Nothing)

distanceValidity
    ng@(NominalGoal (0 :% _))
    nd@(NominalDistance (MkQuantity 0))
    nFly
    md
    bd
    dSum =
    ( DistanceValidity $ Stats.min 1 $ dvr area nFly dSum
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
        ( DistanceValidity . Stats.min 1 $ dvr area nFly dSum
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
    bd@ReachToggle{flown = FlownMax dMax'}
    dSum
    | dNom < dMin =
        ( DistanceValidity (1 % 1)
        , Nothing
        )
    | otherwise =
        ( DistanceValidity . Stats.min 1 $ dvr area nFly dSum
        , Just $ DistanceValidityWorking dSum nFly area ng nd md bd
        )
    where
        MkQuantity dNom = toRational' dNom'
        MkQuantity dMin = toRational' dMin'
        MkQuantity dMax = toRational' dMax'

        a :: Rational
        a = (gNom + 1) * (dNom - dMin)

        b :: Rational
        b = Stats.max 0 $ gNom * (dMax - dNom)

        area :: NominalDistanceArea
        area = NominalDistanceArea $ (a + b) * (1 % 2)

stopValidity
    :: PilotsFlying
    -> PilotsAtEss
    -> PilotsLanded
    -> PilotsFlying
    -> ReachToggle (Maybe ReachStats)
    -> LaunchToEss (Quantity Double [u| km |])
    -> (StopValidity, Maybe StopValidityWorking)
stopValidity
    pf@(PilotsFlying flying)
    pe@(PilotsAtEss ess)
    landedByStop@(PilotsLanded landed)
    stillFlying
    ReachToggle{extra, flown}
    ed'@(LaunchToEss ed)
    | flying == 0 = (StopValidity 0, Nothing)
    -- REVIEW: Check what the formula in the GAP rules does when flown > ed.
    | Nothing <- flown = (StopValidity 0, Nothing)
    | Just ReachStats{max = FlownMax flownMax'} <- flown
    , flownMax' > ed = (StopValidity 0, Nothing)
    | Just flown' <- flown =
        let ReachStats
                { max = FlownMax flownMax'
                , mean = FlownMean flownMean
                , stdDev = FlownStdDev flownStdDev
                } = flown'

            flownMax :: Quantity Double [u| km |]
            flownMax = convert flownMax'
            denom = ed -: flownMax +: [u| 1 km |]

            a :: Quantity Double [u| 1 |]
            a =
                sqrt' $
                    ((flownMax -: flownMean) /: denom)
                    *: sqrt' (flownStdDev /: [u| 5 km |])

            b = fromIntegral landed / (fromIntegral flying :: Double)

            w = StopValidityWorking
                    { pilotsAtEss = pe
                    , landed = landedByStop
                    , stillFlying = stillFlying
                    , flying = pf
                    , reachStats =
                        ReachToggle
                            { extra = extra
                            , flown = flown
                            }
                    , launchToEssDistance = Just ed'
                    }

        in
            if | denom == zero -> (StopValidity 0, Nothing)
               | ess > 0 -> (StopValidity 1, Just w)
               | otherwise -> (StopValidity $ Stats.min 1 (toRational $ unQuantity a + b**3), Just w)

taskValidity
    :: LaunchValidity
    -> DistanceValidity
    -> TimeValidity
    -> Maybe StopValidity
    -> TaskValidity
taskValidity (LaunchValidity l) (DistanceValidity d) (TimeValidity t) sv =
    TaskValidity $ maybe (l * t * d) (\(StopValidity s) -> l * t * d * s) sv