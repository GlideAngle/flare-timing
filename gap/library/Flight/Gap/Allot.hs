{-# LANGUAGE DuplicateRecordFields #-}
module Flight.Gap.Allot
    ( PositionAtEss(..)
    , ArrivalFraction(..)
    , arrivalFraction
    , PilotTime(..)
    , bestTime'
    , SpeedFraction(..)
    , speedFraction
    ) where

import Data.Ratio ((%))
import qualified Data.List as List (minimum)
import Data.Aeson (ToJSON(..), FromJSON(..))
import GHC.Generics (Generic)
import Data.UnitsOfMeasure (u, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Gap.Pilots (PilotsAtEss(..))
import Flight.Gap.Time.Best (BestTime(..))
import Flight.Gap.Time.Pilot (PilotTime(..))
import Flight.Gap.Ratio.Arrival (ArrivalFraction(..))
import Flight.Gap.Ratio.Speed (SpeedFraction(..))

-- | A 1-based rank of the pilot arrival at goal, 1st in is 1, 2nd is 2 etc.
newtype PositionAtEss = PositionAtEss Integer
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

arrivalFraction :: PilotsAtEss -> PositionAtEss -> ArrivalFraction
arrivalFraction (PilotsAtEss n) (PositionAtEss rank)
    | n <= 0 =
        ArrivalFraction 0
    | rank <= 0 =
        ArrivalFraction 0
    | rank > n =
        ArrivalFraction 0
    | otherwise =
        ArrivalFraction $
        (2 % 10)
        + (37 % 1000) * ac
        + (13 % 100) * ac * ac
        + (633 % 1000) * ac * ac * ac
        where
            ac = 1 - ((rank - 1) % n)

bestTime'
    :: [PilotTime (Quantity Double [u| h |])]
    -> Maybe (BestTime (Quantity Double [u| h |]))
bestTime' [] = Nothing
bestTime' xs =
    Just . BestTime $ t
    where
        PilotTime t = List.minimum xs

speedFraction
    :: BestTime (Quantity Double [u| h |])
    -> PilotTime (Quantity Double [u| h |])
    -> SpeedFraction
speedFraction (BestTime best') (PilotTime t') =
    SpeedFraction $ max (0 % 1) sf
    where
        MkQuantity best = toRational' best'
        MkQuantity t = toRational' t'
        numerator = fromRational $ t - best :: Double
        denominator = fromRational best ** (1 / 2)
        frac = (numerator / denominator) ** (2 / 3)
        sf = (1 % 1) - toRational frac
