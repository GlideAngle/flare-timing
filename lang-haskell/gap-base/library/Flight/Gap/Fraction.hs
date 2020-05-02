module Flight.Gap.Fraction (Fractions(..)) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Gap.Fraction.Linear (LinearFraction(..))
import Flight.Gap.Fraction.Difficulty (DifficultyFraction(..))
import Flight.Gap.Fraction.Distance (DistanceFraction(..))
import Flight.Gap.Fraction.Leading (LeadingFraction(..))
import Flight.Gap.Fraction.Arrival (ArrivalFraction(..))
import Flight.Gap.Fraction.Speed (SpeedFraction(..))

-- | These fractions are a pilot's fraction of the available points in each
-- category.
data Fractions =
    Fractions
        { reach :: LinearFraction
        , effort :: DifficultyFraction
        , distance :: DistanceFraction
        , leading :: LeadingFraction
        , arrival :: ArrivalFraction
        , time :: SpeedFraction
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)
