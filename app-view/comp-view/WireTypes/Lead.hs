module WireTypes.Lead
    ( LeadingFraction(..)
    , LeadingCoefficient(..)
    , TrackLead(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..))

newtype LeadingFraction = LeadingFraction Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype LeadingCoefficient = LeadingCoefficient Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

data TrackLead =
    TrackLead
        { coef :: LeadingCoefficient
        , frac :: LeadingFraction
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)
