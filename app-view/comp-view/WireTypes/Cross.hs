module WireTypes.Cross (FlyingSection, TrackFlyingSection(..)) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..))

type FlyingSection a = Maybe (a, a)

data TrackFlyingSection =
    TrackFlyingSection
        { loggedFixes :: Maybe Int
        , flyingFixes :: FlyingSection Int
        , scoredFixes :: FlyingSection Int
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)
