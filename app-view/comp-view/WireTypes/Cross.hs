module WireTypes.Cross
    ( FlyingSection
    , TrackFlyingSection(..)
    , Fix(..)
    ) where

import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..))
import WireTypes.Zone (RawLat(..), RawLng(..))

type FlyingSection a = Maybe (a, a)

data TrackFlyingSection =
    TrackFlyingSection
        { loggedFixes :: Maybe Int
        , flyingFixes :: FlyingSection Int
        , scoredFixes :: FlyingSection Int
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

data Fix =
    Fix { fix :: Int
        , time :: UTCTime
        , lat :: RawLat
        , lng :: RawLng
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)
