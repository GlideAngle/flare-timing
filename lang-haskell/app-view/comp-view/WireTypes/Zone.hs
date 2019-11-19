module WireTypes.Zone
    ( RawZone(..)
    , Zones(..)
    , RawLat(..)
    , RawLng(..)
    , RawLatLng(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..))
import WireTypes.ZoneKind (TaskZones(..), Radius(..), RawLat(..), RawLng(..), Alt(..))

data RawLatLng =
    RawLatLng
        { lat :: RawLat
        , lng :: RawLng
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass FromJSON

data RawZone =
    RawZone
        { zoneName :: String
        , lat :: RawLat
        , lng :: RawLng
        , radius :: Radius
        , giveIn :: Maybe Radius
        , giveOut :: Maybe Radius
        , alt :: Maybe Alt
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

data Zones =
    Zones
        { raw :: [RawZone]
        , raceKind :: Maybe TaskZones
        , openKind :: Maybe TaskZones
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)
