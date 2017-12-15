{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Flight.Zone.Raw
    ( RawZone(..)
    , RawRadius
    , showRadius
    , showZone
    ) where

import Data.Ratio((%))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.LatLng.Raw (RawLat, RawLng, showLat, showLng)
import Data.Aeson.ViaScientific (ViaScientific(..))

type RawRadius = Integer

data RawZone =
    RawZone { zoneName :: String
            , lat :: ViaScientific RawLat
            , lng :: ViaScientific RawLng
            , radius :: RawRadius
            -- ^ Radius in metres.
            }
            deriving (Eq, Ord, Show, Generic)

instance ToJSON RawZone
instance FromJSON RawZone

showRadius :: RawRadius -> String
showRadius r
    | r < 1000 = show r ++ " m"
    | otherwise = let y = truncate (r % 1000) :: Integer in show y ++ " km"

showZone :: RawZone -> String
showZone (RawZone name (ViaScientific lat') (ViaScientific lng') rad) =
    unwords [ name
            , showLat lat'
            , showLng lng'
            , showRadius rad
            ]
