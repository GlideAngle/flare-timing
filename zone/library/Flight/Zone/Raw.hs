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

type RawRadius = Integer

data RawZone =
    RawZone { zoneName :: String
            , lat :: RawLat
            , lng :: RawLng
            , radius :: RawRadius
            } deriving (Eq, Show, Generic)

instance ToJSON RawZone
instance FromJSON RawZone

showRadius :: RawRadius -> String
showRadius r
    | r < 1000 = show r ++ " m"
    | otherwise = let y = truncate (r % 1000) :: Integer in show y ++ " km"

showZone :: RawZone -> String
showZone (RawZone name lat' lng' rad) =
    unwords [ name
            , showLat lat'
            , showLng lng'
            , showRadius rad
            ]
