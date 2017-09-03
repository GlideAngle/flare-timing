{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Data.Flight.Zone
    ( Zone(..)
    , Radius
    , showRadius
    , showZone
    ) where

import Data.Ratio((%))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Data.Flight.LatLng (Latitude, Longitude, showLat, showLng)

type Radius = Integer

data Zone =
    Zone { zoneName :: String
         , lat :: Latitude
         , lng :: Longitude
         , radius :: Radius
         } deriving (Eq, Show, Generic)

instance ToJSON Zone
instance FromJSON Zone

showRadius :: Radius -> String
showRadius r
    | r < 1000 = show r ++ " m"
    | otherwise = let y = truncate (r % 1000) :: Integer in show y ++ " km"

showZone :: Zone -> String
showZone (Zone name lat' lng' rad) =
    unwords [ name
            , showLat lat'
            , showLng lng'
            , showRadius rad
            ]
