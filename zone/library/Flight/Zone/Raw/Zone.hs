module Flight.Zone.Raw.Zone
    ( RawZone(..)
    , showZone
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)

import Flight.LatLng (QAlt)
import Flight.LatLng.Raw (RawLat, RawLng, showLat, showLng)
import Flight.Zone.Radius (QRadius)

data RawZone =
    RawZone
        { zoneName :: String
        , lat :: RawLat
        , lng :: RawLng
        , radius :: QRadius Double [u| m |]
        , alt :: Maybe (QAlt Double [u| m |])
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

showZone :: RawZone -> String
showZone (RawZone name lat' lng' rad alt') =
    unwords [ name
            , showLat lat'
            , showLng lng'
            , show rad
            , maybe "" show alt'
            ]
