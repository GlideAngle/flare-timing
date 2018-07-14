module Flight.Zone.Raw.Zone
    ( RawZone(..)
    , showZone
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng.Raw (RawLat, RawLng, showLat, showLng)
import Flight.Zone.Radius (Radius(..))

data RawZone =
    RawZone { zoneName :: String
            , lat :: RawLat
            , lng :: RawLng
            , radius :: Radius (Quantity Double [u| m |])
            }
            deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

showZone :: RawZone -> String
showZone (RawZone name lat' lng' rad) =
    unwords [ name
            , showLat lat'
            , showLng lng'
            , show rad
            ]
