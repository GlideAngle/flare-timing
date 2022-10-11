module Flight.LatLng.RawLatLng
    ( RawLatLng(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson
    (ToJSON(..), FromJSON(..), (.:), (.=), object, withObject)
import Flight.LatLng.Raw

data RawLatLng =
    RawLatLng
        { lat :: RawLat
        , lng :: RawLng
        }
    deriving (Eq, Ord, Generic)

instance Show RawLatLng where
    show RawLatLng{lat, lng} =
        "(" ++ showLat lat ++ "," ++ showLng lng ++ ")"

instance ToJSON RawLatLng where
    toJSON RawLatLng{..} =
        object ["lat" .= lat, "lng" .= lng]

instance FromJSON RawLatLng where
    parseJSON = withObject "RawLatLng" $ \v -> RawLatLng
        <$> v .: "lat"
        <*> v .: "lng"
