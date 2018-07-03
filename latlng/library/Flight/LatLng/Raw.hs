module Flight.LatLng.Raw
    ( RawLat(..)
    , RawLng(..)
    , RawLatLng(..)
    , fromSci
    , toSci
    , showLat
    , showLng
    ) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson
    (ToJSON(..), FromJSON(..), (.:), (.=), object, withObject)
import qualified Data.Csv as Csv ((.:))
import Data.Csv
    (ToNamedRecord(..), FromNamedRecord(..), namedRecord, namedField)
import Data.Via.Scientific
    ( dpDegree, fromSci, toSci, showSci
    , deriveDecimalPlaces, deriveJsonViaSci, deriveCsvViaSci
    )

data RawLatLng =
    RawLatLng
        { lat :: RawLat
        , lng :: RawLng
        }
    deriving (Eq, Ord, Show)

instance ToJSON RawLatLng where
    toJSON RawLatLng{..} =
        object ["lat" .= lat, "lng" .= lng]

instance FromJSON RawLatLng where
    parseJSON = withObject "RawLatLng" $ \v -> RawLatLng
        <$> v .: "lat"
        <*> v .: "lng"

newtype RawLat = RawLat Rational deriving (Eq, Ord, Show)
newtype RawLng = RawLng Rational deriving (Eq, Ord, Show)

deriveDecimalPlaces dpDegree ''RawLat
deriveDecimalPlaces dpDegree ''RawLng

deriveJsonViaSci ''RawLat
deriveJsonViaSci ''RawLng

deriveCsvViaSci ''RawLat
deriveCsvViaSci ''RawLng

instance Newtype RawLat Rational where
    pack = RawLat
    unpack (RawLat a) = a

instance Newtype RawLng Rational where
    pack = RawLng
    unpack (RawLng a) = a

csvSci :: Rational -> String
csvSci = showSci dpDegree . toSci dpDegree

instance ToNamedRecord RawLat where
    toNamedRecord (RawLat x) =
        namedRecord [ namedField "lat" $ csvSci x ]

instance ToNamedRecord RawLng where
    toNamedRecord (RawLng x) =
        namedRecord [ namedField "lng" $ csvSci x ]

instance FromNamedRecord RawLat where
    parseNamedRecord m = RawLat . fromSci <$> m Csv..: "lat"

instance FromNamedRecord RawLng where
    parseNamedRecord m = RawLng . fromSci <$> m Csv..: "lat"

showLat :: RawLat -> String
showLat (RawLat lat') =
    if x < 0
       then showSci dpDegree (negate x) ++ " S"
       else showSci dpDegree x ++ " N"
    where
        x = toSci dpDegree lat'

showLng :: RawLng -> String
showLng (RawLng lng') =
    if x < 0
       then showSci dpDegree (negate x) ++ " W"
       else showSci dpDegree x ++ " E"
    where
        x = toSci dpDegree lng'
