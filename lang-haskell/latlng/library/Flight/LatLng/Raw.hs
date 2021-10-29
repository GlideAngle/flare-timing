module Flight.LatLng.Raw
    ( RawLat(..)
    , RawLng(..)
    , RawAlt(..)
    , RawLatLng(..)
    , fromSci
    , toSci
    , showLat
    , showLng
    , showAlt
    ) where

import GHC.Generics (Generic)
import Control.DeepSeq
import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson
    (ToJSON(..), FromJSON(..), (.:), (.=), object, withObject)
import qualified Data.Csv as Csv ((.:))
import Data.Csv
    (ToNamedRecord(..), FromNamedRecord(..), namedRecord, namedField)
import Data.Via.Scientific
    ( dpDegree, dpAlt, fromSci, toSci, showSci
    , deriveDecimalPlaces, deriveJsonViaSci, deriveCsvViaSci
    , deriveShowValueViaSci
    )

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

newtype RawLat = RawLat Rational deriving (Eq, Ord, Generic)
newtype RawLng = RawLng Rational deriving (Eq, Ord, Generic)
newtype RawAlt = RawAlt Rational deriving (Eq, Ord, Generic)

deriving anyclass instance NFData RawLat
deriving anyclass instance NFData RawLng
deriving anyclass instance NFData RawAlt

instance Newtype RawLat Rational where
    pack = RawLat
    unpack (RawLat a) = a

instance Newtype RawLng Rational where
    pack = RawLng
    unpack (RawLng a) = a

instance Newtype RawAlt Rational where
    pack = RawAlt
    unpack (RawAlt a) = a

csvSciLL :: Rational -> String
csvSciLL = showSci dpDegree . toSci dpDegree

csvSciAlt :: Rational -> String
csvSciAlt = showSci dpAlt . toSci dpAlt

instance ToNamedRecord RawLat where
    toNamedRecord (RawLat x) =
        namedRecord [ namedField "lat" $ csvSciLL x ]

instance ToNamedRecord RawLng where
    toNamedRecord (RawLng x) =
        namedRecord [ namedField "lng" $ csvSciLL x ]

instance ToNamedRecord RawAlt where
    toNamedRecord (RawAlt x) =
        namedRecord [ namedField "alt" $ csvSciAlt x ]

instance FromNamedRecord RawLat where
    parseNamedRecord m = RawLat . fromSci <$> m Csv..: "lat"

instance FromNamedRecord RawLng where
    parseNamedRecord m = RawLng . fromSci <$> m Csv..: "lng"

instance FromNamedRecord RawAlt where
    parseNamedRecord m = RawAlt . fromSci <$> m Csv..: "alt"

showLat :: RawLat -> String
showLat (RawLat lat') =
    if x < 0
       then showSci dpDegree (negate x) ++ "°S"
       else showSci dpDegree x ++ "°N"
    where
        x = toSci dpDegree lat'

showLng :: RawLng -> String
showLng (RawLng lng') =
    if x < 0
       then showSci dpDegree (negate x) ++ "°W"
       else showSci dpDegree x ++ "°E"
    where
        x = toSci dpDegree lng'

showAlt :: RawAlt -> String
showAlt (RawAlt alt') =
    showSci dpAlt x ++ "m"
    where
        x = toSci dpAlt alt'

deriveDecimalPlaces dpDegree ''RawLat
deriveDecimalPlaces dpDegree ''RawLng
deriveDecimalPlaces dpAlt ''RawAlt

deriveJsonViaSci ''RawLat
deriveJsonViaSci ''RawLng
deriveJsonViaSci ''RawAlt

deriveShowValueViaSci ''RawLat
deriveShowValueViaSci ''RawLng
deriveShowValueViaSci ''RawAlt

deriveCsvViaSci ''RawLat
deriveCsvViaSci ''RawLng
deriveCsvViaSci ''RawAlt
