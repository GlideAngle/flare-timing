module WireTypes.Zone
    ( RawZone(..)
    , Zones(..)
    , RawLat(..)
    , RawLng(..)
    , RawLatLng(..)
    , showLat
    , showLng
    ) where

import Control.Applicative (empty)
import GHC.Generics (Generic)
import Data.Aeson (Value(..), FromJSON(..))
import Data.Scientific (Scientific, toRealFloat, fromRationalRepetend)
import WireTypes.ZoneKind (TaskZones(..), Radius(..))

newtype RawLat = RawLat Rational
    deriving (Eq, Ord, Show)

newtype RawLng = RawLng Rational
    deriving (Eq, Ord, Show)

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
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

data Zones =
    Zones
        { raw :: [RawZone]
        , raceKind :: Maybe TaskZones
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

fromSci :: Scientific -> Rational
fromSci x = toRational (toRealFloat x :: Double)

toSci :: Rational -> Scientific
toSci x =
    case fromRationalRepetend (Just 7) x of
        Left (s, _) -> s
        Right (s, _) -> s

instance FromJSON RawLat where
    parseJSON x@(Number _) = RawLat . fromSci <$> parseJSON x
    parseJSON _ = empty

instance FromJSON RawLng where
    parseJSON x@(Number _) = RawLng . fromSci <$> parseJSON x
    parseJSON _ = empty

showLat :: RawLat -> String
showLat (RawLat x) = (show . toSci $ x) ++ " °"

showLng :: RawLng -> String
showLng (RawLng x) = (show . toSci $ x) ++ " °"
