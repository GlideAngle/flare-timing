module WireTypes.Zone
    ( RawZone(..)
    , Zones(..)
    , RawLat(..)
    , RawLng(..)
    , Radius(..)
    , showRadius
    , showLat
    , showLng
    ) where

import Control.Applicative (empty)
import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Aeson (Value(..), ToJSON(..), FromJSON(..))
import Data.Scientific (Scientific, toRealFloat, fromRationalRepetend)

newtype RawLat = RawLat Rational
    deriving (Eq, Ord, Show)

newtype RawLng = RawLng Rational
    deriving (Eq, Ord, Show)

newtype Radius = Radius Double
    deriving (Eq, Ord, Show)

data RawZone =
    RawZone
        { zoneName :: String
        , lat :: RawLat
        , lng :: RawLng
        , radius :: Radius
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Zones =
    Zones
        { raw :: [RawZone]
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

fromSci :: Scientific -> Rational
fromSci x = toRational (toRealFloat x :: Double)

toSci :: Rational -> Scientific
toSci x =
    case fromRationalRepetend (Just 7) x of
        Left (s, _) -> s
        Right (s, _) -> s

instance ToJSON Radius where
    toJSON (Radius x) = String . T.pack $ show x ++ " m"

instance FromJSON Radius where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'm' : ' ' : xs -> return . Radius . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

instance ToJSON RawLat where
    toJSON (RawLat x) = Number $ toSci x

instance FromJSON RawLat where
    parseJSON x@(Number _) = RawLat . fromSci <$> parseJSON x
    parseJSON _ = empty

instance ToJSON RawLng where
    toJSON (RawLng x) = Number $ toSci x

instance FromJSON RawLng where
    parseJSON x@(Number _) = RawLng . fromSci <$> parseJSON x
    parseJSON _ = empty

showRadius :: Radius -> String
showRadius (Radius r)
    | r < 1000 = show (truncate r :: Integer) ++ " m"
    | otherwise = show (truncate (r / 1000) :: Integer) ++ " km"

showLat :: RawLat -> String
showLat (RawLat x) = (show . toSci $ x) ++ " °"

showLng :: RawLng -> String
showLng (RawLng x) = (show . toSci $ x) ++ " °"
