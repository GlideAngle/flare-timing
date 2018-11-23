module Data.Flight.Types
    ( Comp(..)
    , Nominal(..)
    , Task(..)
    , Zones(..)
    , RawZone(..)
    , RawLat(..)
    , RawLng(..)
    , Name
    , Radius(..)
    , SpeedSection
    , getAllRawZones
    , getRaceRawZones
    , getSpeedSection
    , fromSci
    , toSci
    , showRadius
    , showLat
    , showLng
    ) where

import Control.Applicative (empty)
import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Aeson (Value(..), ToJSON(..), FromJSON(..))
import Data.Scientific (Scientific, toRealFloat, fromRationalRepetend)

type Name = String

newtype RawLat = RawLat Rational
    deriving (Eq, Ord, Show)

newtype RawLng = RawLng Rational
    deriving (Eq, Ord, Show)

newtype Radius = Radius Double
    deriving (Eq, Ord, Show)

type SpeedSection = Maybe (Integer, Integer)

data Comp =
    Comp
        { civilId :: String
        , compName :: String
        , location :: String
        , from :: String
        , to :: String
        }
    deriving (Show, Generic, ToJSON, FromJSON)

data Nominal =
    Nominal
        { distance :: String
        , free :: String
        , time :: String
        , goal :: Double
        , launch :: Double
        }
    deriving (Show, Generic, ToJSON, FromJSON)

data Task =
    Task
        { taskName :: Name
        , zones :: Zones
        , speedSection :: SpeedSection
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

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

getSpeedSection :: Task -> SpeedSection
getSpeedSection (Task _ _ ss) = ss

getAllRawZones :: Task -> [RawZone]
getAllRawZones (Task _ Zones{raw} _) = raw

getRaceRawZones :: Task -> [RawZone]
getRaceRawZones (Task _ Zones{raw = tps} ss) =
    speedSectionOnly ss tps
    where
        speedSectionOnly :: SpeedSection -> [RawZone] -> [RawZone]
        speedSectionOnly Nothing xs =
            xs
        speedSectionOnly (Just (start, end)) xs =
            take (end' - start' + 1) $ drop (start' - 1) xs
            where
                start' = fromInteger start
                end' = fromInteger end
