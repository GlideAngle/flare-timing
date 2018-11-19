{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

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
    , fromSci
    , toSci
    , showRadius
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
        , utcOffset :: String 
        }
    deriving (Show, Generic, ToJSON, FromJSON)

data Nominal =
    Nominal
        { distance :: String
        , time :: String 
        , goal :: String 
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
    | r < 1000 = show r ++ " m"
    | otherwise = show (truncate (r / 1000) :: Integer) ++ " km"
