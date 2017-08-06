{-# LANGUAGE DeriveGeneric #-}

module Data.Flight.Types
    ( Comp(..)
    , Nominal(..)
    , Task(..)
    , Turnpoint(..)
    , Latitude(..)
    , Longitude(..)
    , Name
    , Radius
    , SpeedSection
    , fromSci
    , toSci
    , showRadius
    ) where

import Data.Ratio((%))
import Control.Applicative
import GHC.Generics
import Data.Aeson
import Data.Scientific (Scientific, toRealFloat, fromRationalRepetend)

type Name = String
newtype Latitude = Latitude Rational deriving (Eq, Show)
newtype Longitude = Longitude Rational deriving (Eq, Show)
type Radius = Integer
type SpeedSection = Maybe (Integer, Integer)

data Comp = Comp { civilId :: String
                 , compName :: String 
                 , location :: String 
                 , from :: String 
                 , to :: String 
                 , utcOffset :: String 
                 } deriving (Show, Generic)

instance ToJSON Comp
instance FromJSON Comp

data Nominal = Nominal { distance :: String
                       , time :: String 
                       , goal :: String 
                       } deriving (Show, Generic)

instance ToJSON Nominal
instance FromJSON Nominal

data Task =
    Task { taskName :: Name
         , speedSection :: SpeedSection
         , zones :: [Turnpoint]
         } deriving (Eq, Show, Generic)

data Turnpoint =
    Turnpoint { zoneName :: Name
              , lat :: Latitude
              , lng :: Longitude
              , radius :: Radius
              } deriving (Eq, Show, Generic)

instance ToJSON Turnpoint
instance FromJSON Turnpoint

instance ToJSON Task
instance FromJSON Task

fromSci :: Scientific -> Rational
fromSci x = toRational (toRealFloat x :: Double)

toSci :: Rational -> Scientific
toSci x =
    case fromRationalRepetend (Just 7) x of
        Left (s, _) -> s
        Right (s, _) -> s

instance ToJSON Latitude where
    toJSON (Latitude x) = Number $ toSci x

instance FromJSON Latitude where
    parseJSON x@(Number _) = Latitude . fromSci <$> parseJSON x
    parseJSON _ = empty

instance ToJSON Longitude where
    toJSON (Longitude x) = Number $ toSci x

instance FromJSON Longitude where
    parseJSON x@(Number _) = Longitude . fromSci <$> parseJSON x
    parseJSON _ = empty

showRadius :: Radius -> String
showRadius r
    | r < 1000 = show r ++ " m"
    | otherwise = show (truncate (r % 1000)) ++ " km"
