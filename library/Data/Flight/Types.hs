{-# LANGUAGE DeriveGeneric #-}

module Data.Flight.Types
    ( Latitude(..)
    , Longitude(..)
    , Radius
    , Name
    , Turnpoint(..)
    , Task(..)
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

data Task = Task Name SpeedSection [Turnpoint] deriving (Eq, Show, Generic)
data Turnpoint = Turnpoint Name Latitude Longitude Radius deriving (Eq, Show, Generic)

instance ToJSON Turnpoint
instance FromJSON Turnpoint

instance ToJSON Task
instance FromJSON Task

fromSci :: Scientific -> Rational
fromSci x = toRational (toRealFloat x :: Double)

toSci  :: Rational -> Scientific
toSci x =
    case fromRationalRepetend Nothing x of
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
    | otherwise = let y = truncate (r % 1000) :: Integer in show y ++ " km"
