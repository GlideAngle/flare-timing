module Data.Flight.Types
    ( LLA(..)
    , Fix(..)
    , Seconds
    , Latitude
    , Longitude
    , Altitude
    ) where

type Latitude = Rational
type Longitude = Rational
type Altitude = Integer
type Seconds = Integer

data LLA = LLA Latitude Longitude Altitude deriving (Eq, Show)
data Fix = Fix Seconds LLA Altitude deriving (Eq, Show)

