module Data.Flight.Types
    ( Latitude
    , Longitude
    , Radius
    , Name
    , Turnpoint(..)
    , Task(..)
    , SpeedSection
    ) where

type Name = String
type Latitude = Rational
type Longitude = Rational
type Radius = Integer
type SpeedSection = Maybe (Integer, Integer)

data Task = Task Name SpeedSection [Turnpoint] deriving (Eq, Show)
data Turnpoint = Turnpoint Name Latitude Longitude Radius deriving (Eq, Show)
