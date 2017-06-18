module Data.Flight.Types
    ( Latitude
    , Longitude
    , Radius
    , Name
    , Turnpoint(..)
    , Task(..)
    ) where

type Name = String
type Latitude = String
type Longitude = String
type Radius = String

data Task = Task Name [Turnpoint] deriving (Eq, Show)
data Turnpoint = Turnpoint Name Latitude Longitude Radius deriving (Eq, Show)
