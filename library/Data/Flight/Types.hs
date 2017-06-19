{-# LANGUAGE DeriveGeneric #-}

module Data.Flight.Types
    ( Latitude
    , Longitude
    , Radius
    , Name
    , Turnpoint(..)
    , Task(..)
    , SpeedSection
    ) where

import GHC.Generics
import Data.Aeson

type Name = String
type Latitude = Rational
type Longitude = Rational
type Radius = Integer
type SpeedSection = Maybe (Integer, Integer)

data Task = Task Name SpeedSection [Turnpoint] deriving (Eq, Show, Generic)
data Turnpoint = Turnpoint Name Latitude Longitude Radius deriving (Eq, Show, Generic)

instance ToJSON Turnpoint
instance FromJSON Turnpoint

instance ToJSON Task
instance FromJSON Task
