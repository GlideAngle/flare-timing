{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}

module FlareTiming.Task
    ( Task(..)
    , Turnpoint(..)
    , Latitude(..)
    , Longitude(..)
    , Name
    , Radius
    , SpeedSection
    , fromSci
    , toSci
    , forbes
    ) where

import Prelude hiding (map)
import Control.Monad
import Control.Applicative
import Data.Aeson
import GHC.Generics
import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map (Map, fromList, union)
import Data.Monoid((<>))
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

forbes :: Task
forbes =
    Task "Day1" (Just (2, 5)) [ launch, start, tp1, tp2, goal ]
    where
        launch = Turnpoint "FORBES" (Latitude $ negate 33.36137) (Longitude 147.93207) 100
        start = Turnpoint "FORBES" (Latitude $ negate 33.36137) (Longitude 147.93207) 10000
        tp1 = Turnpoint "PINEY" (Latitude $ negate 33.85373) (Longitude 147.94195) 400
        tp2 = Turnpoint "EUGOWR" (Latitude $ negate 33.4397) (Longitude 148.34533) 400
        goal = Turnpoint "GOALD1" (Latitude $ negate 33.61965) (Longitude 148.4099) 400
