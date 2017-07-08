module Data.Flight.Types
    ( Fix(..)
    , LLA(..)
    , LatLngAlt(..)
    , FixMark(..)
    , Seconds
    , Latitude
    , Longitude
    , Altitude
    , mkPosition
    ) where

type Latitude = Rational
type Longitude = Rational
type Altitude = Integer
type Seconds = Integer

data LLA = LLA Latitude Longitude Altitude deriving (Eq, Show)
data Fix = Fix Seconds LLA (Maybe Altitude) deriving (Eq, Show)

mkPosition :: (Latitude, Longitude, Altitude) -> LLA
mkPosition (lat', lng', alt') = LLA lat' lng' alt'

class LatLngAlt a where
    lat :: a -> Latitude
    lng :: a -> Longitude
    altGps :: a -> Altitude

instance LatLngAlt LLA where
    lat (LLA x _ _) = x
    lng (LLA _ x _) = x
    altGps (LLA _ _ x) = x

instance LatLngAlt Fix where
    lat (Fix _ x _) = lat x
    lng (Fix _ x _) = lng x
    altGps (Fix _ x _) = altGps x

class LatLngAlt a => FixMark a where
    mark :: a -> Seconds
    altBaro :: a -> Maybe Altitude

instance FixMark Fix where
    mark (Fix x _ _) = x
    altBaro (Fix _ _ x) = x
