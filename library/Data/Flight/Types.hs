module Data.Flight.Types
    ( Fix(..)
    , LLA(..)
    , AtTime(..)
    , Lat(..)
    , Lng(..)
    , AltGps(..)
    , AltBaro(..)
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
data Fix = Fix Seconds LLA Altitude deriving (Eq, Show)

mkPosition :: (Latitude, Longitude, Altitude) -> LLA
mkPosition (lat', lng', alt') = LLA lat' lng' alt'

class AtTime a where
    mark :: a -> Seconds

class Lat a where
    lat :: a -> Latitude

class Lng a where
    lng :: a -> Longitude

class AltGps a where
    altGps :: a -> Altitude

class AltBaro a where
    altBaro :: a -> Altitude

instance AtTime Fix where
    mark (Fix x _ _) = x

instance Lat Fix where
    lat (Fix _ x _) = lat x

instance Lng Fix where
    lng (Fix _ x _) = lng x

instance AltGps Fix where
    altGps (Fix _ x _) = altGps x

instance AltBaro Fix where
    altBaro (Fix _ _ x) = x

instance Lat LLA where
    lat (LLA x _ _) = x

instance Lng LLA where
    lng (LLA _ x _) = x

instance AltGps LLA where
    altGps (LLA _ _ x) = x
