module Data.Flight.LatLng
    ( Latitude(..)
    , Longitude(..)
    , fromSci
    , toSci
    , showLat
    , showLng
    ) where

import Control.Applicative (empty)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(Number))
import Data.Scientific
    ( Scientific
    , FPFormat(..)
    , toRealFloat
    , fromRationalRepetend
    , formatScientific
    )

newtype Latitude = Latitude Rational deriving (Eq, Show)
newtype Longitude = Longitude Rational deriving (Eq, Show)

fromSci :: Scientific -> Rational
fromSci x = toRational (toRealFloat x :: Double)

toSci :: Rational -> Scientific
toSci x =
    case fromRationalRepetend (Just 7) x of
        Left (s, _) -> s
        Right (s, _) -> s

showSci :: Scientific -> String
showSci =
    formatScientific Fixed (Just 3)

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

showLat :: Latitude -> String
showLat (Latitude lat') =
    if x < 0
       then showSci (negate x) ++ " S"
       else showSci x ++ " N"
    where
        x = toSci lat'

showLng :: Longitude -> String
showLng (Longitude lng') =
    if x < 0
       then showSci (negate x) ++ " W"
       else showSci x ++ " E"
    where
        x = toSci lng'
