{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
    , showLat
    , showLng
    , showTask
    , showTurnpoint
    ) where

import Data.Ratio((%))
import Data.List (intercalate)
import Control.Applicative (empty)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(Number))
import Data.Scientific
    ( Scientific
    , FPFormat(..)
    , toRealFloat
    , fromRationalRepetend
    , formatScientific
    )

type Name = String
newtype Latitude = Latitude Rational deriving (Eq, Show)
newtype Longitude = Longitude Rational deriving (Eq, Show)
type Radius = Integer
type SpeedSection = Maybe (Integer, Integer)

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

showRadius :: Radius -> String
showRadius r
    | r < 1000 = show r ++ " m"
    | otherwise = let y = truncate (r % 1000) :: Integer in show y ++ " km"

showLat :: Latitude -> String
showLat (Latitude lat) =
    if x < 0
       then showSci (negate x) ++ " S"
       else showSci x ++ " N"
    where
        x = toSci lat

showLng :: Longitude -> String
showLng (Longitude lng) =
    if x < 0
       then showSci (negate x) ++ " W"
       else showSci x ++ " E"
    where
        x = toSci lng

showTurnpoint :: Turnpoint -> String
showTurnpoint (Turnpoint name lat lng rad) =
    unwords [ name
            , showLat lat
            , showLng lng
            , showRadius rad
            ]

showTask :: Task -> String
showTask (Task name ss xs) =
    unwords [ "Task"
            , name
            , show ss
            , intercalate ", " $ showTurnpoint <$> xs
            ]
