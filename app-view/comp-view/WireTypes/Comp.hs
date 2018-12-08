module WireTypes.Comp
    ( Comp(..)
    , Nominal(..)
    , Task(..)
    , Name
    , SpeedSection
    , UtcOffset(..)
    , getAllRawZones
    , getRaceRawZones
    , getSpeedSection
    , getAbsent
    , fromSci
    , toSci
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..))
import Data.Scientific (Scientific, toRealFloat, fromRationalRepetend)
import WireTypes.Zone (RawZone, Zones(..))
import WireTypes.Pilot (Pilot)

type Name = String

type SpeedSection = Maybe (Integer, Integer)

newtype UtcOffset = UtcOffset { timeZoneMinutes :: Int }
    deriving (Eq, Ord, Show, Read, Generic)
    deriving anyclass (FromJSON)

data Comp =
    Comp
        { civilId :: String
        , compName :: String
        , location :: String
        , from :: String
        , to :: String
        , utcOffset :: UtcOffset
        }
    deriving (Show, Generic, FromJSON)

data Nominal =
    Nominal
        { distance :: String
        , free :: String
        , time :: String
        , goal :: Double
        , launch :: Double
        }
    deriving (Show, Generic, FromJSON)

data Task =
    Task
        { taskName :: Name
        , zones :: Zones
        , speedSection :: SpeedSection
        , absent :: [Pilot]
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

fromSci :: Scientific -> Rational
fromSci x = toRational (toRealFloat x :: Double)

toSci :: Rational -> Scientific
toSci x =
    case fromRationalRepetend (Just 7) x of
        Left (s, _) -> s
        Right (s, _) -> s

getAbsent :: Task -> [Pilot]
getAbsent Task{absent} = absent

getSpeedSection :: Task -> SpeedSection
getSpeedSection Task{speedSection = ss} = ss

getAllRawZones :: Task -> [RawZone]
getAllRawZones Task{zones = Zones{raw}} = raw

getRaceRawZones :: Task -> [RawZone]
getRaceRawZones Task{zones = Zones{raw = tps}, speedSection = ss} =
    speedSectionOnly ss tps
    where
        speedSectionOnly :: SpeedSection -> [RawZone] -> [RawZone]
        speedSectionOnly Nothing xs =
            xs
        speedSectionOnly (Just (start, end)) xs =
            take (end' - start' + 1) $ drop (start' - 1) xs
            where
                start' = fromInteger start
                end' = fromInteger end
