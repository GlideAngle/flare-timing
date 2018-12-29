module WireTypes.Comp
    ( Comp(..)
    , Nominal(..)
    , Task(..)
    , Name
    , SpeedSection
    , OpenClose(..)
    , StartGate(..)
    , UtcOffset(..)
    , ScoreBackTime
    , getAllRawZones
    , getRaceRawZones
    , getGoalShape
    , getEssShape
    , getOpenShape
    , getSpeedSection
    , getOpenClose
    , getStartGates
    , getAbsent
    , fromSci
    , toSci
    , showNominalTime
    , showScoreBackTime
    ) where

import Text.Printf (printf)
import Data.Time.Clock (UTCTime)
import Control.Applicative (empty)
import Control.Monad (join)
import GHC.Generics (Generic)
import Data.Aeson (Value(..), FromJSON(..))
import qualified Data.Text as T (unpack)
import Data.Scientific (Scientific, toRealFloat, fromRationalRepetend)
import WireTypes.Zone (RawZone, Zones(..))
import WireTypes.ZoneKind
import WireTypes.Pilot (Pilot)

type Name = String

type SpeedSection = Maybe (Integer, Integer)

data OpenClose =
    OpenClose
        { open :: UTCTime 
        , close :: UTCTime
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype StartGate = StartGate UTCTime
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype UtcOffset = UtcOffset { timeZoneMinutes :: Int }
    deriving (Eq, Ord, Show, Read, Generic)
    deriving anyclass (FromJSON)

newtype NominalTime = NominalTime Double
    deriving (Eq, Ord)

instance FromJSON NominalTime where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'h' : ' ' : xs -> return . NominalTime . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

instance Show NominalTime where
    show = showNominalTime

showNominalTime :: NominalTime -> String
showNominalTime (NominalTime h) =
    if ms == 0 then printf "%d h" hh else printf "%d:%02d:%02d" hh mm ss'
    where
        totalSecs :: Int
        totalSecs = round $ 3600.0 * h

        (hh, ms) = quotRem (abs totalSecs) 3600
        mm = quot ms 60

        ss =
            (abs h - fromIntegral hh) * 3600.0
            - fromIntegral (mm * 60)

        ss' :: Int
        ss' = truncate ss

newtype ScoreBackTime = ScoreBackTime Double
    deriving (Eq, Ord)

instance FromJSON ScoreBackTime where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            's' : ' ' : xs -> return . ScoreBackTime . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

instance Show ScoreBackTime where
    show = showScoreBackTime

showScoreBackTime :: ScoreBackTime -> String
showScoreBackTime (ScoreBackTime s) =
    if hh == 0 && ss' == 0
        then printf "%d mins" mm
        else printf "%d:%02d:%02d" hh mm ss'
    where
        totalSecs :: Int
        totalSecs = round s

        h = s / 3600.0

        (hh, ms) = quotRem (abs totalSecs) 3600
        mm = quot ms 60

        ss =
            (abs h - fromIntegral hh) * 3600.0
            - fromIntegral (mm * 60)

        ss' :: Int
        ss' = truncate ss

data Give =
    Give
        { giveFraction :: Double
        , giveDistance :: Maybe Radius
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

data Comp =
    Comp
        { civilId :: String
        , compName :: String
        , location :: String
        , from :: String
        , to :: String
        , utcOffset :: UtcOffset
        , scoreBack :: Maybe ScoreBackTime
        , give :: Maybe Give
        }
    deriving (Show, Generic, FromJSON)

data Nominal =
    Nominal
        { distance :: String
        , free :: String
        , time :: NominalTime
        , goal :: Double
        , launch :: Double
        }
    deriving (Show, Generic, FromJSON)

data Task =
    Task
        { taskName :: Name
        , zones :: Zones
        , speedSection :: SpeedSection
        , zoneTimes :: [OpenClose]
        , startGates :: [StartGate]
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

getOpenClose :: Task -> [OpenClose]
getOpenClose Task{zoneTimes = ts} = ts

getStartGates :: Task -> [StartGate]
getStartGates Task{startGates = gs} = gs

getAllRawZones :: Task -> [RawZone]
getAllRawZones Task{zones = Zones{raw}} = raw

getGoalShape :: Task -> Maybe Shape
getGoalShape Task{zones = Zones{raceKind}} =
    join $
    (\case
        TzEssIsGoal (ZoneKind g) -> Just g
        TzEssIsNotGoal _ (ZoneKind g) -> Just g
        TzOpenDistance _ -> Nothing)
    <$> raceKind

getEssShape :: Task -> Maybe Shape
getEssShape Task{zones = Zones{raceKind}} =
    join $
    (\case
        TzEssIsGoal _ -> Nothing
        TzEssIsNotGoal (ZoneKind e) _ -> Just e
        TzOpenDistance _ -> Nothing)
    <$>
    raceKind


getOpenShape :: Task -> Maybe Shape
getOpenShape Task{zones = Zones{openKind}} =
    join $
    (\case
        TzEssIsGoal _ -> Nothing
        TzEssIsNotGoal _ _ -> Nothing
        TzOpenDistance (ZoneKind o) -> Just o)
    <$>
    openKind

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
