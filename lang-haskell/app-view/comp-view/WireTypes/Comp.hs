module WireTypes.Comp
    ( Comp(..)
    , Give(..)
    , Discipline(..)
    , Nominal(..)
    , Task(..)
    , TaskStop(..)
    , Name
    , SpeedSection
    , OpenClose(..)
    , UtcOffset(..)
    , MinimumDistance(..)
    , ScoreBackTime
    , Projection(..)
    , EarthMath(..)
    , Ellipsoid(..)
    , EarthModel(..)
    , Tweak(..)
    , LwScaling(..)
    , AwScaling(..)
    , EGwScaling(..)
    , JumpTheGunLimit(..)
    , SecondsPerPoint(..)
    , EarlyStart(..)
    , PowerExponent(..)
    , powerExp23
    , scaling
    , getAllRawZones
    , getRaceRawZones
    , getGoalShape
    , getEssShape
    , getOpenShape
    , getSpeedSection
    , getOpenClose
    , getStartGates
    , fromSci
    , toSci
    , showMinimumDistance
    , showNominalTime
    , showScoreBackTime
    , showEarthMath
    , showEarlyStartEarliest
    , showEarlyStartPenaltyRate
    ) where

import Text.Printf (printf)
import Data.Time.Clock (UTCTime)
import Data.Maybe (fromMaybe)
import Control.Applicative (empty)
import Control.Monad (join)
import GHC.Generics (Generic)
import Data.Aeson
    ( Value(..), FromJSON(..), Options(..), SumEncoding(..)
    , genericParseJSON, defaultOptions
    )
import qualified Data.Text as T (Text, pack, unpack)
import Data.Scientific (Scientific, toRealFloat, fromRationalRepetend)
import WireTypes.Zone (RawZone, Zones(..))
import WireTypes.ZoneKind
import WireTypes.Pilot (Pilot)
import WireTypes.Penalty (PenaltySeqs)
import WireTypes.Point (StartGate(..))
import FlareTiming.Time (UtcOffset(..))
import FlareTiming.Time (showHmsForSecs)

type Name = String

type SpeedSection = Maybe (Integer, Integer)

data OpenClose =
    OpenClose
        { open :: UTCTime
        , close :: UTCTime
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype MinimumDistance = MinimumDistance Double
    deriving (Eq, Ord)

instance FromJSON MinimumDistance where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'm' : 'k' : ' ' : xs -> return . MinimumDistance . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

showMinimumDistance :: MinimumDistance -> T.Text
showMinimumDistance (MinimumDistance d) =
    T.pack . printf "%.1f km" $ d

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

data Projection = UTM
    deriving (Eq, Ord, Show, Generic)

instance FromJSON Projection where
    parseJSON _ = return UTM

data EarthMath
    = Pythagorus
    | Haversines
    | Vincenty
    | AndoyerLambert
    | ForsytheAndoyerLambert
    | FsAndoyer
    deriving (Eq, Ord, Show)

instance FromJSON EarthMath where
    parseJSON o@(String _) = do
        s :: String <- parseJSON o
        case s of
            "Pythagorus" -> return Pythagorus
            "Haversines" -> return Haversines
            "Vincenty" -> return Vincenty
            "Andoyer-Lambert" -> return AndoyerLambert
            "Forsythe-Andoyer-Lambert" -> return ForsytheAndoyerLambert
            "FS-Andoyer" -> return FsAndoyer
            _ -> empty

    parseJSON _ = empty

showEarthMath :: EarthMath -> T.Text
showEarthMath Pythagorus = "Pythagorus"
showEarthMath Haversines = "Haversines"
showEarthMath Vincenty = "Vincenty"
showEarthMath AndoyerLambert = "Andoyer-Lambert"
showEarthMath ForsytheAndoyerLambert = "Forsythe-Andoyer-Lambert"
showEarthMath FsAndoyer = "FS-Andoyer"

data Ellipsoid =
    Ellipsoid
        { equatorialR :: Radius
        , recipF :: Double
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

data EarthModel
    = EarthAsSphere {radius :: Radius}
    | EarthAsEllipsoid Ellipsoid
    | EarthAsFlat {projection :: Projection}
    deriving (Eq, Ord, Show, Generic)

earthModelCtorTag :: String -> String
earthModelCtorTag s
    | s == "EarthAsSphere" = "sphere"
    | s == "EarthAsEllipsoid" = "ellipsoid"
    | s == "EarthAsFlat" = "flat"
    | otherwise = s

instance FromJSON EarthModel where
    parseJSON = genericParseJSON $
        defaultOptions
            { sumEncoding = ObjectWithSingleField
            , constructorTagModifier = earthModelCtorTag
            }

data Give =
    Give
        { giveFraction :: Double
        , giveDistance :: Maybe Radius
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

data Discipline
    = HangGliding
    | Paragliding
    deriving (Eq, Ord, Generic)

disciplineOptions :: Options
disciplineOptions =
    defaultOptions
        { constructorTagModifier = \case
            "HangGliding" -> "hg"
            "Paragliding" -> "pg"
            x -> x
        }

instance Show Discipline where
    show HangGliding = "hg"
    show Paragliding = "pg"

instance Read Discipline where
    readsPrec _ ('h' : 'g' : s) = [(HangGliding, s)]
    readsPrec _ ('p' : 'g' : s) = [(Paragliding, s)]
    readsPrec _ _ = []

instance FromJSON Discipline where
  parseJSON = genericParseJSON disciplineOptions

data Comp =
    Comp
        { civilId :: String
        , compName :: String
        , discipline :: Discipline
        , location :: String
        , from :: String
        , to :: String
        , utcOffset :: UtcOffset
        , scoreBack :: Maybe ScoreBackTime
        , give :: Maybe Give
        , earth :: EarthModel
        , earthMath :: EarthMath
        }
    deriving (Generic, FromJSON)

data Nominal =
    Nominal
        { distance :: String
        , free :: MinimumDistance
        , time :: NominalTime
        , goal :: Double
        , launch :: Double
        }
    deriving (Generic, FromJSON)

newtype LwScaling = LwScaling Double
    deriving (Eq, Ord, Generic)
    deriving anyclass FromJSON

newtype EGwScaling = EGwScaling Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass FromJSON

newtype AwScaling = AwScaling Double
    deriving (Eq, Ord, Generic)
    deriving anyclass FromJSON

instance Show LwScaling where
    show (LwScaling 0) = "0"
    show (LwScaling x) = show x

instance Show AwScaling where
    show (AwScaling 0) = "0"
    show (AwScaling x) = show x

newtype PowerExponent = PowerExponent Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass FromJSON

powerExp23 :: PowerExponent
powerExp23 = PowerExponent $ 2/3

data Tweak =
    Tweak
        { leadingWeightScaling :: Maybe LwScaling
        , leadingAreaDistanceSquared :: Bool
        , arrivalRank :: Bool
        , arrivalTime :: Bool
        , timePowerExponent :: PowerExponent
        , essNotGoalScaling :: EGwScaling
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

data TaskStop =
    TaskStop
        { announced :: UTCTime
        , retroactive :: UTCTime
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype JumpTheGunLimit = JumpTheGunLimit Double
    deriving (Eq, Ord)

newtype SecondsPerPoint = SecondsPerPoint Double
    deriving (Eq, Ord)

newtype TooEarlyPoints = TooEarlyPoints Int
    deriving (Eq, Ord, Show)
    deriving newtype (FromJSON)

showEarlyStartEarliest :: EarlyStart -> T.Text
showEarlyStartEarliest EarlyStart{earliest = JumpTheGunLimit jtg} =
    showHmsForSecs jtg

showEarlyStartPenaltyRate :: EarlyStart -> T.Text
showEarlyStartPenaltyRate EarlyStart{earlyPenalty = SecondsPerPoint spp} =
    T.pack $ printf "1/%.0fs" spp

instance FromJSON JumpTheGunLimit where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            's' : ' ' : xs -> return . JumpTheGunLimit . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

instance FromJSON SecondsPerPoint where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            's' : ' ' : xs -> return . SecondsPerPoint . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

data EarlyStart =
    EarlyStart
        { earliest :: JumpTheGunLimit
        , earlyPenalty :: SecondsPerPoint
        , tooEarlyReset :: TooEarlyPoints
        }
    deriving (Eq, Ord, Generic)
    deriving anyclass (FromJSON)

data Task =
    Task
        { taskName :: Name
        , zones :: Zones
        , speedSection :: SpeedSection
        , zoneTimes :: [OpenClose]
        , startGates :: [StartGate]
        , stopped :: Maybe TaskStop
        , cancelled :: Bool
        , taskTweak :: Maybe Tweak
        , earlyStart :: EarlyStart
        , penalsAuto :: [(Pilot, PenaltySeqs, String)]
        , penals :: [(Pilot, PenaltySeqs, String)]
        }
    deriving (Eq, Ord, Generic, FromJSON)

fromSci :: Scientific -> Rational
fromSci x = toRational (toRealFloat x :: Double)

toSci :: Rational -> Scientific
toSci x =
    case fromRationalRepetend (Just 7) x of
        Left (s, _) -> s
        Right (s, _) -> s

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
        TzEssIsGoal (ZoneKind e) -> Just e
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

scaling :: Discipline -> Maybe Tweak -> Tweak
scaling HangGliding Nothing =
    Tweak
        { leadingWeightScaling = Just (LwScaling 1)
        , leadingAreaDistanceSquared = True
        , arrivalRank = False
        , arrivalTime = False
        , timePowerExponent = powerExp23
        , essNotGoalScaling = EGwScaling 0.8
        }

scaling Paragliding Nothing =
    Tweak
        { leadingWeightScaling = Just (LwScaling 2)
        , leadingAreaDistanceSquared = True
        , arrivalRank = False
        , arrivalTime = False
        , timePowerExponent = powerExp23
        , essNotGoalScaling = EGwScaling 0
        }

scaling
    HangGliding
    (Just Tweak{leadingWeightScaling = lw, ..}) =
    Tweak
        { leadingWeightScaling = Just lw'
        , leadingAreaDistanceSquared
        , arrivalRank
        , arrivalTime
        , timePowerExponent
        , essNotGoalScaling
        }
    where
        lw' = fromMaybe (LwScaling 1) lw

scaling
    Paragliding
    (Just Tweak{leadingWeightScaling = lw, ..}) =
    Tweak
        { leadingWeightScaling = Just lw'
        , leadingAreaDistanceSquared
        , arrivalRank
        , arrivalTime
        , timePowerExponent
        , essNotGoalScaling
        }
    where
        lw' = fromMaybe (LwScaling 2) lw
