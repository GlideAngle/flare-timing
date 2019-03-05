{-# LANGUAGE DuplicateRecordFields #-}

module WireTypes.Point
    ( StartGate(..)
    , Velocity(..)
    , NormBreakdown(..)
    , Breakdown(..)
    , Allocation(..)
    , GoalRatio(..)
    , PilotDistance(..)
    , PilotTime(..)
    , Alt(..)
    , PilotVelocity(..)
    , DistancePoints(..)
    , LinearPoints(..)
    , DifficultyPoints(..)
    , ArrivalPoints(..)
    , TimePoints(..)
    , TaskPlacing(..)
    , TaskPoints(..)
    , Points(..)
    , DistanceWeight(..)
    , LeadingWeight(..)
    , ArrivalWeight(..)
    , TimeWeight(..)
    , Weights(..)
    , PointPenalty(..)
    -- * Showing Breakdown
    , showPilotDistance
    , showPilotAlt
    -- * Showing Points
    , showDistancePoints
    , showLinearPoints
    , showDifficultyPoints
    , showArrivalPoints
    , showTimePoints
    , showLeadingPoints
    , showTaskPoints
    -- * Showing Weights
    , showDistanceWeight
    , showArrivalWeight
    , showTimeWeight
    , showLeadingWeight
    , zeroWeights
    ) where

import Text.Printf (printf)
import Control.Applicative (empty)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson
    ( Value(..), FromJSON(..), Options(..), SumEncoding(..)
    , genericParseJSON, defaultOptions
    )
import qualified Data.Text as T (Text, pack, unpack)

newtype StartGate = StartGate UTCTime
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype GoalRatio = GoalRatio Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype PilotTime = PilotTime Double
    deriving (Eq, Ord, Show)

newtype PilotDistance = PilotDistance Double
    deriving (Eq, Ord, Show, Generic)

newtype PilotVelocity a = PilotVelocity a
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype NominalGoal = NominalGoal Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype ArrivalPoints = ArrivalPoints Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype DistancePoints = DistancePoints Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype LinearPoints = LinearPoints Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype DifficultyPoints = DifficultyPoints Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype LeadingPoints = LeadingPoints Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype TimePoints = TimePoints Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype Alt = Alt Double
    deriving (Eq, Ord, Show, Generic)

instance FromJSON PilotTime where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'h' : ' ' : xs -> return . PilotTime . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

instance FromJSON Alt where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'm' : ' ' : xs -> return . Alt . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

instance FromJSON PilotDistance where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'm' : 'k' : ' ' : xs -> return . PilotDistance . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

data PointPenalty
    = PenaltyPoints Double
    | PenaltyFraction Double
    deriving (Eq, Ord, Show, Generic)

pointPenaltyOptions :: Options
pointPenaltyOptions =
    defaultOptions
        { sumEncoding = ObjectWithSingleField
        , constructorTagModifier = \case
            "PenaltyPoints" -> "penalty-points"
            "PenaltyFraction" -> "penalty-fraction"
            s -> s
        }

instance FromJSON PointPenalty where
    parseJSON = genericParseJSON pointPenaltyOptions

showPilotDistance :: PilotDistance -> T.Text
showPilotDistance (PilotDistance d) =
    T.pack . printf "%.3f" $ d

showPilotAlt :: Alt -> T.Text
showPilotAlt (Alt a) =
    T.pack . printf "%.0f" $ a

data TaskPlacing
    = TaskPlacing Integer
    | TaskPlacingEqual Integer
    deriving (Eq, Ord, Show)

instance FromJSON TaskPlacing where
    parseJSON x@(String _) = do
        s <- T.unpack <$> parseJSON x
        case reverse s of
            '=' : digits ->
                return . TaskPlacingEqual . read . reverse $ digits

            _ ->
                return . TaskPlacing . read $ s

    parseJSON _ = empty

newtype TaskPoints = TaskPoints Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

showMax'
    :: (Show a, RealFrac a)
    => (a -> String)
    -> a
    -> (b -> a)
    -> Maybe b
    -> T.Text
showMax' ppr p unwrap task =
    T.pack . maybe id (f . unwrap) task $ ppr p
    where
        f task'
            | task' == p = \s -> "*" ++ s
            | otherwise = id

showMaxRounded
    :: (Show a, RealFrac a)
    => a
    -> (b -> a)
    -> Maybe b
    -> T.Text
showMaxRounded = showMax' (show . (\x -> round x :: Integer))

showMax
    :: (Show a, RealFrac a)
    => a
    -> (b -> a)
    -> Maybe b
    -> T.Text
showMax = showMax' show

showMaxDistance :: Double -> (b -> Double) -> Maybe b -> T.Text
showMaxDistance = showMax' $ printf "%.3f"

showDistancePoints :: Maybe DistancePoints -> DistancePoints -> T.Text
showDistancePoints task (DistancePoints p) =
    showMaxDistance p (\(DistancePoints x) -> x) task

showLinearPoints :: Maybe LinearPoints -> LinearPoints -> T.Text
showLinearPoints task (LinearPoints p) =
    showMaxDistance p (\(LinearPoints x) -> x) task

showDifficultyPoints :: Maybe DifficultyPoints -> DifficultyPoints -> T.Text
showDifficultyPoints task (DifficultyPoints p) =
    showMaxDistance p (\(DifficultyPoints x) -> x) task

showArrivalPoints :: Maybe ArrivalPoints -> ArrivalPoints -> T.Text
showArrivalPoints task (ArrivalPoints p) =
    showMax p (\(ArrivalPoints x) -> x) task

showTimePoints :: Maybe TimePoints -> TimePoints -> T.Text
showTimePoints task (TimePoints p) =
    showMax p (\(TimePoints x) -> x) task

showLeadingPoints :: Maybe LeadingPoints -> LeadingPoints -> T.Text
showLeadingPoints task (LeadingPoints p) =
    showMax p (\(LeadingPoints x) -> x) task

showTaskPoints :: Maybe TaskPoints -> TaskPoints -> T.Text
showTaskPoints task (TaskPoints p) =
    showMaxRounded p (\(TaskPoints x) -> x) task

data Points =
    Points 
        { reach :: LinearPoints
        , effort :: DifficultyPoints
        , distance :: DistancePoints
        , leading :: LeadingPoints
        , arrival :: ArrivalPoints
        , time :: TimePoints
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

newtype ArrivalWeight = ArrivalWeight Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype DistanceWeight = DistanceWeight Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype LeadingWeight = LeadingWeight Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype TimeWeight = TimeWeight Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

pprWg :: Double -> String
pprWg = printf "%.2f%%" . (* 100.0)

showDistanceWeight :: DistanceWeight -> T.Text
showDistanceWeight (DistanceWeight p) = T.pack . pprWg $ p

showLeadingWeight :: LeadingWeight -> T.Text
showLeadingWeight (LeadingWeight p) = T.pack . pprWg $ p

showArrivalWeight :: ArrivalWeight -> T.Text
showArrivalWeight (ArrivalWeight p) = T.pack . pprWg $ p

showTimeWeight :: TimeWeight -> T.Text
showTimeWeight (TimeWeight p) = T.pack . pprWg $ p

zeroWeights :: Weights
zeroWeights =
    Weights
        { distance = DistanceWeight 0
        , leading = LeadingWeight 0
        , arrival = ArrivalWeight 0
        , time = TimeWeight 0
        }

data Weights =
    Weights
        { distance :: DistanceWeight
        , leading :: LeadingWeight
        , arrival :: ArrivalWeight
        , time :: TimeWeight
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

data Velocity =
    Velocity
        { ss :: Maybe UTCTime
          -- ^ The time the pilot crossed the start and started the speed
          -- section.
        , gs :: Maybe StartGate
          -- ^ The time the pilot was deemed to have started when there are
          -- start gates. This is the opening time of the start gate that the
          -- pilot took.
        , es :: Maybe UTCTime
        , ssElapsed :: Maybe PilotTime
          -- ^ The elapsed time from the moment the pilot crossed the start.
        , gsElapsed :: Maybe PilotTime
          -- ^ The elapsed time from the start gate. Always as long as
          -- @ssElapsed@.
        , ssDistance :: Maybe PilotDistance
          -- ^ The distance the pilot made, not exceeding goal.
        , ssVelocity :: Maybe (PilotVelocity String)
          -- ^ The velocity from the time the started the speed section.
        , gsVelocity :: Maybe (PilotVelocity String)
          -- ^ The velocity from the start gate time.
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

data NormBreakdown =
    NormBreakdown
        { place :: TaskPlacing
        , total :: TaskPoints
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

data Breakdown =
    Breakdown
        { place :: TaskPlacing
        , total :: TaskPoints
        , penalties :: [PointPenalty]
        , penaltyReason :: String
        , breakdown :: Points
        , velocity :: Maybe Velocity
        , reachDistance :: Maybe PilotDistance
        , landedDistance :: Maybe PilotDistance
        , stoppedAlt :: Maybe Alt
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

data Allocation =
    Allocation
        { goalRatio :: GoalRatio
        , weight :: Weights
        , points :: Points
        , taskPoints :: TaskPoints
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)
