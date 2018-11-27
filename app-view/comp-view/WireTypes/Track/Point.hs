{-# LANGUAGE DuplicateRecordFields #-}

module WireTypes.Track.Point
    ( Velocity(..)
    , Breakdown(..)
    , Allocation(..)
    , Validity(..)
    , TaskValidity(..)
    , LaunchValidity(..)
    , DistanceValidity(..)
    , TimeValidity(..)
    , GoalRatio(..)
    , PilotDistance(..)
    , PilotTime(..)
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
    , showDistancePoints
    , showLinearPoints
    , showDifficultyPoints
    , showArrivalPoints
    , showTimePoints
    , showLeadingPoints
    , showTaskPoints
    ) where

import Control.Applicative (empty)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (Value(..), ToJSON(..), FromJSON(..))
import qualified Data.Text as T (Text, pack, unpack)

newtype StartGate = StartGate UTCTime
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype GoalRatio = GoalRatio Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype PilotTime a = PilotTime a
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype PilotDistance a = PilotDistance a
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype PilotVelocity a = PilotVelocity a
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype NominalGoal = NominalGoal Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype TaskValidity = TaskValidity Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype LaunchValidity = LaunchValidity Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype DistanceValidity = DistanceValidity Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype TimeValidity = TimeValidity Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Validity =
    Validity 
        { task :: TaskValidity
        , launch :: LaunchValidity
        , distance :: DistanceValidity
        , time :: TimeValidity
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

newtype ArrivalPoints = ArrivalPoints Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype DistancePoints = DistancePoints Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype LinearPoints = LinearPoints Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype DifficultyPoints = DifficultyPoints Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype LeadingPoints = LeadingPoints Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype TimePoints = TimePoints Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data TaskPlacing
    = TaskPlacing Integer
    | TaskPlacingEqual Integer
    deriving (Eq, Ord, Show)

instance ToJSON TaskPlacing where
    toJSON (TaskPlacing x) = String . T.pack $ show x
    toJSON (TaskPlacingEqual x) = String . T.pack $ show x ++ "="

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
    deriving anyclass (ToJSON, FromJSON)

showDistancePoints :: Maybe DistancePoints -> DistancePoints -> T.Text
showDistancePoints task (DistancePoints p) =
    T.pack . maybe id f task $ x
    where
        x = show (truncate p :: Integer)
        f (DistancePoints task')
            | task' == p = \s -> "*" ++ s
            | otherwise = id

showLinearPoints :: LinearPoints -> T.Text
showLinearPoints (LinearPoints p) = T.pack . show $ p

showDifficultyPoints :: DifficultyPoints -> T.Text
showDifficultyPoints (DifficultyPoints p) = T.pack . show $ p

showArrivalPoints :: Maybe ArrivalPoints -> ArrivalPoints -> T.Text
showArrivalPoints task (ArrivalPoints p) =
    T.pack . maybe id f task $ x
    where
        x = show (truncate p :: Integer)
        f (ArrivalPoints task')
            | task' == p = \s -> "*" ++ s
            | otherwise = id

showTimePoints :: Maybe TimePoints -> TimePoints -> T.Text
showTimePoints task (TimePoints p) =
    T.pack . maybe id f task $ x
    where
        x = show (truncate p :: Integer)
        f (TimePoints task')
            | task' == p = \s -> "*" ++ s
            | otherwise = id

showLeadingPoints :: Maybe LeadingPoints -> LeadingPoints -> T.Text
showLeadingPoints task (LeadingPoints p) =
    T.pack . maybe id f task $ x
    where
        x = show (truncate p :: Integer)
        f (LeadingPoints task')
            | task' == p = \s -> "*" ++ s
            | otherwise = id

showTaskPoints :: Maybe TaskPoints -> TaskPoints -> T.Text
showTaskPoints task (TaskPoints p) =
    T.pack . maybe id f task $ x
    where
        x = show (truncate p :: Integer)
        f (TaskPoints task')
            | task' == p = \s -> "*" ++ s
            | otherwise = id

data Points =
    Points 
        { reach :: LinearPoints
        , effort :: DifficultyPoints
        , distance :: DistancePoints
        , leading :: LeadingPoints
        , arrival :: ArrivalPoints
        , time :: TimePoints
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

newtype ArrivalWeight = ArrivalWeight Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype DistanceWeight = DistanceWeight Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype LeadingWeight = LeadingWeight Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype TimeWeight = TimeWeight Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Weights =
    Weights
        { distance :: DistanceWeight
        , leading :: LeadingWeight
        , arrival :: ArrivalWeight
        , time :: TimeWeight
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

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
        , ssElapsed :: Maybe (PilotTime String)
          -- ^ The elapsed time from the moment the pilot crossed the start.
        , gsElapsed :: Maybe (PilotTime String)
          -- ^ The elapsed time from the start gate. Always as long as
          -- @ssElapsed@.
        , distance :: Maybe (PilotDistance String)
          -- ^ The distance the pilot made, not exceeding goal.
        , ssVelocity :: Maybe (PilotVelocity String)
          -- ^ The velocity from the time the started the speed section.
        , gsVelocity :: Maybe (PilotVelocity String)
          -- ^ The velocity from the start gate time.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Breakdown =
    Breakdown
        { place :: TaskPlacing
        , total :: TaskPoints
        , breakdown :: Points
        , velocity :: Velocity
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Allocation =
    Allocation 
        { goalRatio :: GoalRatio
        , weight :: Weights
        , points :: Points
        , taskPoints :: TaskPoints
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)
