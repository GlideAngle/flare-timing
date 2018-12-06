{-# LANGUAGE DuplicateRecordFields #-}

module WireTypes.Point
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
    -- * Showing Validities
    , showLaunchValidity
    , showDistanceValidity
    , showTimeValidity
    , showTaskValidity
    ) where

import Text.Printf (printf)
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

pprVy :: Double -> String
pprVy = printf "%.3f"

showLaunchValidity :: LaunchValidity -> T.Text
showLaunchValidity (LaunchValidity v) = T.pack . pprVy $ v

showDistanceValidity :: DistanceValidity -> T.Text
showDistanceValidity (DistanceValidity v) = T.pack . pprVy $ v

showTimeValidity :: TimeValidity -> T.Text
showTimeValidity (TimeValidity v) = T.pack . pprVy $ v

showTaskValidity :: TaskValidity -> T.Text
showTaskValidity (TaskValidity v) = T.pack . pprVy $ v

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

showDistancePoints :: Maybe DistancePoints -> DistancePoints -> T.Text
showDistancePoints task (DistancePoints p) =
    showMax p (\(DistancePoints x) -> x) task

showLinearPoints :: LinearPoints -> T.Text
showLinearPoints (LinearPoints p) = T.pack . show $ p

showDifficultyPoints :: DifficultyPoints -> T.Text
showDifficultyPoints (DifficultyPoints p) = T.pack . show $ p

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
