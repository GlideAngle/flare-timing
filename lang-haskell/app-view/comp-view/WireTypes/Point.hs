{-# LANGUAGE DuplicateRecordFields #-}

module WireTypes.Point
    ( StartGate(..)
    , Velocity(..)
    , AltBreakdown(..)
    , Breakdown(..)
    , Allocation(..)
    , GoalRatio(..)
    , EssNotGoal(..)
    , PilotDistance(..)
    , JumpedTheGun(..)
    , Alt(..)
    , PilotVelocity(..)
    , DistancePoints(..)
    , LinearPoints(..)
    , DifficultyPoints(..)
    , ArrivalPoints(..)
    , TimePoints(..)
    , LeadingPoints(..)
    , TaskPlacing(..)
    , TaskPoints(..)
    , Points(..)
    , DistanceWeight(..)
    , LeadingWeight(..)
    , ArrivalWeight(..)
    , TimeWeight(..)
    , Weights(..)
    , PointPenalty(..)
    , ReachToggle(..)
    -- * Showing Breakdown
    , showPilotDistance
    , showPilotDistanceDiff
    , showPilotAlt
    , showJumpedTheGunTime
    -- * Showing Points
    , showTaskDistancePoints
    , showTaskLinearPoints
    , showTaskDifficultyPoints
    , showTaskArrivalPoints
    , showTaskTimePoints
    , showTaskTimeArrivalPoints
    , showTaskLeadingPoints
    , showTaskPoints
    , showTaskPointsRounded
    , showTaskPointsNonZero
    , showTaskPointsDiffStats
    , showDemeritPointsNonZero
    , showRounded
    , showTaskPointsDiff
    , showLinearPoints
    , showDifficultyPoints
    , showDistancePoints
    , showLeadingPoints
    , showArrivalPoints
    , showTimePoints
    , showLinearPointsDiff
    , showDifficultyPointsDiff
    , showDistancePointsDiff
    , showLeadingPointsDiff
    , showArrivalPointsDiff
    , showTimePointsDiff
    -- * Showing Weights
    , showDistanceWeight
    , showArrivalWeight
    , showTimeWeight
    , showLeadingWeight
    , zeroWeights
    -- * Comparisons
    , cmpReach
    , cmpEffort
    , cmpTime
    , cmpArrival
    ) where

import Text.Printf (printf)
import Data.Ord (comparing)
import Control.Applicative (empty)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (Value(..), FromJSON(..))
import qualified Data.Text as T (Text, pack, unpack)

import WireTypes.Speed (PilotTime)
import WireTypes.Lead (LeadingArea, LeadingCoefficient)
import WireTypes.Fraction (Fractions)
import WireTypes.Penalty
import qualified FlareTiming.Statistics as Stats
import FlareTiming.Time (showHmsForSecs)

newtype StartGate = StartGate UTCTime
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype GoalRatio = GoalRatio Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype EssNotGoal = EssNotGoal Bool
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype PilotDistance = PilotDistance Double
    deriving (Eq, Ord, Show, Generic)

newtype PilotVelocity = PilotVelocity Double
    deriving (Eq, Ord, Show, Generic)

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

newtype JumpedTheGun = JumpedTheGun Double
    deriving (Eq, Ord, Show, Generic)

newtype Alt = Alt Double
    deriving (Eq, Ord, Show, Generic)

instance FromJSON JumpedTheGun where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            's' : ' ' : xs -> return . JumpedTheGun . read . reverse $ xs
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

instance FromJSON PilotVelocity where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'h' : ' ' : '/' : ' ' : 'm' : 'k' : ' ' : xs -> return . PilotVelocity . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

showPilotDistance :: Int -> PilotDistance -> T.Text
showPilotDistance dp (PilotDistance d) =
    T.pack $ printf "%.*f" dp d

showPilotDistanceDiff :: Int -> PilotDistance -> PilotDistance -> T.Text
showPilotDistanceDiff dp (PilotDistance expected) (PilotDistance actual)
    | f actual == f expected = "="
    | dp > 0 && (filter (not . (flip elem) ['.', '+', '-', '0']) $ f (actual - expected)) == "" =
        T.pack $ printf "%+.*f" (dp + 1) (actual - expected)
    | otherwise = g (actual - expected)
    where
        f :: Double -> String
        f = printf "%+.*f" dp

        g = T.pack . f

showJumpedTheGunTime :: Maybe JumpedTheGun -> T.Text
showJumpedTheGunTime Nothing = ""
showJumpedTheGunTime (Just (JumpedTheGun s)) = showHmsForSecs s

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

showRounded :: (Show a, RealFrac a) => a -> T.Text
showRounded = T.pack . show . (\x -> round x :: Integer)

showMax
    :: (Show a, RealFrac a)
    => a
    -> (b -> a)
    -> Maybe b
    -> T.Text
showMax = showMax' show

showMaxDistance :: Double -> (b -> Double) -> Maybe b -> T.Text
showMaxDistance = showMax' $ printf "%.1f"

showTaskDistancePoints :: Maybe DistancePoints -> DistancePoints -> T.Text
showTaskDistancePoints task (DistancePoints p) =
    showMaxDistance p (\(DistancePoints x) -> x) task

showTaskLinearPoints :: Maybe LinearPoints -> LinearPoints -> T.Text
showTaskLinearPoints task (LinearPoints p) =
    showMaxDistance p (\(LinearPoints x) -> x) task

showTaskDifficultyPoints :: Maybe DifficultyPoints -> DifficultyPoints -> T.Text
showTaskDifficultyPoints task (DifficultyPoints p) =
    showMaxDistance p (\(DifficultyPoints x) -> x) task

showTaskArrivalPoints :: Maybe ArrivalPoints -> ArrivalPoints -> T.Text
showTaskArrivalPoints task (ArrivalPoints p) =
    showMax p (\(ArrivalPoints x) -> x) task

showTaskTimePoints :: Maybe TimePoints -> TimePoints -> T.Text
showTaskTimePoints task (TimePoints p) =
    showMax p (\(TimePoints x) -> x) task

showTaskTimeArrivalPoints
    :: (Maybe (TimePoints, ArrivalPoints))
    -> (TimePoints, ArrivalPoints)
    -> T.Text
showTaskTimeArrivalPoints task (TimePoints tp, ArrivalPoints ap) =
    showMax'
        (printf "%.1f")
        (tp + ap)
        (\(TimePoints tTask, ArrivalPoints aTask) -> tTask + aTask)
        task

showTaskLeadingPoints :: Maybe LeadingPoints -> LeadingPoints -> T.Text
showTaskLeadingPoints task (LeadingPoints p) =
    showMax p (\(LeadingPoints x) -> x) task

showTaskPoints :: Int -> TaskPoints -> T.Text
showTaskPoints dp (TaskPoints p) =
    T.pack $ printf "%.*f" dp p

showTaskPointsRounded :: Maybe TaskPoints -> TaskPoints -> T.Text
showTaskPointsRounded task (TaskPoints p) =
    showMaxRounded p (\(TaskPoints x) -> x) task

showTaskPointsNonZero :: Int -> TaskPoints -> T.Text
showTaskPointsNonZero dp tp
    | tp == TaskPoints 0 = ""
    | otherwise = showTaskPoints dp tp

showDemeritPointsNonZero :: Int -> TaskPoints -> T.Text
showDemeritPointsNonZero dp (TaskPoints p)
    | p == 0 = ""
    | otherwise = showTaskPoints dp (TaskPoints $ negate p)

showTaskPointsDiffStats :: Maybe [TaskPoints] -> [TaskPoints] -> T.Text
showTaskPointsDiffStats es ps =
    T.pack $
        maybe
            ""
            (\es' ->
                let xs = zipWith (\(TaskPoints p) (TaskPoints e) -> p - e) ps es'
                in printf "Δ = %.1f ± %.1f" (Stats.mean xs) (Stats.stdDev xs))
            es

showLinearPoints :: LinearPoints -> T.Text
showLinearPoints (LinearPoints p) = T.pack $ printf "%.1f" p

showDifficultyPoints :: DifficultyPoints -> T.Text
showDifficultyPoints (DifficultyPoints p) = T.pack $ printf "%.1f" p

showDistancePoints :: DistancePoints -> T.Text
showDistancePoints (DistancePoints p) = T.pack $ printf "%.1f" p

showLeadingPoints :: LeadingPoints -> T.Text
showLeadingPoints (LeadingPoints p) = T.pack $ printf "%.1f" p

showArrivalPoints :: ArrivalPoints -> T.Text
showArrivalPoints (ArrivalPoints p) = T.pack $ printf "%.1f" p

showTimePoints :: TimePoints -> T.Text
showTimePoints (TimePoints p) = T.pack $ printf "%.1f" p

showPointsDiff :: Int -> Double -> Double -> T.Text
showPointsDiff dp expected actual
    | f dp actual == f dp expected = "="
    | (filter (not . (flip elem) ['.', '+', '-', '0']) $ f dp (actual - expected)) == "" =
        T.pack . f (dp + 2) $ actual - expected
    | otherwise = T.pack . f dp $ actual - expected
    where
        f :: Int -> Double -> String
        f = printf "%+.*f"

showLinearPointsDiff :: LinearPoints -> LinearPoints -> T.Text
showLinearPointsDiff (LinearPoints expected) (LinearPoints actual) =
    showPointsDiff 1 expected actual

showDifficultyPointsDiff :: DifficultyPoints -> DifficultyPoints -> T.Text
showDifficultyPointsDiff (DifficultyPoints expected) (DifficultyPoints actual) =
    showPointsDiff 1 expected actual

showDistancePointsDiff :: DistancePoints -> DistancePoints -> T.Text
showDistancePointsDiff (DistancePoints expected) (DistancePoints actual) =
    showPointsDiff 1 expected actual

showLeadingPointsDiff :: LeadingPoints -> LeadingPoints -> T.Text
showLeadingPointsDiff (LeadingPoints expected) (LeadingPoints actual) =
    showPointsDiff 1 expected actual

showArrivalPointsDiff :: ArrivalPoints -> ArrivalPoints -> T.Text
showArrivalPointsDiff (ArrivalPoints expected) (ArrivalPoints actual) =
    showPointsDiff 1 expected actual

showTimePointsDiff :: TimePoints -> TimePoints -> T.Text
showTimePointsDiff (TimePoints expected) (TimePoints actual) =
    showPointsDiff 1 expected actual

showTaskPointsDiff :: TaskPoints -> TaskPoints -> T.Text
showTaskPointsDiff (TaskPoints expected) (TaskPoints actual) =
    showPointsDiff 0 expected actual

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
        , ssVelocity :: Maybe PilotVelocity
          -- ^ The velocity from the time the started the speed section.
        , gsVelocity :: Maybe PilotVelocity
          -- ^ The velocity from the start gate time.
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

data ReachToggle a =
    ReachToggle
        { extra :: a
        , flown :: a
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

data AltBreakdown =
    AltBreakdown
        { place :: TaskPlacing
        , total :: TaskPoints
        , breakdown :: Points
        , fractions :: Fractions
        , reach :: ReachToggle (Maybe PilotDistance)
        -- ^ Most pilots have reach but some get nulls from airScore.
        , landedMade :: Maybe PilotDistance
        -- ^ Reported by FS but not by airScore. Not actually used in
        -- calculating points in GAP.
        , ss :: Maybe UTCTime
        , es :: Maybe UTCTime
        , timeElapsed :: Maybe PilotTime
        , leadingArea :: Maybe LeadingArea
        -- ^ Reported by FS but not by airScore.
        , leadingCoef :: LeadingCoefficient
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

data Breakdown =
    Breakdown
        { place :: TaskPlacing
        , subtotal :: TaskPoints
        , demeritFrac :: TaskPoints
        , demeritPoint :: TaskPoints
        , demeritReset :: TaskPoints
        , total :: TaskPoints
        , essNotGoal :: Maybe EssNotGoal
        , penaltiesEssNotGoal :: PenaltySeqs
        , jump :: Maybe JumpedTheGun
        , penaltiesJumpRaw :: Maybe PenaltySeqs
        , penaltiesJumpEffective :: PenaltySeqs
        , penalties :: PenaltySeqs
        , penaltyReason :: String
        , breakdown :: Points
        , velocity :: Maybe Velocity
        , reach :: Maybe (ReachToggle PilotDistance)
        , landedMade :: Maybe PilotDistance
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

cmpReach :: (a, Breakdown) -> (a, Breakdown) -> Ordering
cmpReach = cmpBreakdownPoints (reach :: Points -> LinearPoints)

cmpEffort :: (a, Breakdown) -> (a, Breakdown) -> Ordering
cmpEffort = cmpBreakdownPoints (effort :: Points -> DifficultyPoints)

cmpTime :: (a, Breakdown) -> (a, Breakdown) -> Ordering
cmpTime = cmpBreakdownPoints (time :: Points -> TimePoints)

cmpArrival :: (a, Breakdown) -> (a, Breakdown) -> Ordering
cmpArrival = cmpBreakdownPoints (arrival :: Points -> ArrivalPoints)

-- SEE: https://stackoverflow.com/questions/2349798/in-haskell-how-can-i-use-the-built-in-sortby-function-to-sort-a-list-of-pairst
cmpBreakdownPoints
    :: Ord b
    => (Points -> b)
    -> (a, Breakdown)
    -> (a, Breakdown)
    -> Ordering
cmpBreakdownPoints f =
    flip (comparing (f . breakdown' . snd)) `mappend` comparing (place' . snd)
    where
        breakdown' :: Breakdown -> Points
        breakdown' = breakdown

        place' :: Breakdown -> TaskPlacing
        place' = place
