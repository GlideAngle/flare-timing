module Flight.Gap.Penalty
    ( PointPenalty(..)
    , TooEarlyPoints(..)
    , LaunchToStartPoints(..)
    , applyPenalties
    , applyFractionalPenalties
    , applyPointPenalties
    , applyResetPenalties
    ) where

import Numeric.Natural (Natural)
import Data.List (partition, foldl')
import GHC.Generics (Generic)
import Data.Aeson
    ( ToJSON(..), FromJSON(..), Options(..), SumEncoding(..)
    , genericToJSON, genericParseJSON, defaultOptions
    )

import Flight.Gap.Points.Task (TaskPoints(..))

-- NOTE: Reset points are the final points awarded and so can be ints.
data LaunchToStartPoints = LaunchToStartPoints !Natural
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data TooEarlyPoints = TooEarlyPoints !Natural
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data PointPenalty
    = PenaltyPoints Double
    -- ^ If positive then remove this number of points and if negative add this
    -- number of points.
    | PenaltyFraction Double
    -- ^ If positive then remove this fraction of points and if negative add this
    -- fraction of points.
    | PenaltyReset Natural
    -- ^ Reset points down to this natural number.
    deriving (Eq, Ord, Show, Generic)

-- | Applies the penalties, fractionals then absolutes and finally the resets.
applyPenalties :: [PointPenalty] -> TaskPoints -> TaskPoints
applyPenalties xs p =
    f (f (f p fracs) points) resets
    where
        f = foldl' applyPenalty
        (fracs, ys) = partition isPenaltyFraction xs
        (resets, points) = partition isPenaltyReset ys

zP :: PointPenalty -> Maybe (Ordering, TaskPoints)
zP (PenaltyPoints n) = Just (n `compare` 0, TaskPoints $ abs n)
zP _ = Nothing

zF :: PointPenalty -> Maybe (Ordering, TaskPoints)
zF (PenaltyFraction n) = Just (n `compare` 0, TaskPoints $ abs n)
zF _ = Nothing

applyPenalty :: TaskPoints -> PointPenalty -> TaskPoints
applyPenalty p pp

    | Just (EQ, _) <- zP pp = p
    | Just (GT, n) <- zP pp = max 0 $ p - n
    | Just (LT, n) <- zP pp = p + n

    | Just (EQ, _) <- zF pp = p
    | Just (GT, n) <- zF pp = max 0 $ p * (1.0 - n)
    | Just (LT, n) <- zF pp = p * (1.0 + n)

    | PenaltyReset n <- pp =
        -- NOTE: Resets can only be used as penalties, not bonuses.
        min p (TaskPoints $ fromIntegral n)

    | otherwise = p

isPenaltyPoints, isPenaltyFraction, isPenaltyReset :: PointPenalty -> Bool
isPenaltyPoints = \case PenaltyPoints{} -> True; _ -> False
isPenaltyFraction = \case PenaltyFraction{} -> True; _ -> False
isPenaltyReset = \case PenaltyReset{} -> True; _ -> False

pointPenaltyOptions :: Options
pointPenaltyOptions =
    defaultOptions
        { sumEncoding = ObjectWithSingleField
        , constructorTagModifier = \case
            "PenaltyPoints" -> "penalty-points"
            "PenaltyFraction" -> "penalty-fraction"
            "PenaltyReset" -> "penalty-reset"
            s -> s
        }

instance ToJSON PointPenalty where
    toJSON = genericToJSON pointPenaltyOptions

instance FromJSON PointPenalty where
    parseJSON = genericParseJSON pointPenaltyOptions

-- | Applies only fractional penalties.
applyFractionalPenalties :: [PointPenalty] -> TaskPoints -> TaskPoints
applyFractionalPenalties xs ps =
    foldl' applyPenalty ps fracs
    where
        (fracs, _) = partition isPenaltyFraction xs

-- | Applies only point penalties.
applyPointPenalties :: [PointPenalty] -> TaskPoints -> TaskPoints
applyPointPenalties xs ps =
    foldl' applyPenalty ps points
    where
        (points, _) = partition isPenaltyPoints xs

-- | Applies only reset penalties.
applyResetPenalties :: [PointPenalty] -> TaskPoints -> TaskPoints
applyResetPenalties xs ps =
    foldl' applyPenalty ps resets
    where
        (resets, _) = partition isPenaltyReset xs

