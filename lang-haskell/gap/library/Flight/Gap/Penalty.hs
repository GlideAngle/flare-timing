module Flight.Gap.Penalty
    ( PointPenalty(..)
    , applyPenalties
    , applyFractionalPenalties
    , applyPointPenalties
    , applyResetPenalties
    ) where

import Data.List (partition, foldl')
import GHC.Generics (Generic)
import Data.Aeson
    ( ToJSON(..), FromJSON(..), Options(..), SumEncoding(..)
    , genericToJSON, genericParseJSON, defaultOptions
    )

import Flight.Gap.Points.Task (TaskPoints(..))

data PointPenalty
    = PenaltyPoints Double
    | PenaltyFraction Double
    | PenaltyReset Int
    deriving (Eq, Ord, Show, Generic)

-- | Applies the penalties, fractionals then absolutes and finally the resets.
applyPenalties :: [PointPenalty] -> TaskPoints -> TaskPoints
applyPenalties xs ps =
    f (f (f ps fracs) points) resets
    where
        f = foldl' applyPenalty
        (fracs, ys) = partition isPenaltyFraction xs
        (resets, points) = partition isPenaltyReset ys

isPenaltyPoints, isPenaltyFraction, isPenaltyReset :: PointPenalty -> Bool
isPenaltyPoints = \case PenaltyPoints{} -> True; _ -> False
isPenaltyFraction = \case PenaltyFraction{} -> True; _ -> False
isPenaltyReset = \case PenaltyReset{} -> True; _ -> False

applyPenalty :: TaskPoints -> PointPenalty -> TaskPoints
applyPenalty p (PenaltyPoints n) = max 0 $ p - TaskPoints n
applyPenalty p (PenaltyFraction n) = max 0 $ p - p * TaskPoints n
applyPenalty p (PenaltyReset n) = max 0 $ min p (TaskPoints $ fromIntegral n)

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

