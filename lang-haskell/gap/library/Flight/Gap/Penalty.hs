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

isPenaltyPoints :: PointPenalty -> Bool
isPenaltyPoints = \case
    PenaltyFraction _ -> False
    PenaltyPoints _ -> True
    PenaltyReset _ -> False

isPenaltyFraction :: PointPenalty -> Bool
isPenaltyFraction = \case
    PenaltyFraction _ -> True
    PenaltyPoints _ -> False
    PenaltyReset _ -> False

isPenaltyReset :: PointPenalty -> Bool
isPenaltyReset = \case
    PenaltyFraction _ -> False
    PenaltyPoints _ -> False
    PenaltyReset _ -> True

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

-- | Applies the penalties, fractional ones before absolute ones and finally
-- the reset ones.
applyPenalties :: [PointPenalty] -> TaskPoints -> TaskPoints
applyPenalties xs ps =
    foldl' applyPenalty (foldl' applyPenalty (foldl' applyPenalty ps fracs) points) resets
    where
        (fracs, ys) = partition isPenaltyFraction xs
        (resets, points) = partition isPenaltyReset ys

applyPenalty :: TaskPoints -> PointPenalty -> TaskPoints
applyPenalty (TaskPoints p) (PenaltyPoints n) =
    TaskPoints . max 0 $ p - (toRational n)
applyPenalty (TaskPoints p) (PenaltyFraction n) =
    TaskPoints . max 0 $ p - p * (toRational n)
applyPenalty (TaskPoints p) (PenaltyReset n) =
    TaskPoints . max 0 . min p $ fromIntegral n
