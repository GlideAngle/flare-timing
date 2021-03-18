module Points.Round ((<&>), dpRoundTaskPoints, dpRoundPointsReduced) where

import qualified Data.Ratio.Rounding as Round (dpRound)
import "flight-gap-math" Flight.Score
    (TaskPoints, PointsReduced, onTaskPoints, onPointsReduced)
--
-- TODO: When base >= 4.11 use Data.Functor ((<&>))
(<&>) :: Either c a -> (a -> b) -> Either c b
(<&>) = flip (<$>)

dpRoundTaskPoints :: TaskPoints -> TaskPoints
dpRoundTaskPoints = onTaskPoints (Round.dpRound 8)

-- | Round before comparing point fields, so that 0.40000000000000036 => 0.4.
dpRoundPointsReduced :: PointsReduced -> PointsReduced
dpRoundPointsReduced = onPointsReduced (Round.dpRound 8)
