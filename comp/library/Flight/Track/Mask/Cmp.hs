{-|
Module      : Flight.Track.Mask.Speed
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Tracks masked with task control zones.
-}
module Flight.Track.Mask.Cmp (cmp) where

import Data.String (IsString())

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        -- TODO: first start time & last goal time & launched
        ("openTask", _) -> LT

        ("closeTask", "openTask") -> GT
        ("closeTask", _) -> LT

        ("firstStart", "openTask") -> GT
        ("firstStart", "closeTask") -> GT
        ("firstStart", _) -> LT

        ("lastArrival", "openTask") -> GT
        ("lastArrival", "closeTask") -> GT
        ("lastArrival", "firstStart") -> GT
        ("lastArrival", _) -> LT

        ("tickArrival", "openTask") -> GT
        ("tickArrival", "closeTask") -> GT
        ("tickArrival", "firstStart") -> GT
        ("tickArrival", "lastArrival") -> GT
        ("tickArrival", _) -> LT

        ("tickRace", "openTask") -> GT
        ("tickRace", "closeTask") -> GT
        ("tickRace", "firstStart") -> GT
        ("tickRace", "lastArrival") -> GT
        ("tickRace", "tickArrival") -> GT
        ("tickRace", _) -> LT

        ("tickTask", _) -> GT

        ("pilotsAtEss", _) -> LT

        ("raceTime", "pilotsAtEss") -> GT
        ("raceTime", _) -> LT

        ("best", _) -> LT
        ("last", _) -> GT

        ("ssBestTime", "pilotsAtEss") -> GT
        ("ssBestTime", "raceTime") -> GT
        ("ssBestTime", _) -> LT

        ("gsBestTime", "pilotsAtEss") -> GT
        ("gsBestTime", "raceTime") -> GT
        ("gsBestTime", "ssBestTime") -> GT
        ("gsBestTime", _) -> LT

        ("taskDistance", "pilotsAtEss") -> GT
        ("taskDistance", "raceTime") -> GT
        ("taskDistance", "ssBestTime") -> GT
        ("taskDistance", "gsBestTime") -> GT
        ("taskDistance", _) -> LT

        ("taskSpeedDistance", "pilotsAtEss") -> GT
        ("taskSpeedDistance", "raceTime") -> GT
        ("taskSpeedDistance", "ssBestTime") -> GT
        ("taskSpeedDistance", "gsBestTime") -> GT
        ("taskSpeedDistance", "taskDistance") -> GT
        ("taskSpeedDistance", _) -> LT

        ("bestEffort", "pilotsAtEss") -> GT
        ("bestEffort", "raceTime") -> GT
        ("bestEffort", "ssBestTime") -> GT
        ("bestEffort", "gsBestTime") -> GT
        ("bestEffort", "taskDistance") -> GT
        ("bestEffort", "taskSpeedDistance") -> GT
        ("bestEffort", _) -> LT

        ("bestReach", "pilotsAtEss") -> GT
        ("bestReach", "raceTime") -> GT
        ("bestReach", "ssBestTime") -> GT
        ("bestReach", "gsBestTime") -> GT
        ("bestReach", "taskDistance") -> GT
        ("bestReach", "taskSpeedDistance") -> GT
        ("bestReach", _) -> LT

        ("sumDistance", "pilotsAtEss") -> GT
        ("sumDistance", "raceTime") -> GT
        ("sumDistance", "ssBestTime") -> GT
        ("sumDistance", "gsBestTime") -> GT
        ("sumDistance", "taskDistance") -> GT
        ("sumDistance", "taskSpeedDistance") -> GT
        ("sumDistance", "bestEffort") -> GT
        ("sumDistance", "bestReach") -> GT
        ("sumDistance", _) -> LT

        ("leadAreaToCoef", "pilotsAtEss") -> GT
        ("leadAreaToCoef", "raceTime") -> GT
        ("leadAreaToCoef", "ssBestTime") -> GT
        ("leadAreaToCoef", "gsBestTime") -> GT
        ("leadAreaToCoef", "taskDistance") -> GT
        ("leadAreaToCoef", "taskSpeedDistance") -> GT
        ("leadAreaToCoef", "bestEffort") -> GT
        ("leadAreaToCoef", "bestReach") -> GT
        ("leadAreaToCoef", "sumDistance") -> GT
        ("leadAreaToCoef", _) -> LT

        ("leadCoefMin", "pilotsAtEss") -> GT
        ("leadCoefMin", "raceTime") -> GT
        ("leadCoefMin", "ssBestTime") -> GT
        ("leadCoefMin", "gsBestTime") -> GT
        ("leadCoefMin", "taskDistance") -> GT
        ("leadCoefMin", "taskSpeedDistance") -> GT
        ("leadCoefMin", "bestEffort") -> GT
        ("leadCoefMin", "bestReach") -> GT
        ("leadCoefMin", "sumDistance") -> GT
        ("leadCoefMin", "leadAreaToCoef") -> GT
        ("leadCoefMin", _) -> LT

        ("leadRank", "pilotsAtEss") -> GT
        ("leadRank", "raceTime") -> GT
        ("leadRank", "ssBestTime") -> GT
        ("leadRank", "gsBestTime") -> GT
        ("leadRank", "taskDistance") -> GT
        ("leadRank", "bestEffort") -> GT
        ("leadRank", "bestReach") -> GT
        ("leadRank", "sumDistance") -> GT
        ("leadRank", "leadRankScaling") -> GT
        ("leadRank", "leadRankCoefMin") -> GT
        ("leadRank", _) -> LT

        ("arrivalRank", "pilotsAtEss") -> GT
        ("arrivalRank", "raceTime") -> GT
        ("arrivalRank", "ssBestTime") -> GT
        ("arrivalRank", "gsBestTime") -> GT
        ("arrivalRank", "taskDistance") -> GT
        ("arrivalRank", "bestEffort") -> GT
        ("arrivalRank", "bestReach") -> GT
        ("arrivalRank", "sumDistance") -> GT
        ("arrivalRank", "leadAreaToCoef") -> GT
        ("arrivalRank", "leadCoefMin") -> GT
        ("arrivalRank", "lead") -> GT
        ("arrivalRank", _) -> LT

        ("flowMean", "pilotsAtEss") -> GT
        ("flowMean", "raceTime") -> GT
        ("flowMean", "ssBestTime") -> GT
        ("flowMean", "gsBestTime") -> GT
        ("flowMean", "taskDistance") -> GT
        ("flowMean", "bestEffort") -> GT
        ("flowMean", "bestReach") -> GT
        ("flowMean", "sumDistance") -> GT
        ("flowMean", "leadAreaToCoef") -> GT
        ("flowMean", "leadCoefMin") -> GT
        ("flowMean", "leadRank") -> GT
        ("flowMean", "arrivalRank") -> GT
        ("flowMean", _) -> LT

        ("flownStdDev", "pilotsAtEss") -> GT
        ("flownStdDev", "raceTime") -> GT
        ("flownStdDev", "ssBestTime") -> GT
        ("flownStdDev", "gsBestTime") -> GT
        ("flownStdDev", "taskDistance") -> GT
        ("flownStdDev", "bestEffort") -> GT
        ("flownStdDev", "bestReach") -> GT
        ("flownStdDev", "sumDistance") -> GT
        ("flownStdDev", "leadAreaToCoef") -> GT
        ("flownStdDev", "leadCoefMin") -> GT
        ("flownStdDev", "leadRank") -> GT
        ("flownStdDev", "arrivalRank") -> GT
        ("flownStdDev", "flownMean") -> GT
        ("flownStdDev", _) -> LT

        ("reachMean", "pilotsAtEss") -> GT
        ("reachMean", "raceTime") -> GT
        ("reachMean", "ssBestTime") -> GT
        ("reachMean", "gsBestTime") -> GT
        ("reachMean", "taskDistance") -> GT
        ("reachMean", "bestEffort") -> GT
        ("reachMean", "bestReach") -> GT
        ("reachMean", "sumDistance") -> GT
        ("reachMean", "leadAreaToCoef") -> GT
        ("reachMean", "leadCoefMin") -> GT
        ("reachMean", "leadRank") -> GT
        ("reachMean", "arrivalRank") -> GT
        ("reachMean", "flownMean") -> GT
        ("reachMean", "flownStdDev") -> GT
        ("reachMean", _) -> LT

        ("reachStdDev", "pilotsAtEss") -> GT
        ("reachStdDev", "raceTime") -> GT
        ("reachStdDev", "ssBestTime") -> GT
        ("reachStdDev", "gsBestTime") -> GT
        ("reachStdDev", "taskDistance") -> GT
        ("reachStdDev", "bestEffort") -> GT
        ("reachStdDev", "bestReach") -> GT
        ("reachStdDev", "sumDistance") -> GT
        ("reachStdDev", "leadAreaToCoef") -> GT
        ("reachStdDev", "leadCoefMin") -> GT
        ("reachStdDev", "leadRank") -> GT
        ("reachStdDev", "arrivalRank") -> GT
        ("reachStdDev", "flownMean") -> GT
        ("reachStdDev", "flownStdDev") -> GT
        ("reachStdDev", "reachMean") -> GT
        ("reachStdDev", _) -> LT

        ("reachRank", "pilotsAtEss") -> GT
        ("reachRank", "raceTime") -> GT
        ("reachRank", "ssBestTime") -> GT
        ("reachRank", "gsBestTime") -> GT
        ("reachRank", "taskDistance") -> GT
        ("reachRank", "bestEffort") -> GT
        ("reachRank", "bestReach") -> GT
        ("reachRank", "sumDistance") -> GT
        ("reachRank", "leadAreaToCoef") -> GT
        ("reachRank", "leadCoefMin") -> GT
        ("reachRank", "leadRank") -> GT
        ("reachRank", "arrivalRank") -> GT
        ("reachRank", "flownMean") -> GT
        ("reachRank", "flownStdDev") -> GT
        ("reachRank", "reachMean") -> GT
        ("reachRank", "reachStdDev") -> GT
        ("reachRank", _) -> LT

        ("ssSpeed", "pilotsAtEss") -> GT
        ("ssSpeed", "raceTime") -> GT
        ("ssSpeed", "ssBestTime") -> GT
        ("ssSpeed", "gsBestTime") -> GT
        ("ssSpeed", "taskDistance") -> GT
        ("ssSpeed", "taskSpeedDistance") -> GT
        ("ssSpeed", "bestEffort") -> GT
        ("ssSpeed", "bestReach") -> GT
        ("ssSpeed", "sumDistance") -> GT
        ("ssSpeed", "leadAreaToCoef") -> GT
        ("ssSpeed", "leadCoefMin") -> GT
        ("ssSpeed", "leadRank") -> GT
        ("ssSpeed", "arrivalRank") -> GT
        ("ssSpeed", "flownMean") -> GT
        ("ssSpeed", "flownStdDev") -> GT
        ("ssSpeed", "reachMean") -> GT
        ("ssSpeed", "reachStdDev") -> GT
        ("ssSpeed", "reachRank") -> GT
        ("ssSpeed", _) -> LT

        ("gsSpeed", "pilotsAtEss") -> GT
        ("gsSpeed", "raceTime") -> GT
        ("gsSpeed", "ssBestTime") -> GT
        ("gsSpeed", "gsBestTime") -> GT
        ("gsSpeed", "taskDistance") -> GT
        ("gsSpeed", "taskSpeedDistance") -> GT
        ("gsSpeed", "bestEffort") -> GT
        ("gsSpeed", "bestReach") -> GT
        ("gsSpeed", "sumDistance") -> GT
        ("gsSpeed", "leadAreaToCoef") -> GT
        ("gsSpeed", "leadCoefMin") -> GT
        ("gsSpeed", "leadRank") -> GT
        ("gsSpeed", "arrivalRank") -> GT
        ("gsSpeed", "flownMean") -> GT
        ("gsSpeed", "flownStdDev") -> GT
        ("gsSpeed", "reachMean") -> GT
        ("gsSpeed", "reachStdDev") -> GT
        ("gsSpeed", "reachRank") -> GT
        ("gsSpeed", "ssSpeed") -> GT
        ("gsSpeed", _) -> LT

        ("nigh", "pilotsAtEss") -> GT
        ("nigh", "raceTime") -> GT
        ("nigh", "ssBestTime") -> GT
        ("nigh", "gsBestTime") -> GT
        ("nigh", "taskDistance") -> GT
        ("nigh", "bestEffort") -> GT
        ("nigh", "bestReach") -> GT
        ("nigh", "sumDistance") -> GT
        ("nigh", "leadAreaToCoef") -> GT
        ("nigh", "leadCoefMin") -> GT
        ("nigh", "leadRank") -> GT
        ("nigh", "arrivalRank") -> GT
        ("nigh", "reachMean") -> GT
        ("nigh", "reachStdDev") -> GT
        ("nigh", "reachRank") -> GT
        ("nigh", "ssSpeed") -> GT
        ("nigh", "gsSpeed") -> GT
        ("nigh", _) -> LT

        ("land", "altStopped") -> LT
        ("land", _) -> GT

        ("altStopped", _) -> GT

        ("area", _) -> LT
        ("coef", "area") -> GT
        ("coef", _) -> LT

        ("time", _) -> LT
        ("rank", _) -> LT
        ("frac", _) -> GT

        ("madeGoal", _) -> LT

        ("timeToGoal", "madeGoal") -> GT
        ("timeToGoal", "arrivalRank") -> GT
        ("timeToGoal", _) -> LT

        ("togo", _) -> LT
        ("made", _) -> GT

        _ -> compare a b