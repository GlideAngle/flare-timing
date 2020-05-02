{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Flight.Score
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Provides GAP scoring for hang gliding and paragliding competitons.
-}
module Flight.Score
    ( -- * Nominals
      NominalLaunch(..)
    , NominalTime(..)
    , NominalDistance(..)
    , MinimumDistance(..)
    , NominalGoal(..)
    -- * Ratio
    , isNormal
    , isFoldNormal
    -- * Weighting
    , GoalRatio(..)
    , DistanceRatio(..)
    , DistanceWeight(..)
    , ReachWeight(..)
    , EffortWeight(..)
    , LeadingWeight(..)
    , ArrivalWeight(..)
    , TimeWeight(..)
    , Lw(..)
    , LwScaling(..)
    , Aw(..)
    , Rw(..)
    , Ew(..)
    , Weights(..)
    , distanceRatio
    , distanceWeight
    , reachWeight
    , effortWeight
    , leadingWeight
    , arrivalWeight
    , timeWeight
    -- * Fractional allotment of arrival points
    , ArrivalFraction(..)
    , ArrivalTime(..)
    , ArrivalLag(..)
    , arrivalRankFraction
    , arrivalTimeFraction
    -- * Fractional allotment of time points
    , BestTime(..)
    , PilotTime(..)
    , PilotVelocity(..)
    , SpeedFraction(..)
    , bestTime'
    , speedFraction
    -- * Fractional allotment of linear distance points
    , PilotDistance(..)
    , LinearFraction(..)
    , DistanceFraction(..)
    , linearFraction
    , bestDistance'
    -- * Places
    , ArrivalPlacing(..)
    , TaskPlacing(..)
    -- * Pilots
    , PilotId(..)
    , PilotName(..)
    , Pilot(..)
    -- * Pilot counts
    , PilotsAtEss(..)
    , PilotsLaunched(..)
    , PilotsPresent(..)
    , PilotsFlying(..)
    , PilotsLanded(..)
    -- * Validity
    , LaunchToEss(..)
    , FlownMax(..)
    , FlownMean(..)
    , FlownStdDev(..)
    , SumOfDistance(..)
    , unFlownMaxAsKm
    -- * Math
    , powerFraction
    ) where

--import Flight.Gap.Leading.Area
--import Flight.Gap.Leading.Coef
--import Flight.Gap.Leading.Scaling
--import Flight.Gap.Leading
--import Flight.Gap.Leading1Area
--import Flight.Gap.Leading2Area
import Flight.Gap.Ratio
import Flight.Gap.Allot
import Flight.Gap.Pilots
import Flight.Gap.Fraction.Launch
import Flight.Gap.Fraction.Linear
--import Flight.Gap.Fraction.Difficulty
import Flight.Gap.Fraction.Distance
import Flight.Gap.Fraction.Leading
import Flight.Gap.Fraction.Goal
import Flight.Gap.Time.Arrival
import Flight.Gap.Time.Nominal
import Flight.Gap.Time.Best
import Flight.Gap.Time.Velocity
import Flight.Gap.Distance.Pilot
import Flight.Gap.Distance.Nominal
import Flight.Gap.Distance.Min
import Flight.Gap.Distance.Sum
--import Flight.Gap.Distance.Relative
import Flight.Gap.Distance.Stop
import Flight.Gap.Place.Arrival
import Flight.Gap.Place.Task
import Flight.Gap.Weight.GoalRatio
import Flight.Gap.Weight.Distance
import Flight.Gap.Weight.Leading
import Flight.Gap.Weight.Arrival
import Flight.Gap.Weight.Time
import Flight.Gap.Weighting
import Flight.Gap.Equation
