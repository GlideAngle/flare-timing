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
    , PowerExponent(..)
    , powerFraction
    -- * Fractions
    , Fractions(..)
    , DifficultyFraction(..)
    , LeadingFraction(..)
    ) where

import Flight.Gap.Ratio
import Flight.Gap.Allot
import Flight.Gap.Pilots
import Flight.Gap.Fraction
import Flight.Gap.Fraction.Launch
import Flight.Gap.Fraction.Linear
import Flight.Gap.Fraction.Goal
import Flight.Gap.Time.Arrival
import Flight.Gap.Time.Nominal
import Flight.Gap.Time.Best
import Flight.Gap.Time.Velocity
import Flight.Gap.Distance.Pilot
import Flight.Gap.Distance.Nominal
import Flight.Gap.Distance.Min
import Flight.Gap.Distance.Sum
import Flight.Gap.Distance.Stop
import Flight.Gap.Place.Arrival
import Flight.Gap.Place.Task
import Flight.Gap.Equation
