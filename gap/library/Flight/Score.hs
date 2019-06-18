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
    -- * Validity
    , LaunchValidity(..)
    , LaunchValidityWorking(..)
    , TimeValidity(..)
    , TimeValidityWorking(..)
    , DistanceValidity(..)
    , DistanceValidityWorking(..)
    , StopValidity(..)
    , ReachToggle(..)
    , ReachStats(..)
    , StopValidityWorking(..)
    , TaskValidity(..)
    , Validity(..)
    , ValidityWorking(..)
    , NominalDistanceArea(..)
    , launchValidity
    , distanceValidity
    , timeValidity
    , stopValidity
    , taskValidity
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
    , AwScaling(..)
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
    , arrivalFraction
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
    , linearFraction
    , bestDistance'
    -- * Fractional allotment of difficulty distance points
    , Lookahead(..)
    , Chunk(..)
    , Chunks(..)
    , IxChunk(..)
    , lookahead
    , toIxChunk
    , toChunk
    , chunks
    , landouts
    , mergeChunks
    , SumOfDifficulty(..)
    , RelativeDifficulty(..)
    , DifficultyFraction(..)
    , ChunkRelativeDifficulty(..)
    , ChunkDifficultyFraction(..)
    , ChunkLandings(..)
    , ChunkDifficulty(..)
    , Difficulty(..)
    , gradeDifficulty
    -- * Fractional allotment of leading points 
    , TaskTime(..)
    , DistanceToEss(..)
    , Leg(..)
    , LcPoint(..)
    , LcSeq(..)
    , LcTrack
    , LcArea
    , LcCoef
    , TaskDeadline(..)
    , LengthOfSs(..)
    , LeadingAreaScaling(..)
    , LeadingArea(..)
    , LeadingCoef(..)
    , AreaToCoef(..)
    , LeadingFraction(..)
    , EssTime(..)
    , clampToEss
    , clampToDeadline
    , areaSteps
    , areaToCoef
    , mkCoef
    , leadingFraction
    , leadingFractions
    , leadingCoefficient
    , madeGoal
    , cleanTrack
    , showSecs
    -- * Places
    , ArrivalPlacing(..)
    , TaskPlacing(..)
    -- * Tallying points
    , LaunchToSssPoints(..)
    , MinimumDistancePoints(..)
    , SecondsPerPoint(..)
    , JumpedTheGun(..)
    , Hg
    , Pg
    , Penalty(..)
    , PointPenalty(..)
    , DistancePoints(..)
    , LinearPoints(..)
    , DifficultyPoints(..)
    , LeadingPoints(..)
    , ArrivalPoints(..)
    , TimePoints(..)
    , TaskPoints(..)
    , Points(..)
    , zeroPoints
    , taskPoints
    , applyPointPenalties
    , availablePoints
    -- * Stopped task
    , LaunchToEss(..)
    , FlownMax(..)
    , FlownMean(..)
    , FlownStdDev(..)
    , TaskStopTime(..)
    , AnnouncedTime(..)
    , ScoreBackTime(..)
    , StartGateInterval(..)
    , StopTime(..)
    , CanScoreStopped(..)
    , unFlownMaxAsKm
    , stopTaskTime
    , canScoreStopped
    , SumOfDistance(..)
    , TaskType(..)
    , StartGates(..)
    , ScoreTimeWindow(..)
    , scoreTimeWindow
    , AltitudeAboveGoal(..)
    , DistanceToGoal(..)
    , GlideRatio(..)
    , StoppedTrack(..)
    , applyGlide
    , applyGlides
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
    ) where

import Flight.Gap.Leading.Area
import Flight.Gap.Leading.Coef
import Flight.Gap.Leading.Scaling
import Flight.Gap.Ratio
import Flight.Gap.Allot
import Flight.Gap.Pilots
import Flight.Gap.Ratio.Launch
import Flight.Gap.Ratio.Arrival
import Flight.Gap.Ratio.Leading
import Flight.Gap.Ratio.Goal
import Flight.Gap.Time.Nominal
import Flight.Gap.Time.Best
import Flight.Gap.Time.Velocity
import Flight.Gap.Time.ScoreBack
import Flight.Gap.Distance.Nominal
import Flight.Gap.Distance.Linear
import Flight.Gap.Distance.Min
import Flight.Gap.Distance.Sum
import Flight.Gap.Distance.Relative
import Flight.Gap.Distance.Fraction
import Flight.Gap.Distance.Chunk
import Flight.Gap.Distance.Difficulty
import Flight.Gap.Distance.Stop
import Flight.Gap.Leading
import Flight.Gap.Place.Arrival
import Flight.Gap.Place.Task
import Flight.Gap.Points.Arrival
import Flight.Gap.Points.Distance
import Flight.Gap.Points.Leading
import Flight.Gap.Points.Time
import Flight.Gap.Points.Task
import Flight.Gap.Validity.Launch
import Flight.Gap.Validity.Distance
import Flight.Gap.Validity.Time
import Flight.Gap.Validity.Task
import Flight.Gap.Validity
import Flight.Gap.Weight.GoalRatio
import Flight.Gap.Weight.Distance
import Flight.Gap.Weight.Leading
import Flight.Gap.Weight.Arrival
import Flight.Gap.Weight.Time
import Flight.Gap.Weighting
import Flight.Gap.Points
import Flight.Gap.Stopped
