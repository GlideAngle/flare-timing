{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Flight.Score
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
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
    , TaskValidity(..)
    , Validity(..)
    , ValidityWorking(..)
    , launchValidity
    , distanceValidity
    , timeValidity
    , taskValidity
    -- * Weighting
    , GoalRatio(..)
    , DistanceRatio(..)
    , DistanceWeight(..)
    , LeadingWeight(..)
    , ArrivalWeight(..)
    , TimeWeight(..)
    , Lw(..)
    , Aw(..)
    , Weights(..)
    , distanceWeight
    , leadingWeight
    , arrivalWeight
    , timeWeight
    -- * Fractional allotment of arrival points
    , PositionAtEss(..)
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
    , BestDistance(..)
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
    , TaskDeadline(..)
    , LengthOfSs(..)
    , LeadingAreaStep(..)
    , LeadingCoefficient(..)
    , LeadingFraction(..)
    , EssTime(..)
    , clampToEss
    , clampToDeadline
    , areaScaling
    , areaSteps
    , leadingFraction
    , leadingFractions
    , leadingCoefficient
    , madeGoal
    , cleanTrack
    , showSecs
    -- * Tallying points 
    , LaunchToSssPoints(..)
    , MinimumDistancePoints(..)
    , SecondsPerPoint(..)
    , JumpedTheGun(..)
    , Hg
    , Pg
    , Penalty(..)
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
    , applyPointPenalty
    , availablePoints
    -- * Stopped task 
    , TaskStopTime(..)
    , AnnouncedTime(..)
    , ScoreBackTime(..)
    , StartGateInterval(..)
    , StopTime(..)
    , CanScoreStopped(..)
    , stopTaskTime
    , canScoreStopped
    , MaximumDistance(..)
    , SumOfDistance(..)
    , DistanceLaunchToEss(..)
    , DistanceFlown(..)
    , StoppedValidity(..)
    , stoppedValidity
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
    -- * Pilot counts
    , PilotsAtEss(..)
    , PilotsLaunched(..)
    , PilotsInGoalAtStop(..)
    , PilotsLandedBeforeStop(..)
    , PilotsPresent(..)
    , PilotsFlying(..)
    ) where

import Flight.Gap.Ratio
import Flight.Gap.Allot
import Flight.Gap.Pilots
import Flight.Gap.Ratio.Launch
import Flight.Gap.Ratio.Leading
import Flight.Gap.Ratio.Goal
import Flight.Gap.Time.Nominal
import Flight.Gap.Time.Best
import Flight.Gap.Time.Velocity
import Flight.Gap.Distance.Nominal
import Flight.Gap.Distance.Linear
import Flight.Gap.Distance.Min
import Flight.Gap.Distance.Max
import Flight.Gap.Distance.Best
import Flight.Gap.Distance.Sum
import Flight.Gap.Distance.Relative
import Flight.Gap.Distance.Fraction
import Flight.Gap.Distance.Chunk
import Flight.Gap.Distance.Difficulty
import Flight.Gap.Leading
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
