{-# OPTIONS_GHC -fno-warn-unused-binds #-}

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
    -- * Units
    , Seconds
    , Metres
    -- * Ratio
    , isNormal
    , isFoldNormal
    -- * Validity
    , LaunchValidity(..)
    , TimeValidity(..)
    , DistanceValidity(..)
    , TaskValidity(..)
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
    , distanceWeight
    , leadingWeight
    , arrivalWeight
    , timeWeight
    -- * Fractional allotment of arrival points
    , PilotsAtEss(..)
    , PositionAtEss(..)
    , ArrivalFraction(..)
    , arrivalFraction
    -- * Fractional allotment of time points
    , BestTime(..)
    , PilotTime(..)
    , SpeedFraction(..)
    , bestTime
    , speedFraction
    -- * Fractional allotment of linear distance points
    , BestDistance(..)
    , PilotDistance(..)
    , LinearFraction(..)
    , linearFraction
    , bestDistance
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
    , difficulty
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
    , TaskPointParts(..)
    , TaskPoints(..)
    , zeroPoints
    , taskPoints
    , applyPointPenalty
    -- * Stopped task 
    , TaskStopTime(..)
    , AnnouncedTime(..)
    , ScoreBackTime(..)
    , StartGateInterval(..)
    , StopTime(..)
    , NumberInGoalAtStop(..)
    , CanScoreStopped(..)
    , stopTaskTime
    , canScoreStopped
    , MaximumDistance(..)
    , SumOfDistance(..)
    , PilotsLaunched(..)
    , PilotsLandedBeforeStop(..)
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
    ) where

import Flight.Gap.Ratio
import Flight.Gap.Allot
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
import Flight.Gap.Validity
import Flight.Gap.Weighting.GoalRatio
import Flight.Gap.Weighting
import Flight.Gap.Points
import Flight.Gap.Stopped

type DistancePoint = Rational
type SpeedPoint = Rational
type DeparturePoint = Rational
type ArrivalPoint = Rational

data FixDistance = FixDistance Seconds Metres
data PointsAllocation =
    PointsAllocation { distance :: Rational
                     , speed :: Rational
                     , departure :: Rational
                     , arrival :: Rational
                     }

distancePoints :: [Metres] -> [DistancePoint]
distancePoints = undefined

speedPoints :: [Seconds] -> [SpeedPoint]
speedPoints = undefined

departurePoints :: [FixDistance] -> [DeparturePoint]
departurePoints = undefined

arrivalPoints :: Int -> [ArrivalPoint]
arrivalPoints = undefined

allocatePoints :: Rational -> PointsAllocation
allocatePoints = undefined
