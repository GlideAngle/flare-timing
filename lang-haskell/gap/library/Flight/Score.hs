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
    , Chunking(..)
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
    , LeadingAreas(..)
    , LeadingArea(..)
    , LeadingAreaUnits
    , LeadingArea1Units
    , LeadingArea2Units
    , LeadingAreaToCoefUnits
    , LeadingArea1ToCoefUnits
    , LeadingArea2ToCoefUnits
    , LeadingCoef(..)
    , AreaToCoef(..)
    , LeadingFraction(..)
    , EssTime(..)
    , LeadAllDown(..)
    , clampToEss
    , clampToDeadline
    , area1Steps
    , area2Steps
    , area1toCoef
    , area2toCoef
    , mk1Coef
    , mk2Coef
    , leadingFraction
    , madeGoal
    , cleanTrack
    , showSecs
    , zeroLeadingArea1Units
    , zeroLeadingArea2Units
    -- * Places
    , ArrivalPlacing(..)
    , TaskPlacing(..)
    -- * Tallying points
    , LaunchToStartPoints(..)
    , TooEarlyPoints(..)
    , JumpedTheGun(..)
    , Hg
    , Pg
    , SitRep(..)
    , PointPenalty
    , Hide(..)
    , DistancePoints(..)
    , LinearPoints(..)
    , DifficultyPoints(..)
    , LeadingPoints(..)
    , ArrivalPoints(..)
    , TimePoints(..)
    , TaskPoints(..)
    , Points(..)
    , PointsReduced(..)
    , ReconcilePointErrors(..)
    , PosInt, GE
    , PenaltySeq(..), PenaltySeqs(..)
    , Mul, Add, Reset
    , zeroPoints
    , taskPoints
    , tallySubtotal
    , applyPenalties
    , idSeq, nullSeqs, toSeqs
    , seqOnlyMuls, seqOnlyAdds, seqOnlyResets
    , mulSeq, addSeq, resetSeq
    , idMul, idAdd, idReset
    , mkMul, mkAdd, mkReset
    , exMul, exAdd, exReset
    , identityOfMul, identityOfAdd, identityOfReset
    , applyMul, applyAdd, applyReset
    , availablePointsPg
    , availablePointsHg
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
    -- * Early start
    , SecondsPerPoint(..)
    , JumpTheGunLimit(..)
    , LaunchToSss(..)
    , jumpTheGunSitRepHg
    , jumpTheGunSitRepPg
    , jumpTheGunPenalty
    ) where

import "flight-gap-base" Flight.Score
import "flight-gap-effort" Flight.Score
import "flight-gap-lead" Flight.Score
import "flight-gap-valid" Flight.Score
import Flight.Gap.Time.Early
import Flight.Gap.Distance.Early
import Flight.Gap.Points.Arrival
import Flight.Gap.Points.Distance
import Flight.Gap.Points.Leading
import Flight.Gap.Points.Time
import Flight.Gap.Points.Task
import Flight.Gap.Points
import Flight.Gap.Penalty
import Flight.Gap.Stopped
