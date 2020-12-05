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
    (
    -- * Tallying points
      LaunchToStartPoints(..)
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
    , GoalValidatedPoints(..)
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
    , egPenalty, egPenaltyNull
    -- * Early start
    , SecondsPerPoint(..)
    , JumpTheGunLimit(..)
    , LaunchToSss(..)
    , jumpTheGunSitRepHg
    , jumpTheGunSitRepPg
    , jumpTheGunPenalty
    ) where

import Flight.Gap.Time.Early
import Flight.Gap.Distance.Early
import Flight.Gap.Points.Arrival
import Flight.Gap.Points.Distance
import Flight.Gap.Points.Leading
import Flight.Gap.Points.Time
import Flight.Gap.Points.Task
import Flight.Gap.Points
import Flight.Gap.Penalty
