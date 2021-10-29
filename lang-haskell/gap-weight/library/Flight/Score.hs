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
    -- * Weighting
      GoalRatio(..)
    , DistanceRatio(..)
    , DistanceWeight(..)
    , ReachWeight(..)
    , EffortWeight(..)
    , LeadingWeight(..)
    , ArrivalWeight(..)
    , TimeWeight(..)
    , Lw(..)
    , LwScaling(..)
    , EGwScaling(..)
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
    ) where

import Flight.Gap.Weight.GoalRatio
import Flight.Gap.Weight.Distance
import Flight.Gap.Weight.Leading
import Flight.Gap.Weight.EssNotGoal
import Flight.Gap.Weight.Arrival
import Flight.Gap.Weight.Time
import Flight.Gap.Weighting
