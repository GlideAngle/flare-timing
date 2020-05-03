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
    -- * Stopped task
      LaunchToEss(..)
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
    ) where


import "flight-gap-allot" Flight.Score
import Flight.Gap.Stopped
