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
    -- * Validity
      LaunchValidity(..)
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
    ) where

import Flight.Gap.Validity.Launch
import Flight.Gap.Validity.Distance
import Flight.Gap.Validity.Time
import Flight.Gap.Validity.Task
import Flight.Gap.Validity
