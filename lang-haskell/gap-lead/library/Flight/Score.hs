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
    -- * Fractional allotment of leading points
      TaskTime(..)
    , EssTime(..)
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
    , LeadingCoefUnits
    , LeadingCoef(..)
    , AreaToCoef(..)
    , LeadingFraction(..)
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
    ) where

import "flight-gap-allot" Flight.Score (LeadingFraction(..))
import Flight.Gap.Time.Leading
import Flight.Gap.Leading.Area
import Flight.Gap.Leading.Coef
import Flight.Gap.Leading.Scaling
import Flight.Gap.Leading
import Flight.Gap.Leading1Area
import Flight.Gap.Leading2Area
