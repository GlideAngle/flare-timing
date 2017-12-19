{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Mask.Distance
    ( dashDistancesToGoal
    , dashDistanceToGoal
    , dashDistanceFlown
    ) where

import Prelude hiding (span)
import Data.Time.Clock (UTCTime)
import Data.List (inits)
import Data.UnitsOfMeasure ((-:))
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Kml (MarkedFixes(..))
import qualified Flight.Kml as Kml (Fix)
import Flight.Track.Cross (Fix(..))
import Flight.Comp (Task(..))
import Flight.Score (PilotDistance(..))
import Flight.Units ()
import Flight.Mask.Internal.Zone (TaskZone(..), fixFromFix, fixToPoint)
import Flight.Mask.Internal.Race (Sliver(..), Ticked)
import Flight.Mask.Internal.Dash (dashToGoal)
import qualified Flight.Zone.Raw as Raw (RawZone(..))
import Flight.Distance (TaskDistance(..))

dashDistancesToGoal
    :: (Real a, Fractional a)
    => Ticked
    -> Sliver a
    -> (Raw.RawZone -> TaskZone a)
    -> Task
    -> MarkedFixes
    -> Maybe [(Maybe Fix, Maybe (TaskDistance a))]
    -- ^ Nothing indicates no such task or a task with no zones.
dashDistancesToGoal
    ticked sliver zoneToCyl task@Task{zones} MarkedFixes{mark0, fixes} =
    -- NOTE: A ghci session using inits & tails.
    -- inits [1 .. 4]
    -- [[],[1],[1,2],[1,2,3],[1,2,3,4]]
    --
    -- tails [1 .. 4]
    -- [[1,2,3,4],[2,3,4],[3,4],[4],[]]
    --
    -- tails $ reverse [1 .. 4]
    -- [[4,3,2,1],[3,2,1],[2,1],[1],[]]
    --
    -- drop 1 $ inits [1 .. 4]
    -- [[1],[1,2],[1,2,3],[1,2,3,4]]
    if null zones then Nothing else Just
    $ lfg zoneToCyl task mark0
    <$> drop 1 (inits fixes)
    where
        lfg = lastFixToGoal ticked sliver

dashDistanceToGoal
    :: (Real a, Fractional a)
    => Ticked
    -> Sliver a
    -> (Raw.RawZone -> TaskZone a)
    -> Task
    -> MarkedFixes
    -> Maybe (TaskDistance a)
    -- ^ Nothing indicates no such task or a task with no zones.
dashDistanceToGoal
    ticked sliver zoneToCyl
    Task{speedSection, zones}
    MarkedFixes{fixes} =
    if null zones then Nothing else
    dashToGoal ticked sliver fixToPoint speedSection zs fixes
    where
        zs = zoneToCyl <$> zones

-- | The distance from the last fix to goal passing through the remaining
-- control zones.
lastFixToGoal :: (Real a, Fractional a)
              => Ticked
              -> Sliver a
              -> (Raw.RawZone -> TaskZone a)
              -> Task
              -> UTCTime
              -> [Kml.Fix]
              -> (Maybe Fix, Maybe (TaskDistance a))
lastFixToGoal
    ticked
    sliver@Sliver{..}
    zoneToCyl
    Task{speedSection, zones}
    mark0
    ys =
    case reverse ys of
        [] -> (Nothing, Nothing)
        (y : _) -> (Just $ fixFromFix mark0 (length ys - 1) y, d)
    where
        d = dashToGoal ticked sliver fixToPoint speedSection zs ys
        zs = zoneToCyl <$> zones

dashDistanceFlown
    :: (Real a, Fractional a)
    => TaskDistance a
    -> Ticked
    -> Sliver a
    -> (Raw.RawZone -> TaskZone a)
    -> Task
    -> MarkedFixes
    -> Maybe (PilotDistance a)
dashDistanceFlown
    (TaskDistance dTask)
    ticked
    sliver
    zoneToCyl
    Task{speedSection, zones}
    MarkedFixes{fixes} =
    if null zones then Nothing else do
        TaskDistance dPilot
            <- dashToGoal ticked sliver fixToPoint speedSection zs fixes

        let (MkQuantity diff) = dTask -: dPilot

        return $ PilotDistance diff
    where
        zs = zoneToCyl <$> zones
