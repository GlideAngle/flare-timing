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
    ( distanceToGoal
    , distancesToGoal
    , distanceFlown
    ) where

import Prelude hiding (span)
import Data.Time.Clock (UTCTime)
import Data.List (inits)
import Data.UnitsOfMeasure ((-:))
import Data.UnitsOfMeasure.Internal (Quantity(..))

import qualified Flight.Kml as Kml (MarkedFixes(..), Fix)
import Flight.Track.Cross (Fix(..))
import qualified Flight.Comp as Cmp (Task(..))
import Flight.Score (PilotDistance(..))
import Flight.Units ()
import Flight.Mask.Internal
    (Sliver(..), TaskZone(..), Ticked, fixFromFix, distanceViaZones)
import qualified Flight.Mask.Internal as I (distanceToGoal)
import qualified Flight.Zone.Raw as Raw (RawZone(..))
import Flight.Distance (TaskDistance(..))

distancesToGoal :: (Real a, Fractional a)
                => Ticked
                -> Sliver a
                -> (Raw.RawZone -> TaskZone a)
                -> Cmp.Task
                -> Kml.MarkedFixes
                -> Maybe [(Maybe Fix, Maybe (TaskDistance a))]
                -- ^ Nothing indicates no such task or a task with no zones.
distancesToGoal
    ticked sliver zoneToCyl task@Cmp.Task{zones} Kml.MarkedFixes{mark0, fixes} =
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

-- | The distance from the last fix to goal passing through the remaining
-- control zones.
lastFixToGoal :: (Real a, Fractional a)
              => Ticked
              -> Sliver a
              -> (Raw.RawZone -> TaskZone a)
              -> Cmp.Task
              -> UTCTime
              -> [Kml.Fix]
              -> (Maybe Fix, Maybe (TaskDistance a))
lastFixToGoal ticked sliver@Sliver{..} zoneToCyl task mark0 ys =
    case reverse ys of
        [] -> (Nothing, Nothing)
        (y : _) -> (Just $ fixFromFix mark0 (length ys - 1) y, d)
    where
        xs = Kml.MarkedFixes { Kml.mark0 = mark0, Kml.fixes = ys }
        dvz = distanceViaZones ticked sliver
        d = I.distanceToGoal span zoneToCyl dvz task xs

distanceToGoal :: (Real a, Fractional a)
               => Ticked
               -> Sliver a
               -> (Raw.RawZone -> TaskZone a)
               -> Cmp.Task
               -> Kml.MarkedFixes
               -> Maybe (TaskDistance a)
distanceToGoal ticked sliver@Sliver{span} zoneToCyl =
    I.distanceToGoal span zoneToCyl dvz
    where
        dvz = distanceViaZones ticked sliver

distanceFlown :: (Real a, Fractional a)
              => TaskDistance a
              -> Ticked
              -> Sliver a
              -> (Raw.RawZone -> TaskZone a)
              -> Cmp.Task
              -> Kml.MarkedFixes
              -> Maybe (PilotDistance a)
distanceFlown
    (TaskDistance dTask)
    ticked
    sliver
    zoneToCyl task@Cmp.Task{zones} fixes =
    if null zones then Nothing else do
        TaskDistance dPilot <- distanceToGoal ticked sliver zoneToCyl task fixes
        let (MkQuantity diff) = dTask -: dPilot

        return $ PilotDistance diff
