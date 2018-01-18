{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : Flight.Comp.Distance
Copyright   : (c) Block Scope Limited 2018
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Distances over all pilots.
-}
module Flight.Comp.Distance (DashPathInputs(..), compDistance, compNigh) where

import Data.Maybe (catMaybes, isJust)
import Data.List (sortOn)
import Data.Time.Clock (UTCTime)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList, lookup)
import Data.UnitsOfMeasure ((-:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Kml (MarkedFixes(..))
import Flight.Distance (TaskDistance(..), unTaskDistance)
import Flight.Comp (Pilot, Task(..))
import Flight.Route (TrackLine(..), toTrackLine)
import Flight.Score (BestTime(..))
import Flight.Track.Time (LeadTick(..))
import Flight.Track.Distance (TrackDistance(..), Land, Nigh)
import qualified Flight.Track.Time as Time (TimeRow(..), TickRow(..))
import Flight.Task (fromZs)
import Flight.Mask (dashPathToGoalTimeRows)
import Flight.Mask.Internal.Race (Ticked, FlyCut(..), Sliver(..))
import Flight.Task.Span.Double (zoneToCylF, spanF, csF, cutF, dppF, csegF)

data DashPathInputs =
    DashPathInputs
        { dashTask :: Maybe Task
        , dashTicked :: Ticked
        , dashFlyCut :: Maybe (FlyCut UTCTime MarkedFixes)
        }

compDistance
    :: Double
    -> [Maybe (TaskDistance Double)]
    -> [[Pilot]]
    -> [Maybe (BestTime (Quantity Double [u| h |]))]
    -> [[Maybe (Pilot, Time.TickRow)]]
    -> ( [Maybe Double]
       , [Maybe Double]
       , [[Maybe (Pilot, Maybe LeadTick)]]
       )
compDistance dNom lsTask pilotsLandingOut tsBest rows =
    (dsSum, dsBest, rowTicks)
    where
        -- Distances (ds) of point in the flight closest to goal.
        dsNigh :: [[(Pilot, TrackDistance Land)]] =
                zipWith3
                    lookupTaskBestDistance
                    (Map.fromList . catMaybes <$> rows)
                    lsTask
                    pilotsLandingOut

        -- For each task, for each pilot, the best distance made.
        dsMade :: [Maybe Double] =
                (\xs -> if null xs then Nothing else Just . maximum $ xs)
                . catMaybes
                <$> (fmap . fmap) (made . snd) dsNigh

        -- If a pilot makes goal then their best distance is the task
        -- distance.
        dsBest :: [Maybe Double] =
                zipWith3
                    (\l t d -> if isJust t then unTaskDistance <$> l else d)
                    lsTask
                    tsBest
                    dsMade

        dsSum :: [Maybe Double] =
                [ \case [] -> Nothing; xs -> Just . sum $ xs
                  $ (\d -> max 0 (d - dNom))
                  <$> ds
                | ds <- catMaybes <$> (fmap . fmap) (made . snd) dsNigh
                ]

        rowTicks :: [[Maybe (Pilot, Maybe LeadTick)]] =
                (fmap . fmap . fmap)
                    (fmap (\Time.TickRow{tickLead} -> tickLead))
                    rows

-- | How near did a pilot get to goal during the flight. This can be closer
-- to goal than the landing spot if the pilot flew away from goal to land.
compNigh
    :: [Maybe (TaskDistance Double)]
    -> [Map Pilot DashPathInputs]
    -> [[Maybe (Pilot, Time.TimeRow)]]
    -> [[(Pilot, TrackDistance Nigh)]]
compNigh lsTask zsTaskTicked rows =
        [ nighTrackLine td zs <$> xs
        | td <- lsTask
        | zs <- zsTaskTicked
        | xs <- (catMaybes <$> rows)
        ]

nighTrackLine
    :: Maybe (TaskDistance Double)
    -> Map Pilot DashPathInputs
    -> (Pilot, Time.TimeRow)
    -> (Pilot, TrackDistance Nigh)

nighTrackLine Nothing _ (p, Time.TimeRow{distance}) =
    (p,) TrackDistance
        { togo = Just $ distanceOnlyLine distance
        , made = Nothing
        }

nighTrackLine (Just (TaskDistance td)) zsTaskTicked (p, row@Time.TimeRow{distance}) =
    (p,) TrackDistance
        { togo = Just line
        , made = Just . unTaskDistance . TaskDistance $ td -: mTogo
        }
    where
        kmTogo :: Quantity Double [u| km |]
        kmTogo = MkQuantity distance

        mTogo = convert kmTogo :: Quantity Double [u| m |]

        line =
            case Map.lookup p zsTaskTicked of
                Nothing -> distanceOnlyLine distance
                Just dpi -> pathToGo dpi row distance

distanceOnlyLine :: Double -> TrackLine
distanceOnlyLine d =
    TrackLine
        { distance = d
        , waypoints = []
        , legs = []
        , legsSum = []
        }

pathToGo :: DashPathInputs -> Time.TimeRow -> Double -> TrackLine
pathToGo DashPathInputs{..} x@Time.TimeRow{time} d =
    case dashTask of
        Nothing -> distanceOnlyLine d
        Just dashTask' ->
            maybe
                (distanceOnlyLine d)
                (toTrackLine False)
                (fromZs path)
            where
                path = dashPathToGoalTimeRows
                        dashTicked
                        (Sliver spanF dppF csegF csF cutF)
                        zoneToCylF dashTask'
                        FlyCut{cut = Just (time, time), uncut = [x]}

lookupTaskBestDistance
    :: Map Pilot Time.TickRow
    -> Maybe (TaskDistance Double)
    -> [Pilot]
    -> [(Pilot, TrackDistance Land)]
lookupTaskBestDistance m td =
    sortOn (togo . snd)
    . catMaybes
    . fmap (lookupPilotBestDistance m td)

lookupPilotBestDistance
    :: Map Pilot Time.TickRow
    -> Maybe (TaskDistance Double)
    -> Pilot
    -> Maybe (Pilot, TrackDistance Land)
lookupPilotBestDistance m td p =
    ((p,) . madeDistance td) <$> Map.lookup p m

madeDistance
    :: Maybe (TaskDistance Double)
    -> Time.TickRow
    -> TrackDistance Land

madeDistance Nothing Time.TickRow{distance} =
    TrackDistance
        { togo = Just distance
        , made = Nothing
        }

madeDistance (Just (TaskDistance td)) Time.TickRow{distance} =
    TrackDistance
        { togo = Just distance
        , made = Just . unTaskDistance . TaskDistance $ td -: togo'
        }
    where
        togo :: Quantity Double [u| km |]
        togo = MkQuantity distance

        togo' = convert togo :: Quantity Double [u| m |]
