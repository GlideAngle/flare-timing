{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Mask.Mask (writeMask) where

import Control.Lens ((^?), element)
import Control.Monad.Except (runExceptT)
import Control.Concurrent.ParallelIO (parallel)

import qualified Flight.Comp as Cmp (Nominal(..))
import Flight.Comp
    ( CompInputFile(..)
    , CompSettings(..)
    , Pilot(..)
    , Task(..)
    , Tweak(..)
    , IxTask(..)
    , TrackFileFail(..)
    , RoutesLookupTaskDistance(..)
    , compToMaskArrival
    )
import Flight.Mask (FnIxTask, settingsLogs)
import Flight.Track.Tag (Tagging)
import Flight.Track.Arrival (TrackArrival(..), arrivalsByTime, arrivalsByRank)
import qualified Flight.Track.Arrival as Arrival (TrackArrival(..))
import qualified Flight.Lookup as Lookup
    (arrivalRank, compRoutes, pilotTime, pilotEssTime)
import Flight.Lookup.Tag (tagArrivalRank, tagPilotTime)
import Flight.TrackLog (pilotTrack)
import Flight.Scribe (writeMaskingArrival)
import "flight-gap-allot" Flight.Score (ArrivalFraction(..))
import Flight.Span.Math (Math(..))
import Stats (TimeStats(..), FlightStats(..), nullStats)
import MaskArrival (maskArrival, arrivalInputs)
import MaskPilots (maskPilots)

type IOStep k = Either (Pilot, TrackFileFail) (Pilot, Pilot -> FlightStats k)

writeMask
    :: CompSettings k
    -> RoutesLookupTaskDistance
    -> Math
    -> Maybe Tagging
    -> [IxTask]
    -> [Pilot]
    -> CompInputFile
    -> IO ()
writeMask
    CompSettings
        { nominal = Cmp.Nominal{free}
        , tasks
        , pilotGroups
        }
    routes
    math
    tags
    ixSelectTasks selectPilots compFile = do
    (_, selectedCompLogs) <- settingsLogs compFile ixSelectTasks selectPilots

    fss :: [[IOStep k]] <-
            sequence $
            [
                parallel $
                [ runExceptT $ pilotTrack (flown math tags tasks ixTask) pilotLog
                | pilotLog <- taskLogs
                ]

            | ixTask <- IxTask <$> [1..]
            | taskLogs <- selectedCompLogs
            ]

    let iTasks = IxTask <$> [1 .. length fss]

    -- Task lengths (ls).
    let lsTask' = Lookup.compRoutes routes iTasks

    let yss = maskPilots free tasks lsTask' pilotGroups fss

    -- Arrivals (as).
    let as :: [[(Pilot, TrackArrival)]] =
            [
                let aRank = maybe True arrivalRank tweak
                    aTime = maybe False arrivalTime tweak
                    ys' = arrivalInputs ys
                in
                    case (aRank, aTime) of
                        (True, _) -> arrivalsByRank ys'
                        (False, True) -> arrivalsByTime ys'
                        -- NOTE: We're not using either kind of arrival
                        -- for points so zero the fraction.
                        (False, False) ->
                            [ (p, ta{Arrival.frac = ArrivalFraction 0})
                            | (p, ta) <- arrivalsByRank ys'
                            ]

            | Task{taskTweak = tweak} <- tasks
            | ys <- yss
            ]

    -- REVIEW: Waiting on feedback on GAP rule question about altitude
    -- bonus distance pushing a flight to goal so that it arrives.
    writeMaskingArrival (compToMaskArrival compFile) (maskArrival as)

flown :: Math -> Maybe Tagging -> FnIxTask k (Pilot -> FlightStats k)
flown Rational _ _ _ _ _ = error "Nigh for rationals not yet implemented."
flown Floating tags tasks iTask@(IxTask i) mf p =
    case maybeTask of
        Nothing -> nullStats

        Just _ ->
            case (ssTime, gsTime, esTime, arrivalRank) of
                (Just a, Just b, Just e, c@(Just _)) ->
                    tickedStats {statTimeRank = Just $ TimeStats a b e c}

                _ -> tickedStats

    where
        maybeTask = tasks ^? element (i - 1)

        (_, esTime) = Lookup.pilotEssTime (tagPilotTime tags) mf iTask [] speedSection' p
        ssTime = Lookup.pilotTime (tagPilotTime tags) mf iTask [] speedSection' p
        gsTime = Lookup.pilotTime (tagPilotTime tags) mf iTask startGates' speedSection' p
        arrivalRank = Lookup.arrivalRank (tagArrivalRank tags) mf iTask speedSection' p

        tickedStats = nullStats

        startGates' =
            case tasks ^? element (i - 1) of
                Nothing -> []
                Just Task{..} -> startGates

        speedSection' =
            case tasks ^? element (i - 1) of
                Nothing -> Nothing
                Just Task{..} -> speedSection
