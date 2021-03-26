{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Mask (writeMask) where

import Control.Lens ((^?), element)
import Control.Monad.Except (runExceptT)
import Control.Concurrent.ParallelIO (parallel)

import Flight.Clip (FlyCut(..), FlyClipping(..))
import qualified Flight.Comp as Cmp (Nominal(..))
import Flight.Comp
    ( ScoringInputFiles
    , CompTaskSettings(..)
    , Pilot(..)
    , Task(..)
    , Tweak(..)
    , IxTask(..)
    , TrackFileFail(..)
    , RoutesLookupTaskDistance(..)
    )
import Flight.Mask (FnIxTask, settingsLogs)
import Flight.Track.Tag (CompTagging)
import Flight.Kml (LatLngAlt(..), MarkedFixes(..))
import Flight.Lookup.Stop (ScoredLookup(..))
import Flight.Track.Arrival (TrackArrival(..), arrivalsByTime, arrivalsByRank)
import qualified Flight.Track.Arrival as Arrival (TrackArrival(..))
import qualified Flight.Lookup as Lookup
    (scoredTimeRange, arrivalRank, compRoutes, pilotTime, pilotEssTime)
import Flight.Lookup.Tag (tagArrivalRank, tagPilotTime)
import Flight.TrackLog (pilotTrack)
import Flight.Scribe (writeCompMaskArrival, writeCompMaskSpeed)
import "flight-gap-allot" Flight.Score (PowerExponent(..), ArrivalFraction(..))
import Flight.Span.Math (Math(..))
import Stats (TimeStats(..), FlightStats(..), nullStats, altToAlt)
import MaskArrival (maskArrival, arrivalInputs)
import MaskSpeed (maskSpeed)
import MaskPilots (maskPilots)

type IOStep k = Either (Pilot, TrackFileFail) (Pilot, Pilot -> FlightStats k)

writeMask
    :: CompTaskSettings k
    -> RoutesLookupTaskDistance
    -> Math
    -> ScoredLookup
    -> Maybe CompTagging
    -> [IxTask]
    -> [Pilot]
    -> ScoringInputFiles
    -> IO ()
writeMask
    CompTaskSettings
        { nominal = Cmp.Nominal{free}
        , compTweak
        , tasks
        , pilotGroups
        }
    routes
    math
    flying
    tags
    ixSelectTasks selectPilots inFiles@(compFile, _) = do
    (_, selectedCompLogs) <- settingsLogs inFiles ixSelectTasks selectPilots

    fss :: [[IOStep k]] <-
            sequence $
            [
                parallel $
                [ runExceptT $ pilotTrack (flown math flying tags tasks ixTask) pilotLog
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
    writeCompMaskArrival compFile (maskArrival as)

    let tpe = maybe (PowerExponent $ 2/3) timePowerExponent compTweak
    let (_gsBestTime, maskSpeed') = maskSpeed tpe lsTask' yss

    -- NOTE: For time and leading points do not use altitude bonus distances.
    writeCompMaskSpeed compFile maskSpeed'

flown :: Math -> ScoredLookup -> Maybe CompTagging -> FnIxTask k (Pilot -> FlightStats k)
flown Rational _ _ _ _ _ _ = error "Nigh for rationals not yet implemented."
flown Floating flying tags tasks iTask@(IxTask i) mf@MarkedFixes{mark0} p =
    case maybeTask of
        Nothing -> nullStats

        Just _ ->
            case (ssTime, gsTime, esTime, arrivalRank) of
                (Just a, Just b, Just e, c@(Just _)) ->
                    nullStats {statTimeRank = Just $ TimeStats a b e c}

                _ ->
                    nullStats
                        { statAlt =
                            case reverse ys of
                                [] -> Nothing
                                y : _ -> Just . altToAlt $ altGps y
                        }

    where
        maybeTask = tasks ^? element (i - 1)

        (_, esTime) = Lookup.pilotEssTime (tagPilotTime tags) mf iTask [] speedSection' p
        ssTime = Lookup.pilotTime (tagPilotTime tags) mf iTask [] speedSection' p
        gsTime = Lookup.pilotTime (tagPilotTime tags) mf iTask startGates' speedSection' p
        arrivalRank = Lookup.arrivalRank (tagArrivalRank tags) mf iTask speedSection' p
        FlyCut{uncut = MarkedFixes{fixes = ys}} = clipToCut xs

        xs =
            FlyCut
                { cut = Lookup.scoredTimeRange flying mark0 iTask p
                , uncut = mf
                }

        (startGates', speedSection') =
            case tasks ^? element (i - 1) of
                Nothing -> ([], Nothing)
                Just Task{..} -> (startGates, speedSection)
