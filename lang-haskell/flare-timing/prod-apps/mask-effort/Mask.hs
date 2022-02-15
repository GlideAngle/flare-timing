{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Mask (writeMask) where

import Control.Lens ((^?), element)
import Control.Monad.Except (runExceptT)
import Control.Concurrent.ParallelIO (parallel)
import Data.UnitsOfMeasure (KnownUnit, Unpack, u)

import Flight.Zone.Cylinder (SampleParams(..), Samples(..), Tolerance(..))
import Flight.Zone.Raw (Give)
import Flight.Earth.Ellipsoid (wgs84)
import Flight.Earth.Sphere (earthRadius)
import Flight.Geodesy (EarthMath(..), EarthModel(..), Projection(..))
import Flight.Clip (FlyCut(..))
import qualified Flight.Comp as Cmp (Nominal(..))
import Flight.Comp
    ( ScoringInputFiles
    , CompTaskSettings(..)
    , Comp(..)
    , Tweak(..)
    , Pilot(..)
    , Task(..)
    , IxTask(..)
    , TrackFileFail(..)
    , RoutesLookupTaskDistance(..)
    , TaskRouteDistance(..)
    )
import Flight.Distance (QTaskDistance)
import Flight.Mask (GeoDash(..), FnIxTask, settingsLogs)
import Flight.Track.Tag (CompTagging)
import Flight.Track.Lead (LeadingAreaSum, MkLeadingCoef, MkAreaToCoef)
import qualified Flight.Track.Time as Time (TickRow(..))
import Flight.Track.Mask (CompMaskingArrival(..))
import Flight.Track.Distance (TrackDistance(..), Effort)
import Flight.Kml (MarkedFixes(..))
import Flight.Lookup.Stop (ScoredLookup(..))
import qualified Flight.Lookup as Lookup
    ( scoredTimeRange, arrivalRank, ticked, compRoutes
    , pilotTime, pilotEssTime
    )
import Flight.Lookup.Tag
    ( TaskLeadingLookup(..)
    , tagArrivalRank
    , tagPilotTime
    , tagTicked
    )
import Flight.TrackLog (pilotTrack)
import Flight.Scribe
    ( AltBonus(..)
    , writeCompMaskEffort
    , readCompBestDistances
    -- TODO: Take care to consider bonus altitude distance with leading area.
    -- , readPilotDiscardFurther
    -- , readPilotPegThenDiscard
    , readCompLeadArea
    )
import "flight-gap-allot" Flight.Score (powerExp23)
import Flight.Span.Math (Math(..))
import Stats (TimeStats(..), FlightStats(..), nullStats)
import MaskEffort (maskEffort, landDistances)
import MaskLead (raceTimes)
import MaskLeadCoef (maskLeadCoef)
import MaskSpeed (maskSpeedBestTime)
import MaskPilots (maskPilots)

sp :: SampleParams Double
sp = SampleParams (replicate 6 $ Samples 11) (Tolerance 0.03)

type IOStep k = Either (Pilot, TrackFileFail) (Pilot, Pilot -> FlightStats k)

writeMask
    :: (KnownUnit (Unpack u))
    => CompMaskingArrival
    -> LeadingAreaSum u
    -> MkLeadingCoef u
    -> MkAreaToCoef v
    -> CompTaskSettings k
    -> RoutesLookupTaskDistance
    -> Math
    -> ScoredLookup
    -> Maybe CompTagging
    -> TaskLeadingLookup
    -> [IxTask]
    -> [Pilot]
    -> ScoringInputFiles
    -> IO ()
writeMask
    CompMaskingArrival{arrivalRank}
    sumAreas
    invert
    areaToCoef
    CompTaskSettings
        { comp = Comp{earthMath, give}
        , nominal = Cmp.Nominal{free}
        , compTweak
        , tasks
        , pilotGroups
        }
    routes
    math
    flying
    tags
    lookupTaskLeading
    ixSelectTasks selectPilots inFiles@(compFile, _) = do
    (_, selectedCompLogs) <- settingsLogs inFiles ixSelectTasks selectPilots

    fss :: [[IOStep k]] <-
            sequence
            [
                parallel
                [ runExceptT $ pilotTrack (flown math earthMath give routes flying tags tasks ixTask) pilotLog
                | pilotLog <- taskLogs
                ]

            | ixTask <- IxTask <$> [1..]
            | taskLogs <- selectedCompLogs
            ]

    let iTasks = IxTask <$> [1 .. length fss]

    -- Task lengths (ls).
    let lsTask' = Lookup.compRoutes routes iTasks

    let yss = maskPilots free tasks lsTask' pilotGroups fss

    -- Distances (ds) of the landout spot.
    let dsLand :: [[(Pilot, TrackDistance Effort)]] = landDistances <$> yss
    let psLandingOut = (fmap . fmap) fst dsLand

    let psArriving = (fmap . fmap) fst arrivalRank

    {- TODO: Take care to consider bonus altitude distance with leading area.
    let pilots =
            [ pAs ++ pLs
            | pAs <- psArriving
            | pLs <- psLandingOut
            ]
    -}

    -- Zones (zs) of the task and zones ticked.
    let tpe = maybe powerExp23 timePowerExponent compTweak
    let gsBestTime = maskSpeedBestTime tpe yss
    let raceTimes' = raceTimes lookupTaskLeading iTasks tasks

    {- TODO: Take care to consider bonus altitude distance with leading area.
    nullAltRows :: [[(Pilot, [Time.TickRow])]]
        <-
            sequence $
            [ sequence [sequence (p, readPilotDiscardFurther compFile ix p) | p <- ps]
            | ix <- (IxTask <$> [1 .. ])
            | ps <- pilots
            ]

    bonusAltRows :: [[(Pilot, [Time.TickRow])]]
        <-
            sequence $
            [ sequence [sequence (p, readPilotPegThenDiscard compFile ix p) | p <- ps]
            | ix <- (IxTask <$> [1 .. ])
            | ps <- pilots
            ]
    -}

    -- For each task, for each pilot, the row closest to goal.
    nullAltRowsBest :: [[Maybe (Pilot, Time.TickRow)]]
        <- readCompBestDistances
            (AltBonus False)
            compFile
            (includeTask ixSelectTasks)
            ((fmap . fmap) fst dsLand)

    leadArea <- readCompLeadArea compFile

    let (dsNullAltBest, _nullAltRowTicks, _nullAltLead) =
            maskLeadCoef
                sumAreas
                invert
                areaToCoef
                free
                raceTimes'
                lsTask'
                psArriving
                psLandingOut
                gsBestTime
                nullAltRowsBest
                leadArea

    -- TODO: Use altitude bonus distance for effort.
    writeCompMaskEffort compFile (maskEffort dsNullAltBest dsLand)

includeTask :: [IxTask] -> IxTask -> Bool
includeTask tasks = if null tasks then const True else (`elem` tasks)

flown
    :: Math
    -> EarthMath
    -> Maybe Give
    -> RoutesLookupTaskDistance
    -> ScoredLookup
    -> Maybe CompTagging
    -> FnIxTask k (Pilot -> FlightStats k)
flown math earthMath give (RoutesLookupTaskDistance lookupTaskLength) flying tags tasks iTask fixes =
    maybe
        (const nullStats)
        (\d -> flown' d flying math earthMath give tags tasks iTask fixes)
        taskLength
    where
        taskLength = (fmap wholeTaskDistance . ($ iTask)) =<< lookupTaskLength

flown'
    :: QTaskDistance Double [u| m |]
    -> ScoredLookup
    -> Math
    -> EarthMath
    -> Maybe Give
    -> Maybe CompTagging
    -> FnIxTask k (Pilot -> FlightStats k)
flown' _ _ Rational _ _ _ _ _ _ _ = error "Nigh for rationals not yet implemented."
flown' dTaskF flying Floating earthMath give tags tasks iTask@(IxTask i) mf@MarkedFixes{mark0} p =
    case maybeTask of
        Nothing -> nullStats

        Just task' ->
            case (ssTime, gsTime, esTime, arrivalRank) of
                (Just a, Just b, Just e, c@(Just _)) ->
                    nullStats {statTimeRank = Just $ TimeStats a b e c}

                _ ->
                    nullStats {statEffort = Just $ landDistance task'}

    where
        maybeTask = tasks ^? element (i - 1)

        ticked = Lookup.ticked (tagTicked tags) mf iTask speedSection' p
        (_, esTime) = Lookup.pilotEssTime (tagPilotTime tags) mf iTask [] speedSection' p
        ssTime = Lookup.pilotTime (tagPilotTime tags) mf iTask [] speedSection' p
        gsTime = Lookup.pilotTime (tagPilotTime tags) mf iTask startGates' speedSection' p
        arrivalRank = Lookup.arrivalRank (tagArrivalRank tags) mf iTask speedSection' p

        xs =
            FlyCut
                { cut = Lookup.scoredTimeRange flying mark0 iTask p
                , uncut = mf
                }

        landDistance task =
            let earth =
                    ( earthMath
                    , let e = EarthAsEllipsoid wgs84 in case earthMath of
                          Pythagorus -> EarthAsFlat UTM
                          Haversines -> EarthAsSphere earthRadius
                          Vincenty -> e
                          AndoyerLambert -> e
                          ForsytheAndoyerLambert -> e
                          FsAndoyer -> e
                    )
            in
                TrackDistance
                    { togo =
                        togoAtLanding @Double @Double
                            earth
                            give
                            sp
                            ticked
                            task
                            xs

                    , made =
                        madeAtLanding @Double @Double
                            earth
                            give
                            sp
                            dTaskF
                            ticked
                            task
                            xs
                    }

        (startGates', speedSection') =
            case tasks ^? element (i - 1) of
                Nothing -> ([], Nothing)
                Just Task{..} -> (startGates, speedSection)
