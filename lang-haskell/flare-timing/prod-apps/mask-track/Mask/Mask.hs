{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Mask.Mask (writeMask, check) where

import Data.Maybe (catMaybes)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList)
import Control.Lens ((^?), element)
import Control.Exception.Safe (MonadThrow, catchIO)
import Control.Monad.Except (MonadIO)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone.Cylinder (SampleParams(..), Samples(..), Tolerance(..))
import Flight.Zone.Raw (Give)
import Flight.Earth.Ellipsoid (wgs84)
import Flight.Earth.Sphere (earthRadius)
import Flight.Geodesy (EarthMath(..), EarthModel(..), Projection(..))
import Flight.Clip (FlyCut(..), FlyClipping(..))
import qualified Flight.Comp as Cmp (Nominal(..))
import Flight.Comp
    ( CompInputFile(..)
    , CompSettings(..)
    , Comp(..)
    , Pilot(..)
    , PilotGroup(..)
    , Task(..)
    , IxTask(..)
    , TrackFileFail(..)
    , RoutesLookupTaskDistance(..)
    , TaskRouteDistance(..)
    , DfNoTrack(..)
    , dfNoTrackReach
    , compToMaskArrival
    , compToMaskEffort
    , compToMaskLead
    , compToMaskReach
    , compToMaskSpeed
    , compToBonusReach
    )
import Flight.Distance (TaskDistance(..), QTaskDistance, unTaskDistanceAsKm)
import Flight.Mask (GeoDash(..), FnIxTask, checkTracks)
import Flight.Track.Tag (Tagging)
import qualified Flight.Track.Time as Time (TimeRow(..), TickRow(..))
import Flight.Track.Arrival (TrackArrival(..))
import Flight.Track.Distance (TrackDistance(..), TrackReach(..), Land)
import Flight.Kml (LatLngAlt(..), MarkedFixes(..))
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
import Flight.Scribe
    ( AltBonus(..)
    , writeMaskingArrival
    , writeMaskingEffort
    , writeMaskingLead
    , writeMaskingReach
    , writeMaskingSpeed
    , writeBonusReach
    , readCompBestDistances, readCompTimeRows
    , readPilotDiscardFurther
    , readPilotPegThenDiscard
    )
import qualified Flight.Score as Gap (ReachToggle(..))
import Flight.Span.Math (Math(..))
import Stats (TimeStats(..), FlightStats(..), DashPathInputs(..), nullStats, altToAlt)
import MaskArrival (maskArrival, arrivals)
import MaskEffort (maskEffort, landDistances)
import MaskLead (maskLead, raceTimes)
import Mask.Reach.Time (maskReachTime)
import Mask.Reach.Tick (maskReachTick)
import MaskSpeed (maskSpeed)
import MaskPilots (maskPilots)

sp :: SampleParams Double
sp = SampleParams (replicate 6 $ Samples 11) (Tolerance 0.03)

writeMask
    :: Math
    -> CompSettings k
    -> RoutesLookupTaskDistance
    -> TaskLeadingLookup
    -> [IxTask]
    -> [Pilot]
    -> CompInputFile
    -> (CompInputFile
        -> [IxTask]
        -> [Pilot]
        -> IO
            [
                [Either
                    (Pilot, TrackFileFail)
                    (Pilot, Pilot -> FlightStats k)
                ]
            ])
    -> IO ()
writeMask
    math
    CompSettings
        { comp = Comp{earthMath, give}
        , nominal = Cmp.Nominal{free}
        , tasks
        , pilotGroups
        }
    routes
    lookupTaskLeading
    selectTasks selectPilots compFile f = do

    checks <-
        catchIO
            (Just <$> f compFile selectTasks selectPilots)
            (const $ return Nothing)

    case checks of
        Nothing -> putStrLn "Unable to read tracks for pilots."

        Just fss -> do

            let iTasks = IxTask <$> [1 .. length fss]

            -- Task lengths (ls).
            let lsTask' = Lookup.compRoutes routes iTasks
            let lsWholeTask = (fmap . fmap) wholeTaskDistance lsTask'

            let yss = maskPilots free tasks lsTask' pilotGroups fss

            -- Distances (ds) of the landout spot.
            let dsLand :: [[(Pilot, TrackDistance Land)]] = landDistances <$> yss
            let psLandingOut = (fmap . fmap) fst dsLand

            -- Arrivals (as).
            let as :: [[(Pilot, TrackArrival)]] = arrivals <$> yss
            let psArriving = (fmap . fmap) fst as

            let pilots =
                    [ pAs ++ pLs
                    | pAs <- psArriving
                    | pLs <- psLandingOut
                    ]

            -- Zones (zs) of the task and zones ticked.
            let zsTaskTicked :: [Map Pilot _] = Map.fromList . landTaskTicked <$> yss

            let (gsBestTime, maskSpeed') = maskSpeed lsTask' yss
            let raceTimes' = raceTimes lookupTaskLeading iTasks tasks

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

            -- For each task, for each pilot, the row closest to goal.
            nullAltRowsBest :: [[Maybe (Pilot, Time.TickRow)]]
                <- readCompBestDistances
                    (AltBonus False)
                    compFile
                    (includeTask selectTasks)
                    ((fmap . fmap) fst dsLand)

            bonusAltRowsBest :: [[Maybe (Pilot, Time.TickRow)]]
                <- readCompBestDistances
                    (AltBonus True)
                    compFile
                    (includeTask selectTasks)
                    ((fmap . fmap) fst dsLand)

            let (dsNullAltBest, nullAltRowTicks, nullAltLead) =
                    maskLead
                        free
                        tasks
                        raceTimes'
                        lsTask'
                        psArriving
                        psLandingOut
                        gsBestTime
                        nullAltRowsBest
                        nullAltRows

            let (dsBonusAltBest, _, _) =
                    maskLead
                        free
                        tasks
                        raceTimes'
                        lsTask'
                        psArriving
                        psLandingOut
                        gsBestTime
                        bonusAltRowsBest
                        bonusAltRows

            dsNullAltNighRows :: [[Maybe (Pilot, Time.TimeRow)]]
                <- readCompTimeRows
                        compFile
                        (includeTask selectTasks)
                        (catMaybes <$> nullAltRowTicks)

            let dsBonusAltNighRows = bonusAltRowsBest

            -- NOTE: For time and leading points do not use altitude bonus distances.
            writeMaskingSpeed (compToMaskSpeed compFile) maskSpeed'
            writeMaskingLead (compToMaskLead compFile) nullAltLead

            -- REVIEW: Waiting on feedback on GAP rule question about altitude
            -- bonus distance pushing a flight to goal so that it arrives.
            writeMaskingArrival (compToMaskArrival compFile) (maskArrival as)

            -- TODO: Use altitude bonus distance for effort.
            writeMaskingEffort
                (compToMaskEffort compFile)
                (maskEffort dsNullAltBest dsLand)

            let dfNtReach :: [[(Pilot, TrackReach)]] =
                    [
                        (fmap . fmap) Gap.flown $
                        dfNoTrackReach (TaskDistance $ MkQuantity td)
                        <$> dfnts
                    | dfnts <- unDfNoTrack . didFlyNoTracklog <$> pilotGroups
                    | td <- maybe 0 unTaskDistanceAsKm <$> lsWholeTask
                    ]

            -- NOTE: The reach without altitude bonus distance.
            writeMaskingReach
                (compToMaskReach compFile)
                (maskReachTime
                    math
                    earthMath
                    give
                    free
                    dfNtReach
                    lsWholeTask
                    zsTaskTicked
                    dsNullAltBest
                    dsNullAltNighRows
                    psArriving)

            -- NOTE: The reach with altitude bonus distance.
            writeBonusReach
                (compToBonusReach compFile)
                (maskReachTick
                    free
                    dfNtReach
                    lsWholeTask
                    zsTaskTicked
                    dsBonusAltBest
                    dsBonusAltNighRows
                    psArriving)

includeTask :: [IxTask] -> IxTask -> Bool
includeTask tasks = if null tasks then const True else (`elem` tasks)

landTaskTicked :: [(Pilot, FlightStats k)] -> [(Pilot, _)]
landTaskTicked xs =
    (\(p, FlightStats{..}) -> (p, statDash)) <$> xs

check
    :: (MonadThrow m, MonadIO m)
    => Math
    -> RoutesLookupTaskDistance
    -> ScoredLookup
    -> Maybe Tagging
    -> CompInputFile
    -> [IxTask]
    -> [Pilot]
    -> m [[Either (Pilot, TrackFileFail) (Pilot, Pilot -> FlightStats k)]]
check math lengths flying tags =
    checkTracks $ \CompSettings{tasks, comp = Comp{earthMath, give}} ->
        flown math earthMath give lengths flying tags tasks

flown
    :: Math
    -> EarthMath
    -> Maybe Give
    -> RoutesLookupTaskDistance
    -> ScoredLookup
    -> Maybe Tagging
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
    -> Maybe Tagging
    -> FnIxTask k (Pilot -> FlightStats k)
flown' _ _ Rational _ _ _ _ _ _ _ = error "Nigh for rationals not yet implemented."
flown' dTaskF flying Floating earthMath give tags tasks iTask@(IxTask i) mf@MarkedFixes{mark0} p =
    case maybeTask of
        Nothing -> nullStats

        Just task' ->
            case (ssTime, gsTime, esTime, arrivalRank) of
                (Just a, Just b, Just e, c@(Just _)) ->
                    tickedStats {statTimeRank = Just $ TimeStats a b e c}

                _ ->
                    tickedStats
                        { statLand = Just $ landDistance task'
                        , statAlt =
                            case reverse ys of
                                [] -> Nothing
                                y : _ -> Just . altToAlt $ altGps y
                        }

    where
        maybeTask = tasks ^? element (i - 1)

        ticked = Lookup.ticked (tagTicked tags) mf iTask speedSection' p
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

        tickedStats =
            nullStats
                { statDash =
                    DashPathInputs
                        { dashTask = maybeTask
                        , dashTicked = ticked
                        , dashFlyCut = Just xs
                        }
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

        startGates' =
            case tasks ^? element (i - 1) of
                Nothing -> []
                Just Task{..} -> startGates

        speedSection' =
            case tasks ^? element (i - 1) of
                Nothing -> Nothing
                Just Task{..} -> speedSection
