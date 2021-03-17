{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Mask (writeMask) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList)
import Control.Lens ((^?), element)
import Control.Monad.Except (runExceptT)
import Control.Concurrent.ParallelIO (parallel)
import Data.UnitsOfMeasure (KnownUnit, Unpack, u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone.Cylinder (SampleParams(..), Samples(..), Tolerance(..))
import Flight.Zone.Raw (Give)
import Flight.Earth.Ellipsoid (wgs84)
import Flight.Earth.Sphere (earthRadius)
import Flight.Geodesy (EarthMath(..), EarthModel(..), Projection(..))
import Flight.Clip (FlyCut(..), FlyClipping(..))
import qualified Flight.Comp as Cmp (Nominal(..))
import Flight.Comp
    ( ScoringInputFiles
    , CompTaskSettings(..)
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
    )
import Flight.Distance (TaskDistance(..), QTaskDistance, unTaskDistanceAsKm)
import Flight.Mask (GeoDash(..), FnIxTask, settingsLogs)
import Flight.Track.Tag (CompTagging)
import Flight.Track.Lead (LeadingAreaSum, MkLeadingCoef, MkAreaToCoef)
import qualified Flight.Track.Time as Time (TickRow(..))
import Flight.Track.Mask (CompMaskingArrival(..))
import Flight.Track.Distance (TrackDistance(..), TrackReach(..), Effort)
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
import Flight.TrackLog (pilotTrack)
import Flight.Scribe
    ( AltBonus(..)
    , writeCompMaskReachStop
    , readCompBestDistances
    -- TODO: Take care to consider bonus altitude distance with leading area.
    -- , readPilotDiscardFurther
    -- , readPilotDiscardFurtherStop
    , readCompLeadArea
    )
import qualified "flight-gap-valid" Flight.Score as Gap (ReachToggle(..))
import Flight.Span.Math (Math(..))
import Stats (TimeStats(..), FlightStats(..), DashPathInputs(..), nullStats, altToAlt)
import MaskEffort (landDistances)
import MaskLead (raceTimes)
import MaskLeadCoef (maskLeadCoef)
import Mask.Reach.Tick (maskReachTick)
import MaskSpeed (maskSpeedBestTime)
import MaskPilots (maskPilots)

sp :: SampleParams Double
sp = SampleParams (replicate 6 $ Samples 11) (Tolerance 0.03)

type IOStep k = Either (Pilot, TrackFileFail) (Pilot, Pilot -> FlightStats k)

writeMask
    :: (KnownUnit (Unpack u), KnownUnit (Unpack v))
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
            sequence $
            [
                parallel $
                [ runExceptT $ pilotTrack (flown math earthMath give routes flying tags tasks ixTask) pilotLog
                | pilotLog <- taskLogs
                ]

            | ixTask <- IxTask <$> [1..]
            | taskLogs <- selectedCompLogs
            ]

    let iTasks = IxTask <$> [1 .. length fss]

    -- Task lengths (ls).
    let lsTask' = Lookup.compRoutes routes iTasks
    let lsWholeTask = (fmap . fmap) wholeTaskDistance lsTask'

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
    let zsTaskTicked :: [Map Pilot _] = Map.fromList . landTaskTicked <$> yss

    let gsBestTime = maskSpeedBestTime yss
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
            [ sequence [sequence (p, readPilotDiscardFurtherStop compFile ix p) | p <- ps]
            | ix <- (IxTask <$> [1 .. ])
            | ps <- pilots
            ]
    -}

    -- For each task, for each pilot, the row closest to goal.
    bonusAltRowsBest :: [[Maybe (Pilot, Time.TickRow)]]
        <- readCompBestDistances
            (AltBonus True)
            compFile
            (includeTask ixSelectTasks)
            ((fmap . fmap) fst dsLand)

    leading <- readCompLeadArea compFile

    let (dsBonusAltBest, _, _) =
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
                bonusAltRowsBest
                -- TODO: Use bonus altitude calculating leading.
                leading

    let dsBonusAltNighRows = bonusAltRowsBest

    let dfNtReach :: [[(Pilot, TrackReach)]] =
            [
                (fmap . fmap) Gap.flown $
                dfNoTrackReach (TaskDistance $ MkQuantity td)
                <$> dfnts
            | dfnts <- unDfNoTrack . didFlyNoTracklog <$> pilotGroups
            | td <- maybe 0 unTaskDistanceAsKm <$> lsWholeTask
            ]

    -- NOTE: The reach with altitude bonus distance.
    writeCompMaskReachStop
        compFile
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
                    tickedStats {statTimeRank = Just $ TimeStats a b e c}

                _ ->
                    tickedStats
                        { statEffort= Just $ landDistance task'
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
