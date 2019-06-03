{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Data.Maybe (catMaybes)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Lens ((^?), element)
import Control.Exception.Safe (MonadThrow, catchIO)
import Control.Monad.Except (MonadIO)
import Data.UnitsOfMeasure (u)
import System.FilePath (takeFileName)

import Flight.Clip (FlyCut(..), FlyClipping(..))
import Flight.Route (OptimalRoute(..))
import qualified Flight.Comp as Cmp (Nominal(..))
import Flight.Comp
    ( FileType(CompInput)
    , CompInputFile(..)
    , TaskLengthFile(..)
    , TagZoneFile(..)
    , PegFrameFile(..)
    , CompSettings(..)
    , PilotName(..)
    , Pilot(..)
    , Task(..)
    , IxTask(..)
    , TrackFileFail(..)
    , RoutesLookupTaskDistance(..)
    , TaskRouteDistance(..)
    , compToTaskLength
    , compToCross
    , compToMaskArrival
    , compToMaskEffort
    , compToMaskLead
    , compToMaskReach
    , compToMaskSpeed
    , crossToTag
    , tagToPeg
    , findCompInput
    , speedSectionToLeg
    , ensureExt
    , pilotNamed
    )
import Flight.Distance (QTaskDistance)
import Flight.Mask
    ( FnIxTask
    , checkTracks
    , togoAtLanding
    , madeAtLanding
    )
import Flight.Track.Tag (Tagging)
import Flight.Track.Time (TimeToTick, TickToTick, copyTimeToTick)
import qualified Flight.Track.Time as Time (TimeRow(..), TickRow(..))
import Flight.Track.Arrival (TrackArrival(..))
import Flight.Track.Distance (TrackDistance(..), Land)
import Flight.Kml (LatLngAlt(..), MarkedFixes(..))
import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Lookup.Stop (ScoredLookup(..), stopFlying)
import qualified Flight.Lookup as Lookup
    ( scoredTimeRange, arrivalRank, ticked, compRoutes
    , pilotTime, pilotEssTime
    )
import Flight.Lookup.Tag
    ( TaskLeadingLookup(..)
    , tagTaskLeading
    , tagArrivalRank
    , tagPilotTime
    , tagTicked
    )
import Flight.Scribe
    ( readComp, readRoute, readTagging, readFraming
    , writeMaskingArrival
    , writeMaskingEffort
    , writeMaskingLead
    , writeMaskingReach
    , writeMaskingSpeed
    , readCompLeading, readCompBestDistances, readCompTimeRows
    )
import Flight.Lookup.Route (routeLength)
import Flight.Span.Math (Math(..))
import MaskTrackOptions (description)
import Stats (TimeStats(..), FlightStats(..), DashPathInputs(..), nullStats, altToAlt)
import MaskArrival (maskArrival, arrivals)
import MaskEffort (maskEffort, landDistances)
import MaskLead (maskLead, raceTimes)
import MaskReach (maskReach)
import MaskSpeed (maskSpeed)
import MaskPilots (maskPilots)

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing

    let lf = LenientFile {coerceFile = ensureExt CompInput}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdBatchOptions -> IO ()
drive o = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    files <- findCompInput o
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ (go o) files
    end <- getTime Monotonic
    fprint ("Masking tracks completed in " % timeSpecs % "\n") start end

go :: CmdBatchOptions -> CompInputFile -> IO ()
go CmdBatchOptions{..} compFile@(CompInputFile compPath) = do
    let lenFile@(TaskLengthFile lenPath) = compToTaskLength compFile
    let tagFile@(TagZoneFile tagPath) = crossToTag . compToCross $ compFile
    let stopFile@(PegFrameFile stopPath) = tagToPeg tagFile
    putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading task length from '" ++ takeFileName lenPath ++ "'"
    putStrLn $ "Reading zone tags from '" ++ takeFileName tagPath ++ "'"
    putStrLn $ "Reading scored times from '" ++ takeFileName stopPath ++ "'"

    compSettings <-
        catchIO
            (Just <$> readComp compFile)
            (const $ return Nothing)

    tagging <-
        catchIO
            (Just <$> readTagging tagFile)
            (const $ return Nothing)

    stopping <-
        catchIO
            (Just <$> readFraming stopFile)
            (const $ return Nothing)

    routes <-
        catchIO
            (Just <$> readRoute lenFile)
            (const $ return Nothing)

    let scoredLookup = stopFlying stopping
    let lookupTaskLength = routeLength taskRoute taskRouteSpeedSubset stopRoute routes

    case (compSettings, tagging, stopping, routes) of
        (Nothing, _, _, _) -> putStrLn "Couldn't read the comp settings."
        (_, Nothing, _, _) -> putStrLn "Couldn't read the taggings."
        (_, _, Nothing, _) -> putStrLn "Couldn't read the scored frame."
        (_, _, _, Nothing) -> putStrLn "Couldn't read the routes."
        (Just cs, Just _, Just _, Just _) ->
            writeMask
                cs
                lookupTaskLength
                (tagTaskLeading tagging)
                (IxTask <$> task)
                (pilotNamed cs $ PilotName <$> pilot)
                compFile
                (check math lookupTaskLength scoredLookup tagging)

writeMask
    :: CompSettings k
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
    CompSettings
        { nominal = Cmp.Nominal{free}
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

            -- For each task, for each pilot, the row closest to goal.
            rows :: [[Maybe (Pilot, Time.TickRow)]]
                <- readCompBestDistances
                    compFile
                    (includeTask selectTasks)
                    ((fmap . fmap) fst dsLand)

            -- NOTE: Leading point calculations use the reach without altitude
            -- bonus applied.
            let nullAltBonuses :: [TimeToTick] = const copyTimeToTick <$> tasks
            let nullTickToTicks :: [TickToTick] = const id <$> tasks

            let (gsBestTime, maskSpeed') = maskSpeed lsTask' yss
            let raceTimes' = raceTimes lookupTaskLeading iTasks tasks

            rowsLeadingStep :: [[(Pilot, [Time.TickRow])]]
                <- readCompLeading
                        nullAltBonuses
                        nullTickToTicks
                        routes
                        compFile
                        (includeTask selectTasks)
                        (IxTask <$> [1 .. ])
                        (speedSectionToLeg . speedSection <$> tasks)
                        raceTimes'
                        pilots

            let (dsBest, rowTicks, maskLead') =
                    maskLead
                        free
                        tasks
                        raceTimes'
                        lsTask'
                        psArriving
                        psLandingOut
                        gsBestTime
                        rows
                        rowsLeadingStep

            dsNighRows :: [[Maybe (Pilot, Time.TimeRow)]]
                <- readCompTimeRows
                        compFile
                        (includeTask selectTasks)
                        (catMaybes <$> rowTicks)

            writeMaskingArrival (compToMaskArrival compFile) (maskArrival as)

            writeMaskingEffort
                (compToMaskEffort compFile)
                (maskEffort dsBest dsLand)

            writeMaskingLead (compToMaskLead compFile) maskLead'

            writeMaskingReach
                (compToMaskReach compFile)
                (maskReach free lsWholeTask zsTaskTicked dsBest dsNighRows psArriving)

            writeMaskingSpeed (compToMaskSpeed compFile) maskSpeed'

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
check math lengths flying tags = checkTracks $ \CompSettings{tasks} ->
    flown math lengths flying tags tasks

flown
    :: Math
    -> RoutesLookupTaskDistance
    -> ScoredLookup
    -> Maybe Tagging
    -> FnIxTask k (Pilot -> FlightStats k)
flown math (RoutesLookupTaskDistance lookupTaskLength) flying tags tasks iTask fixes =
    maybe
        (const nullStats)
        (\d -> flown' d flying math tags tasks iTask fixes)
        taskLength
    where
        taskLength = (fmap wholeTaskDistance . ($ iTask)) =<< lookupTaskLength

flown'
    :: QTaskDistance Double [u| m |]
    -> ScoredLookup
    -> Math
    -> Maybe Tagging
    -> FnIxTask k (Pilot -> FlightStats k)
flown' dTaskF flying math tags tasks iTask@(IxTask i) mf@MarkedFixes{mark0} p =
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
            TrackDistance
                { togo = togoAtLanding math ticked task xs
                , made = madeAtLanding math dTaskF ticked task xs
                }

        startGates' =
            case tasks ^? element (fromIntegral i - 1) of
                Nothing -> []
                Just Task{..} -> startGates

        speedSection' =
            case tasks ^? element (fromIntegral i - 1) of
                Nothing -> Nothing
                Just Task{..} -> speedSection
