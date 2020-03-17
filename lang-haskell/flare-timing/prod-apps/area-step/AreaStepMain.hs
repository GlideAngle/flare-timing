{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import Prelude hiding (last)
import Data.Maybe (fromMaybe, catMaybes)
import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (join, mapM_)
import Control.Exception.Safe (catchIO)
import System.FilePath (takeFileName)
import Data.Vector (Vector)
import qualified Data.Vector as V (toList)
import Data.UnitsOfMeasure (u)

import Flight.Clip (FlyCut(..), FlyClipping(..))
import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import qualified Flight.Comp as Cmp (openClose)
import Flight.Route (OptimalRoute(..))
import Flight.Comp
    ( FileType(CompInput)
    , CompInputFile(..)
    , TagZoneFile(..)
    , TaskLengthFile(..)
    , CompSettings(..)
    , TaskStop(..)
    , Task(..)
    , PilotName(..)
    , Pilot(..)
    , TrackFileFail
    , IxTask(..)
    , StartEndDown(..)
    , StartEndDownMark
    , RoutesLookupTaskDistance(..)
    , TaskRouteDistance(..)
    , FirstLead(..)
    , FirstStart(..)
    , LastArrival(..)
    , LastDown(..)
    , compToTaskLength
    , compToCross
    , compToLeadArea
    , crossToTag
    , findCompInput
    , speedSectionToLeg
    , ensureExt
    , pilotNamed
    )
import Flight.Track.Time
    ( LeadingAreas(..), LeadAllDown(..), AreaRow
    , taskToLeading
    , leading2AreaFlown, leading2AreaAfterLanding, leading2AreaBeforeStart
    )
import Flight.Track.Lead (DiscardingLead(..))
import Flight.Track.Mask (RaceTime(..), racing)
import Flight.Mask (checkTracks)
import Flight.Scribe
    ( readComp, readRoute, readTagging
    , writeCompAreaStep
    , readCompLeading, writeDiscardingLead
    )
import Flight.Score (LeadingArea(..), LcPoint, LeadingArea2Units, area2Steps)
import qualified Flight.Lookup as Lookup (compRoutes)
import Flight.Lookup.Route (routeLength)
import Flight.Lookup.Tag (TaskLeadingLookup(..), tagTaskLeading)
import AreaStepOptions (description)

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
    fprint ("Filtering times completed in " % timeSpecs % "\n") start end

go :: CmdBatchOptions -> CompInputFile -> IO ()
go CmdBatchOptions{..} compFile@(CompInputFile compPath) = do
    let lenFile@(TaskLengthFile lenPath) = compToTaskLength compFile
    let tagFile@(TagZoneFile tagPath) = crossToTag . compToCross $ compFile
    putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading task length from '" ++ takeFileName lenPath ++ "'"
    putStrLn $ "Reading zone tags from '" ++ takeFileName tagPath ++ "'"

    compSettings <-
        catchIO
            (Just <$> readComp compFile)
            (const $ return Nothing)

    tagging <-
        catchIO
            (Just <$> readTagging tagFile)
            (const $ return Nothing)

    routes <-
        catchIO
            (Just <$> readRoute lenFile)
            (const $ return Nothing)

    case (compSettings, tagging, routes) of
        (Nothing, _, _) -> putStrLn "Couldn't read the comp settings."
        (_, Nothing, _) -> putStrLn "Couldn't read the taggings."
        (_, _, Nothing) -> putStrLn "Couldn't read the routes."
        (Just cs, Just _, Just _) ->
            filterTime
                cs
                (routeLength taskRoute taskRouteSpeedSubset stopRoute startRoute routes)
                (tagTaskLeading tagging)
                compFile
                (IxTask <$> task)
                (pilotNamed cs $ PilotName <$> pilot)
                checkAll

filterTime
    :: CompSettings k
    -> RoutesLookupTaskDistance
    -> TaskLeadingLookup
    -> CompInputFile
    -> [IxTask]
    -> [Pilot]
    -> (CompInputFile
        -> [IxTask]
        -> [Pilot]
        -> IO [[Either (Pilot, _) (Pilot, _)]])
    -> IO ()
filterTime
    CompSettings{tasks}
    routes
    (TaskLeadingLookup lookupTaskLeading)
    compFile selectTasks selectPilots f = do

    checks <-
        catchIO
            (Just <$> f compFile selectTasks selectPilots)
            (const $ return Nothing)

    case checks of
        Nothing -> putStrLn "Unable to read tracks for pilots."
        Just xs -> do
            let taskPilots :: [[Pilot]] =
                    (fmap . fmap)
                        (\case
                            Left (p, _) -> p
                            Right (p, _) -> p)
                        xs

            let iTasks = IxTask <$> [1 .. length taskPilots]
            let lsTask = Lookup.compRoutes routes iTasks
            let lsSpeedTask = (fmap . fmap) speedSubsetDistance lsTask

            let raceTs :: [Maybe StartEndDownMark] =
                    join <$>
                    [ ($ s) . ($ i) <$> lookupTaskLeading
                    | i <- iTasks
                    | s <- speedSection <$> tasks
                    ]

            let raceFirstLead :: [Maybe FirstLead] =
                    (fmap . fmap) (FirstLead . unStart) raceTs

            let raceFirstStart :: [Maybe FirstStart] =
                    (fmap . fmap) (FirstStart . unStart) raceTs

            let raceLastArrival :: [Maybe LastArrival] =
                    join
                    <$> (fmap . fmap) (fmap LastArrival . unEnd) raceTs

            let raceLastDown :: [Maybe LastDown] =
                    join
                    <$> (fmap . fmap) (fmap LastDown . unDown) raceTs

            let compRaceTimes :: [Maybe RaceTime] =
                    [ racing (Cmp.openClose ss zt) fl fs la ld
                    | ss <- speedSection <$> tasks
                    | zt <- zoneTimes <$> tasks
                    | fl <- raceFirstLead
                    | fs <- raceFirstStart
                    | la <- raceLastArrival
                    | ld <- raceLastDown
                    ]

            let raceTimes =
                    [ do
                        rt@RaceTime{..} <- crt
                        return $
                            maybe
                                rt
                                (\stp ->
                                    uncut . clipToCut $
                                        FlyCut
                                            { cut = Just (openTask, min stp closeTask)
                                            , uncut = rt
                                            })
                                (retroactive <$> stopped task)

                    | crt <- compRaceTimes
                    | task <- tasks
                    ]

            ass :: [[(Pilot, LeadingAreas (Vector AreaRow) (Maybe LcPoint))]]
                    <- readCompLeading
                        area2Steps
                        routes
                        compFile
                        (includeTask selectTasks)
                        (take (length tasks) (IxTask <$> [1 .. ]))
                        (speedSectionToLeg . speedSection <$> tasks)
                        raceTimes
                        taskPilots

            _ <- writeCompAreaStep compFile iTasks ass

            let ass' :: [[(Pilot, (LeadingAreas (LeadingArea LeadingArea2Units) (LeadingArea LeadingArea2Units)))]] =
                    [
                        catMaybes $
                        [
                            do
                                RaceTime{leadAllDown} <- rt
                                down <- leadAllDown
                                flown <- leading2AreaFlown l s $ V.toList af

                                let beforeStart =
                                        fromMaybe (LeadingArea [u| 0 (km^2)*s |]) $ do
                                            bs <- areaBeforeStart
                                            leading2AreaBeforeStart bs

                                let afterLanding =
                                        fromMaybe (LeadingArea [u| 0 (km^2)*s |]) $ do
                                            al <- areaAfterLanding
                                            leading2AreaAfterLanding (LeadAllDown down) al

                                let a' =
                                        LeadingAreas
                                            { areaFlown = flown
                                            , areaAfterLanding = afterLanding
                                            , areaBeforeStart = beforeStart
                                            }
                                return (p, a')

                        | (p, LeadingAreas{areaFlown = af, areaBeforeStart, areaAfterLanding}) <- as
                        ]
                    | as <- ass
                    | l <- (fmap . fmap) taskToLeading lsSpeedTask
                    | s <- speedSection <$> tasks
                    | rt <- raceTimes
                    ]

            writeDiscardingLead (compToLeadArea compFile) (DiscardingLead{areas = ass'})

checkAll
    :: CompInputFile
    -> [IxTask]
    -> [Pilot]
    -> IO [[Either (Pilot, TrackFileFail) (Pilot, ())]]
checkAll = checkTracks $ \CompSettings{tasks} -> (\ _ _ _ -> ()) tasks

includeTask :: [IxTask] -> IxTask -> Bool
includeTask tasks = if null tasks then const True else (`elem` tasks)
