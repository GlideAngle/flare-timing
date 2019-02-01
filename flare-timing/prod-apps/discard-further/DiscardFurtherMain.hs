{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import Data.List (zipWith4)
import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (join, mapM_, when)
import Control.Exception.Safe (catchIO)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName)

import Flight.Clip (FlyCut(..), FlyClipping(..))
import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import qualified Flight.Comp as Cmp (openClose)
import Flight.Route (OptimalRoute(..))
import Flight.Comp
    ( FileType(CompInput)
    , DiscardFurtherDir(..)
    , AlignTimeDir(..)
    , CompInputFile(..)
    , TagZoneFile(..)
    , TaskLengthFile(..)
    , AlignTimeFile(..)
    , DiscardFurtherFile(..)
    , CompSettings(..)
    , TaskStop(..)
    , Task(..)
    , PilotName(..)
    , Pilot(..)
    , TrackFileFail
    , IxTask(..)
    , StartEnd(..)
    , StartEndMark
    , RoutesLookupTaskDistance(..)
    , TaskRouteDistance(..)
    , FirstLead(..)
    , FirstStart(..)
    , LastArrival(..)
    , compFileToCompDir
    , compToTaskLength
    , compToCross
    , crossToTag
    , discardFurtherDir
    , alignTimePath
    , findCompInput
    , speedSectionToLeg
    , ensureExt
    , pilotNamed
    )
import Flight.Track.Time (LeadClose(..), LeadArrival(..), discard, allHeaders)
import Flight.Track.Mask (RaceTime(..), racing)
import Flight.Mask (checkTracks)
import Flight.Scribe
    (readComp, readRoute, readTagging, readAlignTime, writeDiscardFurther)
import Flight.Lookup.Route (routeLength)
import Flight.Lookup.Tag (TaskTimeLookup(..), tagTaskTime)
import Flight.Score (Leg(..))
import DiscardFurtherOptions (description)

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
                (routeLength taskRoute taskRouteSpeedSubset routes)
                (tagTaskTime tagging)
                compFile
                (IxTask <$> task)
                (pilotNamed cs $ PilotName <$> pilot)
                checkAll

filterTime
    :: CompSettings k
    -> RoutesLookupTaskDistance
    -> TaskTimeLookup
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
    lengths
    (TaskTimeLookup lookupTaskTime)
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

            let raceStartEnd :: [Maybe StartEndMark] =
                    join <$>
                    [ ($ s) . ($ i) <$> lookupTaskTime
                    | i <- iTasks
                    | s <- speedSection <$> tasks
                    ]

            let raceFirstLead :: [Maybe FirstLead] =
                    (fmap . fmap) (FirstLead . unStart) raceStartEnd

            let raceFirstStart :: [Maybe FirstStart] =
                    (fmap . fmap) (FirstStart . unStart) raceStartEnd

            let raceLastArrival :: [Maybe LastArrival] =
                    join
                    <$> (fmap . fmap) (fmap LastArrival . unEnd) raceStartEnd

            let compRaceTimes :: [Maybe RaceTime] =
                    [ racing (Cmp.openClose ss zt) fl fs la
                    | ss <- speedSection <$> tasks
                    | zt <- zoneTimes <$> tasks
                    | fl <- raceFirstLead
                    | fs <- raceFirstStart
                    | la <- raceLastArrival
                    ]

            let raceTime =
                    [ do
                        rt@RaceTime{..} <- crt
                        return $
                            maybe
                                rt
                                (\stp ->
                                    uncut . clipToFlown $
                                        FlyCut
                                            { cut = Just (openTask, min stp closeTask)
                                            , uncut = rt
                                            })
                                (retroactive <$> stopped task)

                    | crt <- compRaceTimes
                    | task <- tasks
                    ]

            sequence_ $ zipWith4
                (\ n toLeg rt pilots ->
                        mapM_
                        (readFilterWrite
                            lengths
                            compFile
                            (includeTask selectTasks)
                            n
                            toLeg
                            rt)
                        pilots)
                (IxTask <$> [1 .. ])
                (speedSectionToLeg . speedSection <$> tasks)
                raceTime
                taskPilots

checkAll
    :: CompInputFile
    -> [IxTask]
    -> [Pilot]
    -> IO
         [
             [Either (Pilot, TrackFileFail) (Pilot, ())]
         ]
checkAll = checkTracks $ \CompSettings{tasks} -> (\ _ _ _ -> ()) tasks

includeTask :: [IxTask] -> IxTask -> Bool
includeTask tasks = if null tasks then const True else (`elem` tasks)

readFilterWrite
    :: RoutesLookupTaskDistance
    -> CompInputFile
    -> (IxTask -> Bool)
    -> IxTask
    -> (Int -> Leg)
    -> Maybe RaceTime
    -> Pilot
    -> IO ()
readFilterWrite _ _ _ _ _ Nothing _ = return ()
readFilterWrite
    (RoutesLookupTaskDistance lookupTaskLength)
    compFile
    selectTask
    iTask@(IxTask i) toLeg (Just raceTime) pilot =
    when (selectTask iTask) $ do
    _ <- createDirectoryIfMissing True dOut
    rows <- readAlignTime (AlignTimeFile (dIn </> file))
    f . discard toLeg taskLength close arrival . snd $ rows
    where
        f = writeDiscardFurther (DiscardFurtherFile $ dOut </> file) allHeaders
        dir = compFileToCompDir compFile
        (AlignTimeDir dIn, AlignTimeFile file) = alignTimePath dir i pilot
        (DiscardFurtherDir dOut) = discardFurtherDir dir i
        taskLength = (fmap wholeTaskDistance . ($ iTask)) =<< lookupTaskLength
        close = LeadClose <$> leadClose raceTime
        arrival = LeadArrival <$> leadArrival raceTime
