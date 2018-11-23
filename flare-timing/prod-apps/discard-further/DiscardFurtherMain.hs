{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import Data.List (zipWith4)
import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (join, mapM_, when)
import Control.Monad.Except (ExceptT, runExceptT)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName)
import Data.Yaml (ParseException, prettyPrintParseException)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import qualified Flight.Comp as Cmp (openClose)
import Flight.Comp
    ( FileType(CompInput)
    , DiscardDir(..)
    , AlignDir(..)
    , CompInputFile(..)
    , TagZoneFile(..)
    , TaskLengthFile(..)
    , AlignTimeFile(..)
    , DiscardFurtherFile(..)
    , CompSettings(..)
    , Task(..)
    , PilotName(..)
    , Pilot(..)
    , TrackFileFail
    , IxTask(..)
    , StartEnd(..)
    , StartEndMark
    , RouteLookup(..)
    , FirstLead(..)
    , FirstStart(..)
    , LastArrival(..)
    , compFileToCompDir
    , compToTaskLength
    , compToCross
    , crossToTag
    , discardDir
    , alignPath
    , findCompInput
    , speedSectionToLeg
    , ensureExt
    , pilotNamed
    )
import Flight.Track.Time (LeadClose(..), LeadArrival(..), discard)
import Flight.Track.Mask (RaceTime(..), racing)
import Flight.Mask (checkTracks)
import Flight.Scribe
    (readComp, readRoute, readTagging, readAlignTime, writeDiscardFurther)
import Flight.Lookup.Route (routeLength)
import Flight.Lookup.Tag (TaskTimeLookup(..), tagTaskTime)
import Flight.Score (Leg(..))
import DiscardFurtherOptions (description)

headers :: [String]
headers = ["leg", "tickLead", "tickRace", "distance", "area"]

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

    compSettings <- runExceptT $ readComp compFile
    tagging <- runExceptT $ readTagging tagFile
    routes <- runExceptT $ readRoute lenFile

    let ppr = putStrLn . prettyPrintParseException

    case (compSettings, tagging, routes) of
        (Left e, _, _) -> ppr e
        (_, Left e, _) -> ppr e
        (_, _, Left e) -> ppr e
        (Right cs, Right _, Right _) ->
            filterTime
                cs
                (routeLength routes)
                (tagTaskTime tagging)
                compFile
                (IxTask <$> task)
                (pilotNamed cs $ PilotName <$> pilot)
                checkAll

filterTime
    :: CompSettings k
    -> RouteLookup
    -> TaskTimeLookup
    -> CompInputFile
    -> [IxTask]
    -> [Pilot]
    -> (CompInputFile
        -> [IxTask]
        -> [Pilot]
        -> ExceptT
            ParseException
            IO [[Either (Pilot, _) (Pilot, _)]])
    -> IO ()
filterTime
    CompSettings{tasks}
    lengths
    (TaskTimeLookup lookupTaskTime)
    compFile selectTasks selectPilots f = do

    checks <- runExceptT $ f compFile selectTasks selectPilots

    case checks of
        Left msg -> print msg
        Right xs -> do
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

            let raceTime :: [Maybe RaceTime] =
                    [ racing (Cmp.openClose ss zt) fl fs la
                    | ss <- speedSection <$> tasks
                    | zt <- zoneTimes <$> tasks
                    | fl <- raceFirstLead
                    | fs <- raceFirstStart
                    | la <- raceLastArrival
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
    -> ExceptT
         ParseException
         IO
         [
             [Either (Pilot, TrackFileFail) (Pilot, ())]
         ]
checkAll = checkTracks $ \CompSettings{tasks} -> (\ _ _ _ -> ()) tasks

includeTask :: [IxTask] -> IxTask -> Bool
includeTask tasks = if null tasks then const True else (`elem` tasks)

readFilterWrite
    :: RouteLookup
    -> CompInputFile
    -> (IxTask -> Bool)
    -> IxTask
    -> (Int -> Leg)
    -> Maybe RaceTime
    -> Pilot
    -> IO ()
readFilterWrite _ _ _ _ _ Nothing _ = return ()
readFilterWrite
    (RouteLookup lookupTaskLength)
    compFile
    selectTask
    iTask@(IxTask i) toLeg (Just raceTime) pilot =
    when (selectTask iTask) $ do
    _ <- createDirectoryIfMissing True dOut
    rows <- runExceptT $ readAlignTime (AlignTimeFile (dIn </> file))
    either print (f . discard toLeg taskLength close arrival . snd) rows
    where
        f = writeDiscardFurther (DiscardFurtherFile $ dOut </> file) headers
        dir = compFileToCompDir compFile
        (AlignDir dIn, AlignTimeFile file) = alignPath dir i pilot
        (DiscardDir dOut) = discardDir dir i
        taskLength = ($ iTask) =<< lookupTaskLength
        close = LeadClose <$> leadClose raceTime
        arrival = LeadArrival <$> leadArrival raceTime
