{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import Prelude hiding (last)
import Data.Maybe (fromMaybe)
import Data.List (find)
import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_, void)
import Control.Exception.Safe (catchIO)
import System.Directory (getCurrentDirectory)
import Control.Concurrent.ParallelIO (parallel_)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import qualified Flight.Comp as DFNT (DfNoTrack(..), DfNoTrackPilot(..))
import Flight.Comp
    ( FindDirFile(..)
    , FileType(CompInput)
    , ScoringInputFiles
    , CompInputFile(..)
    , CompTaskSettings(..)
    , Task(..)
    , PilotName(..)
    , Pilot(..)
    , PilotGroup(..)
    , PilotTrackLogFile(..)
    , TrackFileFail
    , IxTask(..)
    , findCompInput
    , reshape
    , pilotNamed
    , mkCompTaskSettings
    , compFileToTaskFiles
    )
import Flight.Track.Time (TimeRow(..), copyTimeToTick)
import Flight.Track.Stop (CompFraming(..), StopFraming(..), TrackScoredSection(..))
import Flight.Mask (checkTracks)
import Flight.Scribe
    ( readCompAndTasks, readCompPegFrame
    , readPilotAlignTimeWriteDiscardFurther
    )
import DiscardFurtherOptions (description)

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing

    let lf = LenientFile {coerceFile = reshape CompInput}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdBatchOptions -> IO ()
drive o@CmdBatchOptions{file} = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    cwd <- getCurrentDirectory
    files <- findCompInput $ FindDirFile {dir = cwd, file = file}
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ (go o) files
    end <- getTime Monotonic
    fprint ("Filtering times completed in " % timeSpecs % "\n") start end

go :: CmdBatchOptions -> CompInputFile -> IO ()
go CmdBatchOptions{pilot = p, ..} compFile = do
    filesTaskAndSettings <-
        catchIO
            (Just <$> do
                ts <- compFileToTaskFiles compFile
                putStrLn $ "TASKS: >>>: " ++ show ts
                s <- readCompAndTasks (compFile, ts)
                return (ts, s))
            (const $ return Nothing)

    stopping <-
        catchIO
            (Just <$> readCompPegFrame compFile)
            (const $ return Nothing)

    case (filesTaskAndSettings, stopping) of
        (Nothing, _) -> putStrLn "Couldn't read the comp settings."
        (_, Nothing) -> putStrLn "Couldn't read the scored frames."
        (Just (taskFiles, settings@(cs, _)), Just CompFraming{stopFlying}) ->
            filterTime
                (uncurry mkCompTaskSettings $ settings)
                (compFile, taskFiles)
                (IxTask <$> task)
                (pilotNamed cs $ PilotName <$> p)
                stopFlying
                checkAll

filterTimeRow :: StopFraming -> TimeRow -> Bool
filterTimeRow StopFraming{stopScored} TimeRow{time = t} = fromMaybe True $ do
    TrackScoredSection{scoredTimes} <- stopScored
    (t0, t1) <- scoredTimes
    return $ t0 <= t && t <= t1

filterTime
    :: CompTaskSettings k
    -> ScoringInputFiles
    -> [IxTask]
    -> [Pilot]
    -> [[(Pilot, StopFraming)]]
    -> (ScoringInputFiles
        -> [IxTask]
        -> [Pilot]
        -> IO [[Either (Pilot, _) (Pilot, _)]])
    -> IO ()
filterTime
    CompTaskSettings{tasks, pilots, pilotGroups}
    inFiles@(compFile, _) selectTasks selectPilots stopFlying f = do

    let filterOnPilotStops pilot stops =
            (maybe
                (const True)
                (filterTimeRow . snd)
                (find ((==) pilot . fst) stops))

    checks <-
        catchIO
            (Just <$> f inFiles selectTasks selectPilots)
            (const $ return Nothing)

    case checks of
        Nothing -> putStrLn "Unable to read tracks for pilots."
        Just _xs -> do
            {-
            let taskPilots :: [[Pilot]] =
                    (fmap . fmap)
                        (\case
                            Left (p, _) -> p
                            Right (p, _) -> p)
                        xs
                        -}

            sequence_
                [
                    if | cancelled -> putStrLn $ "TASK-" ++ show n ++ " CANCELLED"
                       | not (includeTask selectTasks ixTask) -> putStrLn $ "TASK-" ++ show n ++ " EXCLUDED"
                       | otherwise -> do
                            putStrLn $ "TASK-" ++ show n ++ " == " ++ taskName
                            putStrLn $ "TASK-" ++ show n ++ " ABS: " ++ show absent
                            putStrLn $ "TASK-" ++ show n ++ " DNF: " ++ show dnf
                            putStrLn $ "TASK-" ++ show n ++ " DFNT: " ++ show dfnt

                            parallel_
                                [
                                    if | p `elem` absent -> putStrLn $ "TASK-" ++ show n ++ " ABS " ++ show p
                                       | p `elem` dnf -> putStrLn $ "TASK-" ++ show n ++ " DNF " ++ show p
                                       | p `elem` dfnt -> putStrLn $ "TASK-" ++ show n ++ " DFNoTrack " ++ show p
                                       | otherwise -> void $
                                           readPilotAlignTimeWriteDiscardFurther
                                                compFile
                                                ixTask
                                                p
                                                copyTimeToTick
                                                id
                                                (filterOnPilotStops p stops)
                                | PilotTrackLogFile p _ <- taskPilots
                                ]
                | ixTask@(IxTask n) <- (IxTask <$> [1 .. ])
                | Task{taskName, cancelled} <- tasks
                | taskPilots <- pilots
                | PilotGroup{absent, dnf, didFlyNoTracklog} <- pilotGroups
                , let dfnt = DFNT.pilot <$> DFNT.unDfNoTrack didFlyNoTracklog
                | stops <- stopFlying
                ]

checkAll
    :: ScoringInputFiles
    -> [IxTask]
    -> [Pilot]
    -> IO [[Either (Pilot, TrackFileFail) (Pilot, ())]]
checkAll = checkTracks $ \CompTaskSettings{tasks} -> (\ _ _ _ -> ()) tasks

includeTask :: [IxTask] -> IxTask -> Bool
includeTask tasks = if null tasks then const True else (`elem` tasks)
