{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import Prelude hiding (last)
import Data.Maybe (fromMaybe)
import Data.List (find)
import Data.List.NonEmpty (nonEmpty, last)
import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_)
import Control.Exception.Safe (catchIO)
import System.Directory (getCurrentDirectory)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Zone.MkZones (Zones(..))
import Flight.Zone.Raw (RawZone(..))
import Flight.Comp
    ( FindDirFile(..)
    , FileType(CompInput)
    , ScoringInputFiles
    , CompInputFile(..)
    , CompTaskSettings(..)
    , Comp(..)
    , Task(..)
    , PilotName(..)
    , Pilot(..)
    , TrackFileFail
    , IxTask(..)
    , findCompInput
    , reshape
    , pilotNamed
    , compToTag
    , tagToPeg
    , mkCompTaskSettings
    , compFileToTaskFiles
    )
import Flight.Track.Time (TimeRow(..), TimeToTick, glideRatio, altBonusTimeToTick, copyTimeToTick)
import Flight.Track.Stop (Framing(..), StopFraming(..), TrackScoredSection(..))
import Flight.Mask (checkTracks)
import Flight.Scribe
    ( readCompAndTasks, readFraming
    , readPilotAlignTimeWriteDiscardFurther
    , readPilotAlignTimeWritePegThenDiscard
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
go CmdBatchOptions{..} compFile = do
    let tagFile = compToTag compFile
    let stopFile = tagToPeg tagFile
    putStrLn $ "Reading scored times from " ++ show stopFile

    filesTaskAndSettings <-
        catchIO
            (Just <$> do
                ts <- compFileToTaskFiles compFile
                s <- readCompAndTasks (compFile, ts)
                return (ts, s))
            (const $ return Nothing)

    stopping <-
        catchIO
            (Just <$> readFraming stopFile)
            (const $ return Nothing)

    case (filesTaskAndSettings, stopping) of
        (Nothing, _) -> putStrLn "Couldn't read the comp settings."
        (_, Nothing) -> putStrLn "Couldn't read the scored frames."
        (Just (taskFiles, settings@(cs, _)), Just Framing{stopFlying}) ->
            filterTime
                (uncurry mkCompTaskSettings $ settings)
                (compFile, taskFiles)
                (IxTask <$> task)
                (pilotNamed cs $ PilotName <$> pilot)
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
    CompTaskSettings{comp = Comp{discipline = hgOrPg}, tasks}
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
        Just xs -> do
            let taskPilots :: [[Pilot]] =
                    (fmap . fmap)
                        (\case
                            Left (p, _) -> p
                            Right (p, _) -> p)
                        xs

            sequence_
                [
                    mapM_
                        (\p ->
                            readPilotAlignTimeWriteDiscardFurther
                                copyTimeToTick
                                id
                                compFile
                                (includeTask selectTasks)
                                (filterOnPilotStops p stops)
                                n
                                p)
                        pilots
                | n <- (IxTask <$> [1 .. ])
                | pilots <- taskPilots
                | stops <- stopFlying
                ]

            let altBonusesOnTime :: [TimeToTick] =
                    [
                        fromMaybe copyTimeToTick $ do
                            _ <- stopped
                            zs' <- nonEmpty zs
                            let RawZone{alt} = last zs'
                            altBonusTimeToTick (glideRatio hgOrPg) <$> alt

                    | Task{stopped, zones = Zones{raw = zs}} <- tasks
                    ]

            sequence_
                [
                    sequence
                    [ do
                        a <- readPilotAlignTimeWritePegThenDiscard
                                timeToTick
                                id
                                compFile
                                (includeTask selectTasks)
                                (filterOnPilotStops p stops)
                                n
                                p
                        return $ (p, a)

                    | p <- pilots
                    ]
                | n <- (IxTask <$> [1 .. ])
                | pilots <- taskPilots
                | stops <- stopFlying
                | timeToTick <- altBonusesOnTime
                ]

checkAll
    :: ScoringInputFiles
    -> [IxTask]
    -> [Pilot]
    -> IO [[Either (Pilot, TrackFileFail) (Pilot, ())]]
checkAll = checkTracks $ \CompTaskSettings{tasks} -> (\ _ _ _ -> ()) tasks

includeTask :: [IxTask] -> IxTask -> Bool
includeTask tasks = if null tasks then const True else (`elem` tasks)
