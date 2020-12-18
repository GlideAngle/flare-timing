{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

import Debug.Trace (traceShowId)
import Text.Printf (printf)
import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Lens ((^?), element)
import Control.Monad (mapM_)
import Control.DeepSeq
import Control.Concurrent.ParallelIO (parallel_)
import Control.Exception.Safe (catchIO)
import Control.Monad.Except (runExceptT)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>))

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Track.Time
    ( FixIdx(..), ZoneIdx(..), TrackRow(..)
    , commentOnFixRange
    )
import Flight.Comp
    ( FindDirFile(..)
    , FileType(CompInput)
    , UnpackTrackDir(..)
    , CompInputFile(..)
    , UnpackTrackFile(..)
    , CompSettings(..)
    , PilotName(..)
    , Pilot(..)
    , IxTask(..)
    , compFileToCompDir
    , unpackTrackPath
    , findCompInput
    , reshape
    , pilotNamed
    )
import Flight.Mask (FnIxTask, settingsLogs, fixFromFix)
import Flight.Track.Cross (Fix(..))
import Flight.Kml (MarkedFixes(..))
import Flight.Scribe (readComp, writeUnpackTrack)
import UnpackTrackOptions (description)
import Flight.TrackLog (pilotTrack)

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
    if null files then putStrLn "Couldn't find input files."
                  else mapM_ (go o) files
    end <- getTime Monotonic
    fprint ("Unpacking tracks completed in " % timeSpecs % "\n") start end

go :: CmdBatchOptions -> CompInputFile -> IO ()
go CmdBatchOptions{..} compFile = do
    putStrLn $ "Reading competition from " ++ show compFile

    compSettings <-
        catchIO
            (Just <$> readComp compFile)
            (const $ return Nothing)

    case compSettings of
        Nothing -> putStrLn "Couldn't read the comp settings."
        Just cs@CompSettings{tasks} -> do
            let ixSelectTasks = IxTask <$> task
            let ps = pilotNamed cs $ PilotName <$> pilot
            (_, selectedCompLogs) <- settingsLogs compFile ixSelectTasks ps

            parallel_ . concat $
                [
                    [ do
                        check <-
                            catchIO
                                (Just <$> (runExceptT $ pilotTrack (dump tasks ixTask) (traceShowId pilotLog)))
                                (const $ return Nothing)

                        case check of
                            Nothing ->
                                putStrLn
                                $ printf
                                    "Unable to read task %d, %s." n (show pilotLog)

                            Just pt -> do
                                let ptWrite = writePilotTimes compFile n

                                either
                                    (\(p, _) -> ptWrite (p, []))
                                    (\(p, g) -> ptWrite (p, force $ g p))
                                    pt

                    | pilotLog <- taskLogs
                    ]

                | ixTask@(IxTask n) <- IxTask <$> [1..]
                | taskLogs <- selectedCompLogs
                ]

writePilotTimes :: CompInputFile -> Int -> (Pilot, [TrackRow]) -> IO ()
writePilotTimes compFile iTask (pilot, rows) = do
    putStrLn $ printf "Task %d %s" iTask (commentOnFixRange pilot $ fixIdx <$> rows)
    _ <- createDirectoryIfMissing True dOut
    _ <- writeUnpackTrack (UnpackTrackFile $ dOut </> f) rows
    return ()
    where
        dir = compFileToCompDir compFile
        (UnpackTrackDir dOut, UnpackTrackFile f) = unpackTrackPath dir iTask pilot

mkTrackRow :: Fix -> TrackRow
mkTrackRow Fix{fix, time, lat, lng, alt} =
    TrackRow
        { fixIdx = FixIdx fix
        , time = time
        , lat = lat
        , lng = lng
        , alt = alt
        }

dump :: FnIxTask k (Pilot -> [TrackRow])
dump tasks (IxTask i) MarkedFixes{mark0, fixes = xs} _ =
    case tasks ^? element (i - 1) of
        Nothing -> []
        Just _ ->
            mkTrackRow <$> ys
            where
                ys =
                    [ fixFromFix mark0 ix x
                    | x <- xs
                    | ix <- ZoneIdx <$> [1..]
                    ]
