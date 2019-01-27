{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Lens ((^?), element)
import Control.Monad (mapM_, when, zipWithM_)
import Control.Exception.Safe (catchIO)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Track.Time
    (FixIdx(..), ZoneIdx(..), TrackRow(..), unpackTrackHeaders)
import Flight.Comp
    ( FileType(CompInput)
    , UnpackTrackDir(..)
    , CompInputFile(..)
    , UnpackTrackFile(..)
    , CompSettings(..)
    , PilotName(..)
    , Pilot(..)
    , IxTask(..)
    , TrackFileFail
    , compFileToCompDir
    , unpackTrackPath
    , findCompInput
    , ensureExt
    , pilotNamed
    )
import Flight.Mask (FnIxTask, checkTracks, fixFromFix)
import Flight.Track.Cross (Fix(..))
import Flight.Kml (MarkedFixes(..))
import Flight.Scribe (readComp, writeUnpackTrack)
import UnpackTrackOptions (description)

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
    if null files then putStrLn "Couldn't find input files."
                  else mapM_ (go o) files
    end <- getTime Monotonic
    fprint ("Unpacking tracks completed in " % timeSpecs % "\n") start end

go :: CmdBatchOptions -> CompInputFile -> IO ()
go CmdBatchOptions{..} compFile@(CompInputFile compPath) = do
    putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"

    compSettings <-
        catchIO
            (Just <$> readComp compFile)
            (const $ return Nothing)

    case compSettings of
        Nothing -> putStrLn "Couldn't read the comp settings."
        Just cs ->
            writeTime
                (IxTask <$> task)
                (pilotNamed cs $ PilotName <$> pilot)
                (CompInputFile compPath)
                checkAll 

writeTime
    :: [IxTask]
    -> [Pilot]
    -> CompInputFile
    -> (CompInputFile
      -> [IxTask]
      -> [Pilot]
      -> IO [[Either (Pilot, t) (Pilot, Pilot -> [TrackRow])]])
    -> IO ()
writeTime selectTasks selectPilots compFile f = do
    checks <-
        catchIO
            (Just <$> f compFile selectTasks selectPilots)
            (const $ return Nothing)

    case checks of
        Nothing -> putStrLn "Unable to read tracks for pilots."
        Just xs -> do
            let ys :: [[(Pilot, [TrackRow])]] =
                    (fmap . fmap)
                        (\case
                            Left (p, _) -> (p, [])
                            Right (p, g) -> (p, g p))
                        xs

            zipWithM_
                (\ n zs ->
                    when (includeTask selectTasks $ IxTask n) $
                        mapM_ (writePilotTimes compFile n) zs)
                [1 .. ]
                ys

checkAll
    :: CompInputFile
    -> [IxTask]
    -> [Pilot]
    -> IO
         [
             [Either
                 (Pilot, TrackFileFail)
                 (Pilot, Pilot -> [TrackRow])
             ]
         ]
checkAll =
    checkTracks (\CompSettings{tasks} -> dump tasks)

includeTask :: [IxTask] -> IxTask -> Bool
includeTask tasks = if null tasks then const True else (`elem` tasks)

writePilotTimes :: CompInputFile -> Int -> (Pilot, [TrackRow]) -> IO ()
writePilotTimes compFile iTask (pilot, rows) = do
    _ <- createDirectoryIfMissing True dOut
    _ <- writeUnpackTrack (UnpackTrackFile $ dOut </> f) unpackTrackHeaders rows
    return ()
    where
        dir = compFileToCompDir compFile
        (UnpackTrackDir dOut, UnpackTrackFile f) = unpackTrackPath dir iTask pilot

mkTrackRow :: Fix -> TrackRow
mkTrackRow Fix{fix, time, lat, lng} =
    TrackRow
        { fixIdx = FixIdx fix
        , time = time
        , lat = lat
        , lng = lng
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