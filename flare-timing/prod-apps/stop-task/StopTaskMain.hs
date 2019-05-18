{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Data.Time.Clock (diffUTCTime)
import Control.Exception.Safe (catchIO)
import System.FilePath (takeFileName)

import Flight.Track.Cross (Crossing(..), Seconds(..))
import Flight.Track.Tag (Tagging(..), TrackTime(..), PilotTrackTag(..))
import Flight.Track.Stop (StopWindow(..), StopTagging(..), tardyElapsed, tardyGate)
import Flight.Comp
    ( FileType(CompInput)
    , CompInputFile(..)
    , CrossZoneFile(..)
    , TagZoneFile(..)
    , CompSettings(..)
    , TaskStop(..)
    , Task(..)
    , StartGate(..)
    , LastStart(..)
    , compToCross
    , crossToTag
    , tagToStop
    , findCompInput
    , ensureExt
    )
import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Scribe (readComp, readCrossing, readTagging, writeStopTagging)
import StopTaskOptions (description)

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
    fprint ("Stopping task completed in " % timeSpecs % "\n") start end

go :: CmdBatchOptions -> CompInputFile -> IO ()
go CmdBatchOptions{..} compFile@(CompInputFile compPath) = do
    let crossFile@(CrossZoneFile crossPath) = compToCross compFile
    let tagFile@(TagZoneFile tagPath) = crossToTag . compToCross $ compFile
    putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading flying time range from '" ++ takeFileName crossPath ++ "'"
    putStrLn $ "Reading zone tags from '" ++ takeFileName tagPath ++ "'"

    compSettings <-
        catchIO
            (Just <$> readComp compFile)
            (const $ return Nothing)

    crossing <-
        catchIO
            (Just <$> readCrossing crossFile)
            (const $ return Nothing)

    tagging <-
        catchIO
            (Just <$> readTagging tagFile)
            (const $ return Nothing)

    case (compSettings, crossing, tagging) of
        (Nothing, _, _) -> putStrLn "Couldn't read the comp settings."
        (_, Nothing, _) -> putStrLn "Couldn't read the crossings."
        (_, _, Nothing) -> putStrLn "Couldn't read the taggings."
        (Just cs, Just cg, Just tg) ->
            writeStop cs tagFile cg tg

writeStop :: CompSettings k -> TagZoneFile -> Crossing -> Tagging -> IO ()
writeStop CompSettings{tasks} tagFile _ Tagging{timing, tagging} = do

    let sws :: [Maybe StopWindow] =
            [
                do
                    TaskStop{retroactive = t1} <- stopped

                    case gs of
                        -- NOTE: An elapsed time task. Find the last crossing
                        -- of the start.
                        [] -> do
                            LastStart sN <- tardyElapsed ss zonesLast
                            let wt = if sN < t1 then Just (sN, t1) else Nothing
                            return
                                StopWindow
                                    { lastStarters = []
                                    , windowTimes = wt
                                    , windowSeconds = Seconds . round $ t1 `diffUTCTime` sN
                                    }

                        -- NOTE: A race task with a single start gate. All
                        -- pilots share the same start.
                        (StartGate t0) : [] ->
                            return
                                StopWindow
                                    { lastStarters = []
                                    , windowTimes = Just (t0, t1)
                                    , windowSeconds = Seconds . round $ t1 `diffUTCTime` t0
                                    }

                        -- NOTE: A race task with a multiple start gates. Find
                        -- the last start gate taken.
                        _ -> do
                            StartGate gN <- tardyGate gs ss zts zps
                            let wt = if gN < t1 then Just (gN, t1) else Nothing
                            return
                                StopWindow
                                    { lastStarters = []
                                    , windowTimes = wt
                                    , windowSeconds = Seconds . round $ t1 `diffUTCTime` gN
                                    }

            | Task{stopped, startGates = gs, speedSection = ss} <- tasks
            | TrackTime{zonesLast, zonesRankTime = zts, zonesRankPilot = zps} <- timing
            ]

    let times' :: [TrackTime] =
            [ ts
            | ts <- timing
            ]

    let tags :: [[PilotTrackTag]] =
            [ tgs
            | tgs <- tagging
            ]

    let stopTask = StopTagging{timing = times', tagging = tags, stopWindow = sws}
    writeStopTagging (tagToStop tagFile) stopTask
