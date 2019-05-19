﻿{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Data.Function ((&))
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Data.Time.Clock (UTCTime, diffUTCTime)
import Control.Exception.Safe (catchIO)
import System.FilePath (takeFileName)

import Flight.Clip (FlyingSection)
import Flight.Track.Cross
    ( Crossing(..), Seconds(..), TrackFlyingSection(..)
    , ZoneTag(..), InterpolatedFix(..)
    )
import Flight.Track.Tag
    ( Tagging(..), TrackTime(..), PilotTrackTag(..), TrackTag(..))
import qualified Flight.Track.Stop as Stop (TrackScoredSection(..))
import Flight.Track.Stop
    ( StopWindow(..), FreezeFrame(..), TrackScoredSection(..)
    , tardyElapsed, tardyGate, stopClipByDuration, stopClipByGate
    )
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
    , Pilot
    , compToCross
    , crossToTag
    , tagToStop
    , findCompInput
    , ensureExt
    )
import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Scribe (readComp, readCrossing, readTagging, writeFreezeFrame)
import StopCrossOptions (description)

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
writeStop CompSettings{tasks} tagFile Crossing{flying} Tagging{timing, tagging} = do

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

    let sfss :: [[(Pilot, Maybe TrackScoredSection)]] =
            [
                [ (p,) $ sw & \case
                    Nothing -> do
                        TrackFlyingSection{flyingTimes, flyingSeconds} <- tfs

                        return
                            TrackScoredSection
                                { scoredTimes = flyingTimes
                                , scoredSeconds = flyingSeconds
                                }

                    _ -> do
                        StopWindow{windowSeconds = clipSecs} <- sw
                        TrackFlyingSection{flyingTimes = ts, flyingSeconds} <- tfs
                        case gs of
                            [] -> do
                                (t0, t1) <- stopClipByDuration clipSecs ts
                                let delta = t1 `diffUTCTime` t0

                                return
                                    TrackScoredSection
                                        { scoredTimes = Just (t0, t1)
                                        , scoredSeconds = do
                                            (Seconds w0, _) <- flyingSeconds
                                            return (Seconds w0, Seconds $ w0 + round delta)
                                        }

                            _ -> do
                                (t0, t1) <- stopClipByGate clipSecs gs ts
                                let delta = t1 `diffUTCTime` t0

                                return
                                    TrackScoredSection
                                        { scoredTimes = Just (t0, t1)
                                        , scoredSeconds = do
                                            (Seconds w0, _) <- flyingSeconds
                                            return (Seconds w0, Seconds $ w0 + round delta)
                                        }

                | (p, tfs) <- pfs
                ]

            | Task{startGates = gs} <- tasks
            | pfs <- flying
            | sw <- sws
            ]

    let times' :: [TrackTime] =
            [ ts
            | ts <- timing
            ]

    let tags :: [[PilotTrackTag]] =
            [
                if sw == Nothing then pts else
                [
                    PilotTrackTag p $ do
                        TrackTag{zonesTag = zs} <- tt
                        sf' <- sf
                        let zs' = clipByTime (Stop.scoredTimes sf') <$> zs
                        return $ TrackTag{zonesTag = zs'}

                | PilotTrackTag p tt <- pts
                | (_, sf) <- sfs
                ]

            | sw <- sws
            | pts <- tagging
            | sfs <- sfss
            ]

    let frame =
            FreezeFrame
                { stopWindow = sws
                , stopFlying = sfss
                , timing = times'
                , tagging = tags
                }

    writeFreezeFrame (tagToStop tagFile) frame

clipByTime :: FlyingSection UTCTime -> Maybe ZoneTag -> Maybe ZoneTag
clipByTime Nothing x = x
clipByTime _ Nothing = Nothing
clipByTime (Just (_, t1)) zt@(Just ZoneTag{inter = InterpolatedFix{time = t}}) =
    if t > t1 then Nothing else zt
