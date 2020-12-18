{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import Prelude hiding (head, last)
import Data.Maybe (catMaybes)
import Data.List.NonEmpty (nonEmpty, head, last)
import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Data.Function ((&))
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Data.Time.Clock (UTCTime, diffUTCTime)
import Control.Monad (join)
import Control.Exception.Safe (catchIO)
import System.Directory (getCurrentDirectory)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Flight.Clip (FlyingSection)
import Flight.Track.Cross
    ( Crossing(..), Seconds(..), TrackFlyingSection(..)
    , ZoneTag(..), InterpolatedFix(..)
    )
import Flight.Track.Tag
    ( Tagging(..), TrackTime(..), PilotTrackTag(..), TrackTag(..), timed, lastStarting, starting, tagTimes)
import qualified Flight.Track.Stop as Stop (TrackScoredSection(..))
import Flight.Track.Time (FixIdx(..), TrackRow(..))
import Flight.Track.Stop
    ( StopWindow(..), StopFraming(..), Framing(..)
    , TrackScoredSection(..), TrackRacingGateSection(..), TrackRacingStartSection(..)
    , tardyElapsed, tardyGate, stopClipByDuration, stopClipByGate, endOfScored
    )
import Flight.Comp
    ( FindDirFile(..)
    , FileType(CompInput)
    , CompInputFile(..)
    , TagZoneFile(..)
    , CompSettings(..)
    , TaskStop(..)
    , Task(..)
    , StartGate(..)
    , LastStart(..)
    , Pilot
    , compToCross
    , crossToTag
    , tagToPeg
    , findCompInput
    , reshape
    )
import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Scribe
    (readComp, readCrossing, readTagging, writeFraming, readCompTrackRows)
import PegFrameOptions (description)

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
    fprint ("Pegging the scoring frame completed in " % timeSpecs % "\n") start end

go :: CmdBatchOptions -> CompInputFile -> IO ()
go CmdBatchOptions{..} compFile = do
    let crossFile = compToCross compFile
    let tagFile = crossToTag . compToCross $ compFile
    putStrLn $ "Reading competition from " ++ show compFile
    putStrLn $ "Reading flying time range from " ++ show crossFile
    putStrLn $ "Reading zone tags from " ++ show tagFile

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
        (Just cs, Just cg, Just tg) -> writeStop cs compFile tagFile cg tg

writeStop
    :: CompSettings k
    -> CompInputFile
    -> TagZoneFile
    -> Crossing
    -> Tagging
    -> IO ()
writeStop
    CompSettings{tasks}
    compFile
    tagFile
    Crossing{flying}
    Tagging{timing, tagging} = do

    let sws :: [Maybe StopWindow] =
            [
                do
                    TaskStop{retroactive = t1} <- stopped
                    let (tN, psN) = lastStarting ss tt

                    case gs of
                        -- NOTE: An elapsed time task. Find the last crossing of
                        -- the start.
                        [] -> do
                            LastStart sN <- tardyElapsed ss zonesLast
                            return
                                StopWindow
                                    { lastStartTime = tN
                                    , lastStarters = psN
                                    , stopWindowTimes = if sN < t1 then Just (sN, t1) else Nothing
                                    , stopWindowSeconds = Seconds . round $ t1 `diffUTCTime` sN
                                    }

                        -- NOTE: A race task with a single start gate. All
                        -- pilots share the same start.
                        (StartGate t0) : [] ->
                            return
                                StopWindow
                                    { lastStartTime = tN
                                    , lastStarters = psN
                                    , stopWindowTimes = Just (t0, t1)
                                    , stopWindowSeconds = Seconds . round $ t1 `diffUTCTime` t0
                                    }

                        -- NOTE: A race task with a multiple start gates. Find
                        -- the last start gate taken.
                        _ -> do
                            StartGate gN <- tardyGate gs ss zts zps
                            return
                                StopWindow
                                    { lastStartTime = tN
                                    , lastStarters = psN
                                    , stopWindowTimes = if gN < t1 then Just (gN, t1) else Nothing
                                    , stopWindowSeconds = Seconds . round $ t1 `diffUTCTime` gN
                                    }

            | Task{stopped, startGates = gs, speedSection = ss} <- tasks
            | tt@TrackTime{zonesLast, zonesRankTime = zts, zonesRankPilot = zps} <- timing
            ]

    let ps = (fmap . fmap) fst flying
    trackss' :: Maybe [[Maybe (Pilot, [TrackRow])]] <-
        catchIO
            (Just <$> readCompTrackRows compFile (const True) ps)
            (const $ return Nothing)

    let trackss :: [Map Pilot [TrackRow]] =
            maybe (repeat $ Map.empty) (fmap (Map.fromList . catMaybes)) trackss'

    let sfss :: [[(Pilot, (Maybe TrackScoredSection, Maybe TrackRacingGateSection))]] =
            [
                [ (p,) $ sw & \case
                    Nothing -> (, Nothing) $ do
                        TrackFlyingSection{flyingFixes, flyingTimes, flyingSeconds} <- tfs

                        return
                            TrackScoredSection
                                { scoredFixes = flyingFixes
                                , scoredSeconds = flyingSeconds
                                , scoredTimes = flyingTimes
                                , scoredWindowSeconds = do
                                    (t0, t1) <- flyingTimes
                                    return . Seconds . round $ t1 `diffUTCTime` t0
                                }

                    _ -> maybe (Nothing, Nothing) (\(a, b) -> (Just a, b)) $ do
                        StopWindow{stopWindowSeconds = clipSecs} <- sw
                        TrackFlyingSection{flyingTimes = ts, flyingSeconds} <- tfs
                        case gs of
                            [] -> do
                                (t0, t1) <- stopClipByDuration clipSecs ts
                                let delta = round $ t1 `diffUTCTime` t0
                                let st = Just (t0, t1)
                                let track = Map.lookup p tracks
                                let si = join $ scoredIndices st <$> track

                                return
                                    ( TrackScoredSection
                                        { scoredFixes = si
                                        , scoredSeconds = do
                                            (Seconds w0, _) <- flyingSeconds
                                            return (Seconds w0, Seconds $ w0 + delta)
                                        , scoredTimes = st
                                        , scoredWindowSeconds = Just $ Seconds delta
                                        }
                                    , Nothing
                                    )

                            _ -> do
                                let (sg, tsClipped) = stopClipByGate clipSecs gs ts
                                StartGate tG <- sg
                                (t0, t1) <- tsClipped
                                let deltaFlying = t1 `diffUTCTime` t0
                                let rFlying = round deltaFlying

                                let track = Map.lookup p tracks

                                let st = Just (t0, t1)
                                let si = join $ scoredIndices st <$> track

                                let rgt = Just (tG, t1)
                                let rgi = join $ scoredIndices rgt <$> track

                                let sfSecs =
                                        do
                                            (Seconds w0, _) <- flyingSeconds
                                            return (Seconds rFlying, (Seconds w0, Seconds $ w0 + rFlying))

                                let rgSecs =
                                        do
                                            (Seconds w0, _) <- flyingSeconds
                                            let delta0 = tG `diffUTCTime` t0
                                            let delta1 = t1 `diffUTCTime` tG
                                            let r0 = round delta0
                                            let r1 = round delta1
                                            return (Seconds r1, (Seconds $ w0 + r0, Seconds $ w0 + r0 + r1))

                                return
                                    ( TrackScoredSection
                                        { scoredFixes = si
                                        , scoredSeconds = snd <$> sfSecs
                                        , scoredTimes = st
                                        , scoredWindowSeconds = fst <$> sfSecs
                                        }
                                    , Just
                                        TrackRacingGateSection
                                            { racingGateFixes = rgi
                                            , racingGateSeconds = snd <$> rgSecs
                                            , racingGateTimes = rgt
                                            , racingGateWindowSeconds = fst <$> rgSecs
                                            }
                                    )

                | (p, tfs) <- pfs
                ]

            | Task{startGates = gs} <- tasks
            | pfs <- flying
            | sw <- sws
            | tracks <- trackss
            ]

    let tagss :: [[PilotTrackTag]] =
            [
                if sw == Nothing then pts else
                [
                    PilotTrackTag p $ do
                        TrackTag{zonesTag = zs} <- tt
                        sf' <- sf
                        let zs' = clipByTime (Stop.scoredTimes sf') <$> zs
                        return $ TrackTag{zonesTag = zs'}

                | PilotTrackTag p tt <- pts
                | (_, (sf, _)) <- sfs
                ]

            | sw <- sws
            | pts <- tagging
            | sfs <- sfss
            ]

    let timess :: [TrackTime] =
            [ timed ts fs
            | ts <- tagss
            | fs <- fmap (endOfScored . fst . snd) <$> sfss
            ]

    let sfss' =
            [
                [ (p,) $
                    StopFraming
                        { stopScored = sfStop
                        , stopRacingGate = sfGate
                        , stopRacingStart = do
                            TrackFlyingSection{flyingSeconds} <- tfs
                            TrackScoredSection{scoredTimes} <- sfStop
                            TrackRacingGateSection{racingGateTimes} <- sfGate
                            (t0, _) <- scoredTimes
                            (_, t1) <- racingGateTimes
                            tags' <- tagTimes ptt
                            tS <- starting ss tags'

                            let track = Map.lookup p tracks

                            let rst = Just (tS, t1)
                            let rsi = join $ scoredIndices rst <$> track

                            let rsSecs =
                                    do
                                        (Seconds w0, _) <- flyingSeconds
                                        let delta0 = tS `diffUTCTime` t0
                                        let delta1 = t1 `diffUTCTime` tS
                                        let r0 = round delta0
                                        let r1 = round delta1
                                        return (Seconds r1, (Seconds $ w0 + r0, Seconds $ w0 + r0 + r1))

                            return
                                TrackRacingStartSection
                                    { racingStartFixes = rsi
                                    , racingStartSeconds = snd <$> rsSecs
                                    , racingStartTimes = rst
                                    , racingStartWindowSeconds = fst <$> rsSecs
                                    }
                        }

                | (_, tfs) <- pfs
                | (p, (sfStop, sfGate)) <- sfs
                | ptt@PilotTrackTag{} <- tags
                ]

            | Task{speedSection = ss} <- tasks
            | pfs <- flying
            | sfs <- sfss
            | tags <- tagss
            | tracks <- trackss
            ]

    let frame =
            Framing
                { stopWindow = sws
                , stopFlying = sfss'
                , timing = timess
                , tagging = tagss
                }

    let pegFile = tagToPeg tagFile
    putStrLn $ "Writing framing to " ++ show pegFile
    writeFraming pegFile frame

clipByTime :: FlyingSection UTCTime -> Maybe ZoneTag -> Maybe ZoneTag
clipByTime Nothing x = x
clipByTime _ Nothing = Nothing
clipByTime (Just (_, t1)) zt@(Just ZoneTag{inter = InterpolatedFix{time = t}}) =
    if t > t1 then Nothing else zt

scoredIndices :: FlyingSection UTCTime -> [TrackRow] -> FlyingSection Int
scoredIndices section xs = do
    (t0, t1) <- section
    xsGE <- nonEmpty $ filter (\TrackRow{time = t} -> t >= t0) xs
    xsLT <- nonEmpty $ filter (\TrackRow{time = t} -> t < t1) xs
    let FixIdx f0 = fixIdx $ head xsGE
    let FixIdx f1 = fixIdx $ last xsLT
    return (max 0 $ f0 - 1, f1)
