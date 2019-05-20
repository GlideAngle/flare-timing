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
import System.FilePath (takeFileName)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Flight.Clip (FlyingSection)
import Flight.Track.Cross
    ( Crossing(..), Seconds(..), TrackFlyingSection(..)
    , ZoneTag(..), InterpolatedFix(..)
    )
import Flight.Track.Tag
    ( Tagging(..), TrackTime(..), PilotTrackTag(..), TrackTag(..), timed)
import qualified Flight.Track.Stop as Stop (TrackScoredSection(..))
import Flight.Track.Time (FixIdx(..), TrackRow(..))
import Flight.Track.Stop
    ( StopWindow(..), Framing(..), TrackScoredSection(..)
    , tardyElapsed, tardyGate, stopClipByDuration, stopClipByGate, endOfScored
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
    , tagToPeg
    , findCompInput
    , ensureExt
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

    let ps = (fmap . fmap) fst flying
    trackss' :: Maybe [[Maybe (Pilot, [TrackRow])]] <-
        catchIO
            (Just <$> readCompTrackRows compFile (const True) ps)
            (const $ return Nothing)

    let trackss :: [Map Pilot [TrackRow]] =
            maybe (repeat $ Map.empty) (fmap (Map.fromList . catMaybes)) trackss'

    let sfss :: [[(Pilot, Maybe TrackScoredSection)]] =
            [
                [ (p,) $ sw & \case
                    Nothing -> do
                        TrackFlyingSection{flyingFixes, flyingTimes, flyingSeconds} <- tfs

                        return
                            TrackScoredSection
                                { scoredFixes = flyingFixes
                                , scoredTimes = flyingTimes
                                , scoredSeconds = flyingSeconds
                                }

                    _ -> do
                        StopWindow{windowSeconds = clipSecs} <- sw
                        TrackFlyingSection{flyingTimes = ts, flyingSeconds} <- tfs
                        case gs of
                            [] -> do
                                (t0, t1) <- stopClipByDuration clipSecs ts
                                let delta = t1 `diffUTCTime` t0
                                let st = Just (t0, t1)
                                let track = Map.lookup p tracks
                                let si = join $ scoredIndices st <$> track

                                return
                                    TrackScoredSection
                                        { scoredFixes = si
                                        , scoredTimes = st
                                        , scoredSeconds = do
                                            (Seconds w0, _) <- flyingSeconds
                                            return (Seconds w0, Seconds $ w0 + round delta)
                                        }

                            _ -> do
                                (t0, t1) <- stopClipByGate clipSecs gs ts
                                let delta = t1 `diffUTCTime` t0
                                let st = Just (t0, t1)
                                let track = Map.lookup p tracks
                                let si = join $ scoredIndices st <$> track

                                return
                                    TrackScoredSection
                                        { scoredFixes = si
                                        , scoredTimes = st
                                        , scoredSeconds = do
                                            (Seconds w0, _) <- flyingSeconds
                                            return (Seconds w0, Seconds $ w0 + round delta)
                                        }

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
                | (_, sf) <- sfs
                ]

            | sw <- sws
            | pts <- tagging
            | sfs <- sfss
            ]

    let timess :: [TrackTime] =
            [ timed ts fs
            | ts <- tagss
            | fs <- fmap (endOfScored . snd) <$> sfss
            ]

    let frame =
            Framing
                { stopWindow = sws
                , stopFlying = sfss
                , timing = timess
                , tagging = tagss
                }

    writeFraming (tagToPeg tagFile) frame

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
