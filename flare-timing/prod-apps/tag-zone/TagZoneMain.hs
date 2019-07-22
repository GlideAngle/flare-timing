{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_)
import Control.Exception.Safe (catchIO)
import System.FilePath (takeFileName)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..), Extension(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)

import Flight.Mask
    ( TaskZone, Sliver(..), TagInterpolate(..)
    , tagZones, zonesToTaskZones
    )
import Flight.Comp
    ( FileType(CompInput)
    , CompSettings(..)
    , CompInputFile(..)
    , CrossZoneFile(..)
    , Comp(..)
    , Task(..)
    , compToCross
    , crossToTag
    , findCompInput
    , ensureExt
    )
import Flight.Track.Cross
    (Crossing(..), TrackCross(..), PilotTrackCross(..), endOfFlying)
import Flight.Track.Tag
    ( Tagging(..), TrackTag(..), PilotTrackTag(..)
    , timed
    )
import Flight.Scribe (readComp, readCrossing, writeTagging)
import TagZoneOptions (description)
import qualified Flight.Span.Double as Dbl (sliver)
import qualified Flight.Span.Rational as Rat (sliver)
import Flight.Span.Math (Math(..))

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions
                            (ProgramName name)
                            description
                            (Just $ Extension "*.comp-input.yaml")

    let lf = LenientFile {coerceFile = ensureExt CompInput}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdBatchOptions -> IO ()
drive o@CmdBatchOptions{math} = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    files <- findCompInput o
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ (go math) files
    end <- getTime Monotonic
    fprint ("Tagging zones completed in " % timeSpecs % "\n") start end

go :: Math -> CompInputFile -> IO ()
go math compFile@(CompInputFile compPath) = do
    let crossFile@(CrossZoneFile crossPath) = compToCross compFile
    putStrLn $ "Reading tasks from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading zone crossings from '" ++ takeFileName crossPath ++ "'"

    cs <-
        catchIO
            (Just <$> readComp compFile)
            (const $ return Nothing)

    cgs <-
        catchIO
            (Just <$> readCrossing crossFile)
            (const $ return Nothing)

    case (math, cs, cgs) of
        (_, Nothing, _) ->
            putStrLn "Couldn't read the comp settings."

        (_, _, Nothing) ->
            putStrLn "Couldn't read the crossings."

        (Floating, Just CompSettings{tasks, comp = Comp{earthMath}}, Just Crossing{crossing, flying}) -> do
            let sliver@Sliver{az} = Dbl.sliver earthMath
            let pss :: [[PilotTrackTag]] =
                    [
                        (\case
                            PilotTrackCross p Nothing ->
                                PilotTrackTag p Nothing

                            PilotTrackCross p (Just xs) ->
                                PilotTrackTag p (Just $ flownTag sliver zs xs))
                        <$> cg

                    | Task{zones} <- tasks
                    , let zs = zonesToTaskZones az zones
                    | cg <- crossing
                    ]

            let times =
                    [ timed ps fs
                    | ps <- pss
                    | fs <- fmap (endOfFlying . snd) <$> flying
                    ]

            let tagZone = Tagging{timing = times, tagging = pss}

            writeTagging (crossToTag crossFile) tagZone

        (Rational, Just CompSettings{tasks, comp = Comp{earthMath}}, Just Crossing{crossing, flying}) -> do
            let sliver@Sliver{az} = Rat.sliver earthMath
            let pss :: [[PilotTrackTag]] =
                    [
                        (\case
                            PilotTrackCross p Nothing ->
                                PilotTrackTag p Nothing

                            PilotTrackCross p (Just xs) ->
                                PilotTrackTag p (Just $ flownTag sliver zs xs))
                        <$> cg

                    | Task{zones} <- tasks
                    , let zs = zonesToTaskZones az zones
                    | cg <- crossing
                    ]

            let times =
                    [ timed ps fs
                    | ps <- pss
                    | fs <- fmap (endOfFlying . snd) <$> flying
                    ]

            let tagZone = Tagging{timing = times, tagging = pss}

            writeTagging (crossToTag crossFile) tagZone

flownTag
    :: (Real b, Fractional b, TagInterpolate a b)
    => a
    -> [TaskZone b]
    -> TrackCross
    -> TrackTag
flownTag tagInterp zs TrackCross{zonesCrossSelected} =
    TrackTag
        { zonesTag = tagZones tagInterp zs zonesCrossSelected
        }
    where
